use crate::{api_other, sym_yaml_keys, sym_yaml_tag, Fallible, R_STRING_MAX_BYTES};
use extendr_api::prelude::*;
use saphyr::{Mapping, Scalar, Tag, Yaml, YamlEmitter};
use std::{borrow::Cow, fs};

pub(crate) fn yaml_body(yaml: &str, multi: bool) -> &str {
    if multi || !yaml.starts_with("---\n") {
        yaml
    } else {
        &yaml[4..]
    }
}

fn emit_yaml_documents(docs: &[Yaml<'static>], multi: bool) -> Fallible<String> {
    if docs.is_empty() {
        return Ok(String::new());
    }
    let mut output = String::new();
    let mut emitter = YamlEmitter::new(&mut output);
    emitter.multiline_strings(true);
    if multi {
        emitter
            .dump_docs(docs)
            .map_err(|err| api_other(err.to_string()))?;
    } else {
        emitter
            .dump(&docs[0])
            .map_err(|err| api_other(err.to_string()))?;
    }
    Ok(output)
}

fn robj_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let node = match robj.rtype() {
        Rtype::Null => Ok(Yaml::Value(Scalar::Null)),
        Rtype::Logicals => logical_to_yaml(robj),
        Rtype::Integers => integer_to_yaml(robj),
        Rtype::Doubles => real_to_yaml(robj),
        Rtype::Strings => character_to_yaml(robj),
        Rtype::List => list_to_yaml(robj),
        _ => Err(api_other(format!(
            "Unsupported R type {rtype:?} for YAML conversion",
            rtype = robj.rtype()
        ))),
    }?;
    apply_tag_if_present(robj, node)
}

fn logical_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_logical_slice()
        .ok_or_else(|| Error::Other("Expected a logical vector".to_string()))?;
    if let [value] = slice {
        return Ok(if value.is_na() {
            Yaml::Value(Scalar::Null)
        } else {
            Yaml::Value(Scalar::Boolean(value.to_bool()))
        });
    }
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if value.is_na() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::Boolean(value.to_bool())));
        }
    }
    Ok(Yaml::Sequence(values))
}

fn integer_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_integer_slice()
        .ok_or_else(|| Error::Other("Expected an integer vector".to_string()))?;
    if let [value] = slice {
        return Ok(if *value == i32::MIN {
            Yaml::Value(Scalar::Null)
        } else {
            Yaml::Value(Scalar::Integer(*value as i64))
        });
    }
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if *value == i32::MIN {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::Integer(*value as i64)));
        }
    }
    Ok(Yaml::Sequence(values))
}

fn real_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_real_slice()
        .ok_or_else(|| Error::Other("Expected a numeric vector".to_string()))?;
    if let [value] = slice {
        return Ok(if value.is_nan() {
            Yaml::Value(Scalar::Null)
        } else {
            Yaml::Value(Scalar::FloatingPoint((*value).into()))
        });
    }
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if value.is_nan() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::FloatingPoint((*value).into())));
        }
    }
    Ok(Yaml::Sequence(values))
}

fn character_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let mut strings = robj
        .as_str_iter()
        .ok_or_else(|| Error::Other("Expected a character vector".to_string()))?;
    if robj.len() == 1 {
        let value = strings
            .next()
            .expect("character vector length of 1 should yield 1 element");
        return Ok(if value.is_na() {
            Yaml::Value(Scalar::Null)
        } else {
            Yaml::Value(Scalar::String(Cow::Borrowed(value)))
        });
    }
    let mut values = Vec::with_capacity(robj.len());
    for value in strings {
        if value.is_na() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::String(Cow::Borrowed(value))));
        }
    }
    Ok(Yaml::Sequence(values))
}

fn list_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let list = robj
        .as_list()
        .ok_or_else(|| Error::Other("Expected a list".to_string()))?;
    if let Some(keys_attr) = robj.get_attrib(sym_yaml_keys()) {
        if !keys_attr.is_null() {
            let keys: List = keys_attr
                .try_into()
                .map_err(|_| Error::Other("`yaml_keys` attribute must be a list".to_string()))?;
            if keys.len() != list.len() {
                return Err(api_other(
                    "`yaml_keys` attribute must have the same length as the list",
                ));
            }
            let mut mapping = Mapping::with_capacity(list.len());
            for ((_, value), (_, key)) in list.iter().zip(keys.iter()) {
                mapping.insert(robj_to_yaml(&key)?, robj_to_yaml(&value)?);
            }
            return Ok(Yaml::Mapping(mapping));
        }
    }

    match robj.names() {
        Some(names) => {
            let mut mapping = Mapping::with_capacity(list.len());
            for (value, name) in list.as_slice().iter().zip(names) {
                let key = if name.is_na() {
                    Yaml::Value(Scalar::Null)
                } else {
                    Yaml::Value(Scalar::String(name.into()))
                };
                mapping.insert(key, robj_to_yaml(value)?);
            }
            Ok(Yaml::Mapping(mapping))
        }
        None => {
            let seq = list
                .as_slice()
                .iter()
                .map(robj_to_yaml)
                .collect::<Fallible<Vec<_>>>()?;
            Ok(Yaml::Sequence(seq))
        }
    }
}

fn apply_tag_if_present(robj: &Robj, node: Yaml<'static>) -> Fallible<Yaml<'static>> {
    if let Some(tag) = extract_yaml_tag(robj)? {
        Ok(Yaml::Tagged(Cow::Owned(tag), Box::new(node)))
    } else {
        Ok(node)
    }
}

fn extract_yaml_tag(robj: &Robj) -> Fallible<Option<Tag>> {
    let attr = match robj.get_attrib(sym_yaml_tag()) {
        Some(value) => value,
        None => return Ok(None),
    };
    let tag_str: &str = (&attr).try_into().map_err(|err: Error| {
        Error::Other(format!(
            "Invalid `yaml_tag` attribute: expected a single, non-missing string ({err})"
        ))
    })?;
    let tag_str = tag_str.trim();
    if tag_str.is_empty() || is_core_schema_tag(tag_str) {
        return Ok(None);
    }
    parse_tag_string(tag_str).map(Some)
}

fn is_core_schema_tag(tag: &str) -> bool {
    let tag = tag.trim();
    tag.starts_with("!!")
        || tag.starts_with("!<tag:yaml.org,2002:")
        || tag.starts_with("!tag:yaml.org,2002:")
        || tag.starts_with("<tag:yaml.org,2002:")
        || tag.starts_with("tag:yaml.org,2002:")
}

fn parse_tag_string(tag: &str) -> Fallible<Tag> {
    if tag.is_empty() {
        return Err(api_other(
            "`yaml_tag` attribute must not be the empty string",
        ));
    }
    if let Some(pos) = tag.rfind('!') {
        if pos + 1 >= tag.len() {
            return Err(api_other(format!("Invalid YAML tag `{tag}`")));
        }
        let handle = &tag[..pos];
        let suffix = &tag[pos + 1..];
        if handle.is_empty() {
            Ok(Tag {
                handle: "!".to_string(),
                suffix: suffix.to_string(),
            })
        } else {
            Ok(Tag {
                handle: handle.to_string(),
                suffix: suffix.to_string(),
            })
        }
    } else {
        Err(api_other(format!("Invalid YAML tag `{tag}`")))
    }
}

pub(crate) fn format_yaml_impl(value: &Robj, multi: bool) -> Fallible<String> {
    if multi {
        let list = value.as_list().ok_or_else(|| {
            Error::Other("`value` must be a list when `multi = TRUE`".to_string())
        })?;
        let mut docs = Vec::with_capacity(list.len());
        for doc in list.values() {
            docs.push(robj_to_yaml(&doc)?);
        }
        emit_yaml_documents(&docs, true)
    } else {
        robj_to_yaml(value).and_then(|yaml| emit_yaml_documents(&[yaml], false))
    }
}

pub(crate) fn write_yaml_impl(value: &Robj, path: &str, multi: bool) -> Fallible<()> {
    let yaml = format_yaml_impl(value, multi)?;
    let body = yaml_body(&yaml, multi);
    if body.len() > R_STRING_MAX_BYTES {
        return Err(api_other(
            "Formatted YAML exceeds R's 2^31-1 byte string limit",
        ));
    }
    fs::write(path, body).map_err(|err| api_other(format!("Failed to write `{path}`: {err}")))?;
    Ok(())
}
