use crate::handlers::HandlerRegistry;
use crate::timestamp::{is_timestamp_tag, parse_timestamp_node, simplify_timestamp_sequence};
use crate::unwind::EvalError;
use crate::warning::emit_warning;
use crate::{api_other, sym_yaml_keys, sym_yaml_tag, Fallible, TIMESTAMP_SUPPORT_ENABLED};
use extendr_api::prelude::*;
use saphyr::{Mapping, Scalar, Tag, Yaml, YamlLoader};
use saphyr_parser::{Parser, ScalarStyle};
use std::{borrow::Cow, fs, mem};

fn resolve_representation(node: &mut Yaml, _simplify: bool) {
    let (value, style, tag) = match mem::replace(node, Yaml::BadValue) {
        Yaml::Representation(value, style, tag) => (value, style, tag),
        other => {
            *node = other;
            return;
        }
    };

    let is_plain_empty = style == ScalarStyle::Plain && value.trim().is_empty();

    let parsed = match tag {
        Some(tag) => {
            if tag.is_yaml_core_schema() {
                match tag.suffix.as_str() {
                    "str" => Yaml::value_from_cow_and_metadata(value, style, Some(&tag)),
                    "null" => {
                        if is_plain_empty {
                            Yaml::Value(Scalar::Null)
                        } else {
                            Yaml::value_from_cow_and_metadata(value, style, Some(&tag))
                        }
                    }
                    // _ if is_timestamp_tag(tag.as_ref()) => {
                    //     Yaml::Tagged(tag, Box::new(Yaml::Value(Scalar::String(value))))
                    // }
                    "binary" | "set" | "omap" | "pairs" | "timestamp" => {
                        Yaml::Tagged(tag, Box::new(Yaml::Value(Scalar::String(value))))
                    }
                    _ => {
                        let parsed =
                            Yaml::value_from_cow_and_metadata(value.clone(), style, Some(&tag));
                        if matches!(parsed, Yaml::BadValue)
                            && !matches!(
                                tag.suffix.as_str(),
                                "bool" | "int" | "float" | "null" | "str"
                            )
                        {
                            Yaml::Tagged(tag, Box::new(Yaml::Value(Scalar::String(value))))
                        } else {
                            parsed
                        }
                    }
                }
            } else {
                Yaml::Tagged(tag, Box::new(Yaml::Value(Scalar::String(value))))
            }
        }
        None if is_plain_empty => Yaml::Value(Scalar::Null),
        None => Yaml::value_from_cow_and_metadata(value, style, None),
    };

    *node = parsed;
}

fn yaml_to_robj(
    node: &mut Yaml,
    simplify: bool,
    handlers: Option<&HandlerRegistry<'_>>,
) -> Fallible<Robj> {
    match node {
        Yaml::Value(scalar) => Ok(scalar_to_robj(scalar)),
        Yaml::Tagged(tag, inner) => convert_tagged(tag, inner.as_mut(), simplify, handlers),
        Yaml::Sequence(seq) => sequence_to_robj(seq, simplify, handlers),
        Yaml::Mapping(map) => mapping_to_robj(map, simplify, handlers),
        Yaml::Alias(_) => Err(api_other(
            "Internal error: encountered unresolved YAML alias node",
        )),
        Yaml::BadValue => Err(api_other("Encountered an invalid YAML scalar value")),
        Yaml::Representation(_, _, _) => {
            resolve_representation(node, simplify);
            yaml_to_robj(node, simplify, handlers)
        }
    }
}

fn scalar_to_robj(scalar: &Scalar) -> Robj {
    match scalar {
        Scalar::Null => NULL.into(),
        Scalar::Boolean(value) => r!(*value),
        Scalar::Integer(value) => {
            if let Ok(v) = i32::try_from(*value) {
                r!(v)
            } else {
                r!(*value as f64)
            }
        }
        Scalar::FloatingPoint(value) => r!(value.into_inner()),
        Scalar::String(value) => r!(value.as_ref()),
    }
}

fn sequence_to_robj(
    seq: &mut [Yaml],
    simplify_seqs: bool,
    handlers: Option<&HandlerRegistry<'_>>,
) -> Fallible<Robj> {
    #[derive(Copy, Clone, PartialEq, Eq)]
    enum RVectorType {
        List,
        Logical,
        Integer,
        Double,
        Character,
    }

    let mut out_type = RVectorType::List;
    let mut simplify = simplify_seqs;

    if !simplify_seqs {
        let mut values = Vec::with_capacity(seq.len());
        for node in seq {
            resolve_representation(node, simplify_seqs);
            values.push(yaml_to_robj(node, simplify_seqs, handlers)?);
        }
        return Ok(List::from_values(values).into());
    }

    // iterate over the vec once to see if we can simplify, fail early/fast if not
    for node in seq.iter_mut() {
        resolve_representation(node, simplify_seqs);
        match node {
            Yaml::Tagged(_, _) => {
                simplify = false;
                break;
            }
            Yaml::Value(scalar_type) => {
                let this_kind = match scalar_type {
                    Scalar::Null => RVectorType::List,
                    Scalar::Boolean(_) => RVectorType::Logical,
                    Scalar::Integer(_) => RVectorType::Integer,
                    Scalar::FloatingPoint(_) => RVectorType::Double,
                    Scalar::String(_) => RVectorType::Character,
                };

                if this_kind == out_type || matches!(scalar_type, Scalar::Null) {
                    continue;
                }
                if this_kind == RVectorType::Double && out_type == RVectorType::Integer {
                    out_type = RVectorType::Double;
                    continue;
                }
                if let Scalar::Integer(i) = scalar_type {
                    if out_type == RVectorType::Double {
                        continue;
                    }
                    if i32::try_from(*i).is_err() {
                        out_type = RVectorType::Double;
                        continue;
                    }
                }
                if out_type == RVectorType::List {
                    out_type = this_kind;
                    continue;
                }
                simplify = false;
                break;
            }
            _ => {
                simplify = false;
                break;
            }
        }
    }

    if simplify {
        match out_type {
            RVectorType::Logical => {
                let logicals = Logicals::from_values(seq.iter().map(|node| match node {
                    Yaml::Value(Scalar::Boolean(b)) => (*b).into(),
                    Yaml::Value(Scalar::Null) => Rbool::na_value(),
                    _ => unreachable!("expected only booleans or nulls"),
                }));
                return Ok(logicals.into());
            }
            RVectorType::Integer => {
                let integers = Integers::from_values(seq.iter().map(|node| match node {
                    Yaml::Value(Scalar::Integer(value)) => Rint::from(*value as i32),
                    Yaml::Value(Scalar::Null) => Rint::na(),
                    _ => unreachable!("expected only integers or nulls"),
                }));
                return Ok(integers.into());
            }
            RVectorType::Double => {
                let doubles = Doubles::from_values(seq.iter().map(|node| match node {
                    Yaml::Value(Scalar::FloatingPoint(value)) => Rfloat::from(value.into_inner()),
                    Yaml::Value(Scalar::Integer(value)) => Rfloat::from(*value as f64),
                    Yaml::Value(Scalar::Null) => Rfloat::na(),
                    _ => unreachable!("expected only doubles, integers, or nulls"),
                }));
                return Ok(doubles.into());
            }
            RVectorType::Character => {
                let strings = Strings::from_values(seq.iter().map(|node| match node {
                    Yaml::Value(Scalar::String(value)) => Rstr::from(value.as_ref()),
                    Yaml::Value(Scalar::Null) => Rstr::na(),
                    _ => unreachable!("expected only strings or nulls"),
                }));
                return Ok(strings.into());
            }
            RVectorType::List => {}
        }
    }

    // can't simplify via scalar types; try timestamp-aware simplification
    if TIMESTAMP_SUPPORT_ENABLED {
        if let Some(out) =
            simplify_timestamp_sequence(seq, |node| resolve_representation(node, true))?
        {
            return Ok(out);
        }
    }

    // can't simplify, return a list
    let mut values = Vec::with_capacity(seq.len());
    for node in seq {
        values.push(yaml_to_robj(node, simplify_seqs, handlers)?);
    }

    Ok(List::from_values(values).into())
}

fn mapping_to_robj(
    map: &mut Mapping,
    simplify: bool,
    handlers: Option<&HandlerRegistry<'_>>,
) -> Fallible<Robj> {
    let len = map.len();

    if handlers.is_none() {
        let all_plain_string_keys = map
            .iter()
            .all(|(key, _)| matches!(key, Yaml::Value(Scalar::String(_))));

        if all_plain_string_keys {
            let mut names: Vec<Cow<'_, str>> = Vec::with_capacity(len);
            let mut values: Vec<Robj> = Vec::with_capacity(len);
            for (key, mut value) in mem::take(map) {
                let name = match key {
                    Yaml::Value(Scalar::String(name)) => name,
                    _ => unreachable!("checked for only plain string keys"),
                };
                names.push(name);
                values.push(yaml_to_robj(&mut value, simplify, handlers)?);
            }

            let name_refs: Vec<&str> = names.iter().map(|name| name.as_ref()).collect();
            let list = List::from_names_and_values(&name_refs, values.into_iter())
                .map_err(|err| api_other(err.to_string()))?;
            return Ok(list.into());
        }
    }

    let mut values: Vec<Robj> = Vec::with_capacity(len);
    let mut keys_for_names: Vec<Yaml> = Vec::with_capacity(len);
    let mut key_handler_results: Vec<Option<Robj>> = Vec::with_capacity(len);

    // 1st pass: resolve keys/values while consuming the mapping to avoid cloning keys.
    for (mut key, mut value) in mem::take(map) {
        resolve_representation(&mut key, simplify);

        // If the key is tagged and a handler exists, apply it to the key itself.
        // Keep the handled value alive so we can borrow its string data when
        // constructing R names without allocating.
        let key_handler_result = if let (Some(registry), Yaml::Tagged(tag, _)) = (handlers, &key) {
            if let Some(handler) = registry.get_for_tag(tag.as_ref()) {
                let key_obj = yaml_to_robj(&mut key, simplify, handlers)?;
                Some(registry.apply(handler, key_obj)?)
            } else {
                None
            }
        } else {
            None
        };

        keys_for_names.push(key);
        key_handler_results.push(key_handler_result);
        values.push(yaml_to_robj(&mut value, simplify, handlers)?);
    }

    // 2nd pass: build names as &str from keys_for_names.
    // String mapping keys should contribute regular R names. `needs_yaml_keys_attr`
    // tracks whether we must attach the `yaml_keys` attribute because at least
    // one key cannot be represented purely by R names: either a non-string key,
    // or a string key carrying a non-canonical (informative) tag. Canonical
    // core string tags are treated as "no information" for this purpose.
    let mut needs_yaml_keys_attr = false;
    let mut names: Vec<&str> = Vec::with_capacity(len);
    for (key_for_name, key_handler_result) in keys_for_names.iter().zip(key_handler_results.iter())
    {
        if let Some(handled) = key_handler_result {
            if let Some(name) = handled.as_str() {
                // Handler returned a string: use it for the R name.
                names.push(name);
                continue;
            } else {
                // Handler returned a non-string; we'll still need yaml_keys.
                needs_yaml_keys_attr = true;
            }
        }

        match key_for_name {
            Yaml::Value(Scalar::String(name)) => {
                // Plain string key: representable as an R name with no extra metadata.
                names.push(name);
            }
            Yaml::Tagged(tag, inner) => match inner.as_ref() {
                Yaml::Value(Scalar::String(name)) => {
                    names.push(name);
                    if !is_core_string_tag(tag) {
                        needs_yaml_keys_attr = true;
                    }
                }
                _ => {
                    needs_yaml_keys_attr = true;
                    names.push("");
                }
            },
            _ => {
                needs_yaml_keys_attr = true;
                names.push("");
            }
        }
    }

    let mut list = List::from_names_and_values(&names, values.into_iter())
        .map_err(|err| api_other(err.to_string()))?;

    if needs_yaml_keys_attr {
        let mut yaml_keys = Vec::with_capacity(keys_for_names.len());
        for (mut key, handled_value) in keys_for_names.into_iter().zip(key_handler_results) {
            if let Some(val) = handled_value {
                yaml_keys.push(val);
            } else {
                yaml_keys.push(yaml_to_robj(&mut key, simplify, handlers)?);
            }
        }
        let yaml_keys = List::from_values(yaml_keys);
        list.set_attrib(sym_yaml_keys(), yaml_keys)
            .map_err(|err| api_other(err.to_string()))?;
    }

    Ok(list.into())
}

fn convert_tagged(
    tag: &Tag,
    node: &mut Yaml,
    simplify: bool,
    handlers: Option<&HandlerRegistry<'_>>,
) -> Fallible<Robj> {
    if let Some(registry) = handlers {
        if let Some(handler) = registry.get_for_tag(tag) {
            let value = yaml_to_robj(node, simplify, handlers)?;
            return registry.apply(handler, value);
        }
    }

    if TIMESTAMP_SUPPORT_ENABLED && is_timestamp_tag(tag) {
        let keep_empty_tzone = tag.handle.as_str() == "!";
        let preserve_tzone = true;
        if let Some(timestamp) = parse_timestamp_node(node, preserve_tzone, keep_empty_tzone)? {
            return Ok(timestamp);
        }
    }

    let value = yaml_to_robj(node, simplify, handlers)?;
    if tag.is_yaml_core_schema() {
        return match tag.suffix.as_str() {
            "str" | "null" | "bool" | "int" | "float" | "seq" | "map" => Ok(value),
            "timestamp" | "set" | "omap" | "pairs" | "binary" => set_yaml_tag_attr(value, tag),
            other => Err(api_other(format!(
                "Unsupported core-schema tag `{handle}{other}`",
                handle = tag.handle
            ))),
        };
    }

    set_yaml_tag_attr(value, tag)
}

fn is_core_string_tag(tag: &Tag) -> bool {
    tag.is_yaml_core_schema() && tag.suffix.as_str() == "str"
}

fn is_core_null_tag(tag: &Tag) -> bool {
    tag.is_yaml_core_schema() && tag.suffix.as_str() == "null"
}

fn set_yaml_tag_attr(mut value: Robj, tag: &Tag) -> Fallible<Robj> {
    let mut rendered_tag = String::with_capacity(tag.handle.len() + tag.suffix.len());
    rendered_tag.push_str(tag.handle.as_str());
    rendered_tag.push_str(tag.suffix.as_str());

    if rendered_tag.is_empty() {
        return Ok(value);
    }

    if value.is_null() {
        if !is_core_null_tag(tag) {
            let warn_msg = format!(
                "yaml12: discarding tag `{rendered_tag}` on null scalar; R NULL cannot carry attributes"
            );
            emit_warning(&warn_msg)?;
        }
        return Ok(value);
    }

    value.set_attrib(sym_yaml_tag(), rendered_tag.as_str())?;
    Ok(value)
}

fn wrap_unsupported(err: EvalError) -> EvalError {
    match err {
        EvalError::Api(inner) => EvalError::Api(Error::Other(format!("Unsupported YAML: {inner}"))),
        EvalError::Jump(token) => EvalError::Jump(token),
    }
}

fn load_yaml_documents<'input>(text: &'input str, multi: bool) -> Fallible<Vec<Yaml<'input>>> {
    let mut parser = Parser::new_from_str(text);
    let mut loader = YamlLoader::default();
    loader.early_parse(false);
    parser
        .load(&mut loader, multi)
        .map_err(|err| api_other(format!("YAML parse error: {err}")))?;
    Ok(loader.into_documents())
}

pub(crate) fn parse_yaml_impl(
    text: Strings,
    multi: bool,
    simplify: bool,
    handlers: Robj,
) -> Fallible<Robj> {
    let handler_registry = HandlerRegistry::from_robj(&handlers)?;
    let handlers = handler_registry.as_ref();

    match text.len() {
        0 => Ok(NULL.into()),
        1 => {
            let first = text.elt(0);
            if first.is_na() {
                return Err(api_other("`text` must not contain NA strings"));
            }
            let docs = load_yaml_documents(first.as_ref(), multi)?;
            docs_to_robj(docs, multi, simplify, handlers)
        }
        _ => {
            let joined_iter = joined_lines_iter(&text)?;
            let docs = load_yaml_documents_iter(joined_iter, multi)?;
            docs_to_robj(docs, multi, simplify, handlers)
        }
    }
}

fn docs_to_robj(
    mut docs: Vec<Yaml<'_>>,
    multi: bool,
    simplify: bool,
    handlers: Option<&HandlerRegistry<'_>>,
) -> Fallible<Robj> {
    if multi {
        let mut values = Vec::with_capacity(docs.len());
        for doc in docs.iter_mut() {
            values.push(yaml_to_robj(doc, simplify, handlers).map_err(wrap_unsupported)?);
        }
        Ok(List::from_values(values).into())
    } else {
        match docs.first_mut() {
            Some(doc) => yaml_to_robj(doc, simplify, handlers).map_err(wrap_unsupported),
            None => Ok(NULL.into()),
        }
    }
}

fn joined_lines_iter<'a>(text: &'a Strings) -> Fallible<JoinedLinesIter<'a>> {
    for line in text.iter() {
        if line.is_na() {
            return Err(api_other("`text` must not contain NA strings"));
        }
    }
    Ok(JoinedLinesIter::new(text.as_slice().iter()))
}

type LinesIter<'a> = std::slice::Iter<'a, Rstr>;

struct JoinedLinesIter<'a> {
    lines: LinesIter<'a>,
    current: std::str::Chars<'a>,
}

impl<'a> JoinedLinesIter<'a> {
    fn new(mut lines: LinesIter<'a>) -> Self {
        let current = lines
            .next()
            .map_or_else(|| "".chars(), |line| line.as_ref().chars());
        Self { lines, current }
    }
}

impl<'a> Iterator for JoinedLinesIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.current.next() {
            return Some(ch);
        }
        if let Some(next_line) = self.lines.next() {
            self.current = next_line.as_ref().chars();
            return Some('\n');
        }
        None
    }
}

fn load_yaml_documents_iter<'input, I>(iter: I, multi: bool) -> Fallible<Vec<Yaml<'input>>>
where
    I: Iterator<Item = char> + 'input,
{
    let mut parser = Parser::new_from_iter(iter);
    let mut loader = YamlLoader::default();
    loader.early_parse(false);
    parser
        .load(&mut loader, multi)
        .map_err(|err| Error::Other(format!("YAML parse error: {err}")))?;
    Ok(loader.into_documents())
}

pub(crate) fn read_yaml_impl(
    path: &str,
    multi: bool,
    simplify: bool,
    handlers: Robj,
) -> Fallible<Robj> {
    let handler_registry = HandlerRegistry::from_robj(&handlers)?;
    let handlers = handler_registry.as_ref();

    let contents = fs::read_to_string(path)
        .map_err(|err| api_other(format!("Failed to read `{path}`: {err}")))?;
    let docs = load_yaml_documents(&contents, multi)?;
    docs_to_robj(docs, multi, simplify, handlers)
}

#[cfg(test)]
mod tests {
    use super::*;
    use saphyr::{LoadableYamlNode, Scalar as YamlScalar};

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    enum ParsedValueKind {
        String,
        Boolean,
    }

    fn load_scalar(input: &str) -> Yaml<'_> {
        let mut docs = Yaml::load_from_str(input).expect("parser should load tagged scalar");
        docs.pop().expect("expected one document")
    }

    fn normalized_suffix(suffix: &str) -> &str {
        let suffix = suffix.trim_start_matches('!');
        suffix.strip_prefix("tag:yaml.org,2002:").unwrap_or(suffix)
    }

    #[test]
    fn canonical_string_tags_cover_all_forms() {
        let canonical_string = Tag {
            handle: "tag:yaml.org,2002:".to_string(),
            suffix: "str".to_string(),
        };
        assert!(is_core_string_tag(&canonical_string));

        let cases = [
            ("!!str true", ParsedValueKind::String),
            ("!str true", ParsedValueKind::Boolean),
            ("!<str> true", ParsedValueKind::Boolean),
            ("!<!str> true", ParsedValueKind::Boolean),
            ("!<!!str> true", ParsedValueKind::Boolean),
            ("!<tag:yaml.org,2002:str> true", ParsedValueKind::Boolean),
        ];

        for (input, expected_value) in cases {
            let parsed = load_scalar(input);
            match parsed {
                Yaml::Value(YamlScalar::String(value)) => {
                    assert_eq!(
                        expected_value,
                        ParsedValueKind::String,
                        "input `{input}` should resolve to string value"
                    );
                    assert_eq!(value.as_ref(), "true");
                }
                Yaml::Tagged(tag, inner) => {
                    assert_eq!(
                        is_core_string_tag(&tag),
                        tag.is_yaml_core_schema()
                            && normalized_suffix(tag.suffix.as_str()) == "str",
                        "input `{input}` canonical detection should match core `str` suffix",
                    );
                    match (expected_value, inner.as_ref()) {
                        (ParsedValueKind::Boolean, Yaml::Value(YamlScalar::Boolean(value))) => {
                            assert!(
                                *value,
                                "input `{input}` should parse to boolean `true` when not core"
                            );
                        }
                        (expected, other) => {
                            panic!(
                                "input `{input}` expected value kind {expected:?}, got {other:?}"
                            )
                        }
                    }
                }
                other => panic!("input `{input}` expected tagged or string value, got {other:?}"),
            }
        }
    }

    #[test]
    fn canonical_null_tags_cover_all_forms() {
        let canonical_null = Tag {
            handle: "tag:yaml.org,2002:".to_string(),
            suffix: "null".to_string(),
        };
        assert!(is_core_null_tag(&canonical_null));

        let cases = [
            "!!null null",
            "!<null> null",
            "!<!null> null",
            "!<!!null> null",
            "!<tag:yaml.org,2002:null> null",
        ];

        for input in cases {
            let parsed = load_scalar(input);
            match parsed {
                Yaml::Value(YamlScalar::Null) => {
                    // Canonical null scalars should not carry tags.
                }
                Yaml::Tagged(tag, inner) => {
                    assert_eq!(
                        is_core_null_tag(&tag),
                        tag.is_yaml_core_schema()
                            && normalized_suffix(tag.suffix.as_str()) == "null",
                        "input `{input}` canonical detection should match core `null` suffix",
                    );
                    assert!(
                        matches!(inner.as_ref(), Yaml::Value(YamlScalar::Null)),
                        "input `{input}` should parse to tagged null scalar"
                    );
                }
                other => panic!("input `{input}` expected null scalar, got {other:?}"),
            }
        }
    }
}
