use crate::unwind::EvalError;
use crate::warning::emit_warning;
use crate::{api_other, sym_yaml_keys, sym_yaml_tag, Fallible};
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

    let parsed = match tag {
        Some(tag) => {
            let owned_tag = tag.into_owned();
            match classify_tag(&owned_tag) {
                TagClass::Canonical(kind) => {
                    if kind == CanonicalTagKind::CoreNull
                        && style == ScalarStyle::Plain
                        && value.trim().is_empty()
                    {
                        Yaml::Value(Scalar::Null)
                    } else {
                        let canonical_cow = Cow::Owned(make_canonical_tag(kind));
                        Yaml::value_from_cow_and_metadata(
                            value.clone(),
                            style,
                            Some(&canonical_cow),
                        )
                    }
                }
                TagClass::Core => {
                    let core_cow = Cow::Owned(owned_tag.clone());
                    Yaml::value_from_cow_and_metadata(value.clone(), style, Some(&core_cow))
                }
                TagClass::NonCore => Yaml::Tagged(
                    Cow::Owned(owned_tag),
                    Box::new(Yaml::Value(Scalar::String(value.clone()))),
                ),
            }
        }
        None if style == ScalarStyle::Plain && value.trim().is_empty() => Yaml::Value(Scalar::Null),
        None => Yaml::value_from_cow_and_metadata(value.clone(), style, None),
    };
    *node = parsed;
}

fn yaml_to_robj(node: &mut Yaml, simplify: bool) -> Fallible<Robj> {
    match node {
        Yaml::Value(scalar) => Ok(scalar_to_robj(scalar)),
        Yaml::Tagged(tag, inner) => convert_tagged(tag, inner.as_mut(), simplify),
        Yaml::Sequence(seq) => sequence_to_robj(seq, simplify),
        Yaml::Mapping(map) => mapping_to_robj(map, simplify),
        Yaml::Alias(_) => Err(api_other(
            "Internal error: encountered unresolved YAML alias node",
        )),
        Yaml::BadValue => Err(api_other("Encountered an invalid YAML scalar value")),
        Yaml::Representation(_, _, _) => {
            resolve_representation(node, simplify);
            yaml_to_robj(node, simplify)
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

fn sequence_to_robj(seq: &mut [Yaml], simplify_seqs: bool) -> Fallible<Robj> {
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

    // can't simplify, return a list
    let mut values = Vec::with_capacity(seq.len());
    for node in seq {
        values.push(yaml_to_robj(node, simplify_seqs)?);
    }

    Ok(List::from_values(values).into())
}

fn mapping_to_robj(map: &mut Mapping, simplify: bool) -> Fallible<Robj> {
    let len = map.len();

    let mut values: Vec<Robj> = Vec::with_capacity(len);
    let mut resolved_keys: Vec<Yaml> = Vec::with_capacity(len);

    // 1st pass: resolve keys and collect values
    for (key, value) in map.iter_mut() {
        // iter_mut() only returns mut value, not mut key.
        let mut resolved_key = key.clone();
        resolve_representation(&mut resolved_key, simplify);

        resolved_keys.push(resolved_key);
        values.push(yaml_to_robj(value, simplify)?);
    }

    // 2nd pass: build names as &str from resolved_keys.
    // String mapping keys should contribute regular R names. `needs_yaml_keys_attr`
    // tracks whether we must attach the `yaml_keys` attribute because at least
    // one key cannot be represented purely by R names: either a non-string key,
    // or a string key carrying a non-canonical (informative) tag. Canonical
    // core string tags are treated as "no information" for this purpose.
    let mut needs_yaml_keys_attr = false;
    let mut names: Vec<&str> = Vec::with_capacity(len);
    for resolved_key in &resolved_keys {
        match resolved_key {
            Yaml::Value(Scalar::String(name)) => {
                // Plain string key: representable as an R name with no extra metadata.
                names.push(name);
            }
            Yaml::Tagged(tag, inner) => {
                match inner.as_ref() {
                    // Tagged string scalar key: use the string as the R name.
                    Yaml::Value(Scalar::String(name)) => {
                        names.push(name);
                        // Non-canonical tags carry information that must be preserved
                        // via `yaml_keys`; canonical string tags do not.
                        if !matches!(
                            classify_tag(tag),
                            TagClass::Canonical(CanonicalTagKind::CoreString)
                        ) {
                            needs_yaml_keys_attr = true;
                        }
                    }
                    // Any other tagged key (non-string) cannot be represented by an R
                    // name alone; force `yaml_keys`.
                    _ => {
                        names.push("");
                        needs_yaml_keys_attr = true;
                    }
                }
            }
            // Non-string keys always require `yaml_keys`, since they cannot be expressed
            // purely via R names.
            _ => {
                needs_yaml_keys_attr = true;
                names.push("");
            }
        }
    }

    let mut list = List::from_names_and_values(&names, values.into_iter())
        .map_err(|err| api_other(err.to_string()))?;

    if needs_yaml_keys_attr {
        let mut yaml_keys = Vec::with_capacity(resolved_keys.len());
        for mut key in resolved_keys {
            yaml_keys.push(yaml_to_robj(&mut key, simplify)?);
        }
        let yaml_keys = List::from_values(yaml_keys);
        list.set_attrib(sym_yaml_keys(), yaml_keys)
            .map_err(|err| api_other(err.to_string()))?;
    }

    Ok(list.into())
}

fn convert_tagged(tag: &Tag, node: &mut Yaml, simplify: bool) -> Fallible<Robj> {
    let is_canonical = matches!(classify_tag(tag), TagClass::Canonical(_));
    let value = yaml_to_robj(node, simplify)?;
    if is_canonical {
        return Ok(value);
    }
    let tag_str = format!("{tag}");
    set_yaml_tag_attr(value, &tag_str)
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum CanonicalTagKind {
    CoreString,
    CoreNull,
}

enum TagClass {
    Canonical(CanonicalTagKind),
    Core,
    NonCore,
}

fn canonical_tag_kind(tag: &Tag) -> Option<CanonicalTagKind> {
    match (tag.handle.as_str(), tag.suffix.as_str()) {
        ("tag:yaml.org,2002:", "str") => Some(CanonicalTagKind::CoreString),
        ("!", "str") => Some(CanonicalTagKind::CoreString),
        ("", "str") => Some(CanonicalTagKind::CoreString),
        ("", "!str") => Some(CanonicalTagKind::CoreString),
        ("", "!!str") => Some(CanonicalTagKind::CoreString),
        ("", "tag:yaml.org,2002:str") => Some(CanonicalTagKind::CoreString),
        ("tag:yaml.org,2002:", "null") => Some(CanonicalTagKind::CoreNull),
        ("", "null") => Some(CanonicalTagKind::CoreNull),
        ("", "!null") => Some(CanonicalTagKind::CoreNull),
        ("", "!!null") => Some(CanonicalTagKind::CoreNull),
        ("", "tag:yaml.org,2002:null") => Some(CanonicalTagKind::CoreNull),
        _ => None,
    }
}

fn classify_tag(tag: &Tag) -> TagClass {
    if let Some(kind) = canonical_tag_kind(tag) {
        TagClass::Canonical(kind)
    } else if tag.is_yaml_core_schema() {
        TagClass::Core
    } else {
        TagClass::NonCore
    }
}

fn make_canonical_tag(kind: CanonicalTagKind) -> Tag {
    let suffix = match kind {
        CanonicalTagKind::CoreString => "str",
        CanonicalTagKind::CoreNull => "null",
    };
    Tag {
        handle: "tag:yaml.org,2002:".to_string(),
        suffix: suffix.to_string(),
    }
}

fn is_canonical_null_tag(tag: &str) -> bool {
    // Enumerate all variants that should be treated as the canonical null tag
    const NULL_TAGS: &[&str] = &[
        "null",
        "!null",
        "!!null",
        "!!!null",
        "<null>",
        "!<null>",
        "<!null>",
        "!<!null>",
        "<!!null>",
        "!<!!null>",
        "tag:yaml.org,2002:null",
        "!tag:yaml.org,2002:null",
        "<tag:yaml.org,2002:null>",
        "!<tag:yaml.org,2002:null>",
    ];

    NULL_TAGS.contains(&tag.trim())
}

fn set_yaml_tag_attr(mut value: Robj, tag: &str) -> Fallible<Robj> {
    // R NULL cannot carry attributes; skip instead of erroring and panicking
    if tag.is_empty() {
        return Ok(value);
    }

    if value.is_null() {
        if !is_canonical_null_tag(tag) {
            let warn_msg = format!(
                "yaml12: discarding tag `{tag}` on null scalar; R NULL cannot carry attributes"
            );
            emit_warning(&warn_msg)?;
        }
        return Ok(value);
    }

    value.set_attrib(sym_yaml_tag(), tag)?;
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

pub(crate) fn parse_yaml_impl(text: Strings, multi: bool, simplify: bool) -> Fallible<Robj> {
    match text.len() {
        0 => Ok(NULL.into()),
        1 => {
            let first = text.elt(0);
            if first.is_na() {
                return Err(api_other("`text` must not contain NA strings"));
            }
            let docs = load_yaml_documents(first.as_ref(), multi)?;
            docs_to_robj(docs, multi, simplify)
        }
        _ => {
            let joined_iter = joined_lines_iter(&text)?;
            let docs = load_yaml_documents_iter(joined_iter, multi)?;
            docs_to_robj(docs, multi, simplify)
        }
    }
}

fn docs_to_robj(mut docs: Vec<Yaml<'_>>, multi: bool, simplify: bool) -> Fallible<Robj> {
    if multi {
        let mut values = Vec::with_capacity(docs.len());
        for doc in docs.iter_mut() {
            values.push(yaml_to_robj(doc, simplify).map_err(wrap_unsupported)?);
        }
        Ok(List::from_values(values).into())
    } else {
        match docs.first_mut() {
            Some(doc) => yaml_to_robj(doc, simplify).map_err(wrap_unsupported),
            None => Ok(NULL.into()),
        }
    }
}

fn joined_lines_iter<'a>(text: &'a Strings) -> Fallible<JoinedLinesIter<'a>> {
    let mut lines = Vec::with_capacity(text.len());
    for line in text.iter() {
        if line.is_na() {
            return Err(api_other("`text` must not contain NA strings"));
        }
        lines.push(line.as_ref());
    }
    Ok(JoinedLinesIter::new(lines))
}

struct JoinedLinesIter<'a> {
    lines: std::vec::IntoIter<&'a str>,
    current: std::str::Chars<'a>,
}

impl<'a> JoinedLinesIter<'a> {
    fn new(lines: Vec<&'a str>) -> Self {
        let mut iter = lines.into_iter();
        let current = iter.next().unwrap_or("").chars();
        Self {
            lines: iter,
            current,
        }
    }
}

impl<'a> Iterator for JoinedLinesIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.current.next() {
            return Some(ch);
        }
        if let Some(next_line) = self.lines.next() {
            self.current = next_line.chars();
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

pub(crate) fn read_yaml_impl(path: &str, multi: bool, simplify: bool) -> Fallible<Robj> {
    let contents = fs::read_to_string(path)
        .map_err(|err| api_other(format!("Failed to read `{path}`: {err}")))?;
    let docs = load_yaml_documents(&contents, multi)?;
    docs_to_robj(docs, multi, simplify)
}
