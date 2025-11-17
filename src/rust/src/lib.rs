mod unwind;
mod warning;

use crate::unwind::{Fallible, RUnwindError};
use extendr_api::prelude::*;
use saphyr::{Mapping, Scalar, Tag, Yaml, YamlEmitter, YamlLoader};
use saphyr_parser::Parser;
use std::{borrow::Cow, cell::OnceCell, fs, thread_local};
use warning::emit_warning;

const R_STRING_MAX_BYTES: usize = i32::MAX as usize;

fn unwrap_or_resume<T>(res: Fallible<T>) -> T {
    match res {
        Ok(val) => val,
        Err(RUnwindError::Jump(token)) => unsafe { token.resume() },
        Err(RUnwindError::Api(err)) => throw_r_error(err.to_string()),
    }
}

macro_rules! cached_sym {
    ($cell:ident, $name:ident, $getter:ident) => {
        thread_local! {
            static $cell: OnceCell<Robj> = OnceCell::new();
        }

        #[inline]
        fn $getter() -> Robj {
            $cell.with(|cell| cell.get_or_init(|| sym!($name)).clone())
        }
    };
}

fn yaml_to_robj(node: &Yaml) -> Fallible<Robj> {
    match node {
        Yaml::Value(scalar) => Ok(scalar_to_robj(scalar)),
        Yaml::Tagged(tag, inner) => convert_tagged(tag, inner),
        Yaml::Sequence(seq) => sequence_to_robj(seq),
        Yaml::Mapping(map) => mapping_to_robj(map),
        Yaml::Alias(_) => {
            Err(Error::Other("YAML aliases are not supported by yaml12".to_string()).into())
        }
        Yaml::BadValue => {
            Err(Error::Other("Encountered an invalid YAML scalar value".to_string()).into())
        }
        Yaml::Representation(_, _, _) => {
            unreachable!("Unexpected Yaml::Representation; loader runs with early_parse(true)")
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

fn sequence_to_robj(seq: &[Yaml]) -> Fallible<Robj> {
    let mut values = Vec::with_capacity(seq.len());
    for node in seq {
        values.push(yaml_to_robj(node)?);
    }
    Ok(List::from_values(values).into())
}

fn mapping_to_robj(map: &Mapping) -> Fallible<Robj> {
    let mut names: Vec<&str> = Vec::with_capacity(map.len());
    let mut values = Vec::with_capacity(map.len());
    let mut has_non_string_key = false;
    for (key, value) in map.iter() {
        match key {
            Yaml::Value(Scalar::String(value)) => names.push(value.as_ref()),
            _ => {
                names.push("");
                has_non_string_key = true;
            }
        }
        values.push(yaml_to_robj(value)?);
    }
    let mut list = List::from_names_and_values(&names, values.into_iter())
        .map_err(|err| Error::Other(err.to_string()))?;
    if has_non_string_key {
        let mut key_values = Vec::with_capacity(map.len());
        for (key, _) in map.iter() {
            key_values.push(yaml_to_robj(key)?);
        }
        let yaml_keys = List::from_values(key_values);
        list.set_attrib(sym_yaml_keys(), yaml_keys)
            .map_err(|err| Error::Other(err.to_string()))?;
    }
    Ok(list.into())
}

cached_sym!(YAML_KEYS_SYM, yaml_keys, sym_yaml_keys);
cached_sym!(YAML_TAG_SYM, yaml_tag, sym_yaml_tag);

fn convert_tagged(tag: &Tag, node: &Yaml) -> Fallible<Robj> {
    let value = yaml_to_robj(node)?;
    set_yaml_tag_attr(value, &format!("{tag}"))
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
            .map_err(|err| Error::Other(err.to_string()))?;
    } else {
        emitter
            .dump_with_document_start(&docs[0], false)
            .map_err(|err| Error::Other(err.to_string()))?;
    }
    // R strings are limited to 2^31 - 1 bytes; error clearly if we would overflow.
    if output.len() > R_STRING_MAX_BYTES {
        return Err(
            Error::Other("Encoded YAML exceeds R's 2^31-1 byte string limit".to_string()).into(),
        );
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
        _ => Err(Error::Other(format!(
            "Unsupported R type {rtype:?} for YAML conversion",
            rtype = robj.rtype()
        ))
        .into()),
    }?;
    apply_tag_if_present(robj, node)
}

fn logical_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_logical_slice()
        .ok_or_else(|| Error::Other("Expected a logical vector".to_string()))?;
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if value.is_na() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::Boolean(value.to_bool())));
        }
    }
    Ok(sequence_or_scalar(values))
}

fn integer_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_integer_slice()
        .ok_or_else(|| Error::Other("Expected an integer vector".to_string()))?;
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if *value == i32::MIN {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::Integer(*value as i64)));
        }
    }
    Ok(sequence_or_scalar(values))
}

fn real_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let slice = robj
        .as_real_slice()
        .ok_or_else(|| Error::Other("Expected a numeric vector".to_string()))?;
    let mut values = Vec::with_capacity(slice.len());
    for value in slice {
        if value.is_nan() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::FloatingPoint((*value).into())));
        }
    }
    Ok(sequence_or_scalar(values))
}

fn character_to_yaml(robj: &Robj) -> Fallible<Yaml<'static>> {
    let strings = robj
        .as_str_iter()
        .ok_or_else(|| Error::Other("Expected a character vector".to_string()))?;
    let mut values = Vec::with_capacity(robj.len());
    for value in strings {
        if value.is_na() {
            values.push(Yaml::Value(Scalar::Null));
        } else {
            values.push(Yaml::Value(Scalar::String(Cow::Borrowed(value))));
        }
    }
    Ok(sequence_or_scalar(values))
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
                return Err(Error::Other(
                    "`yaml_keys` attribute must have the same length as the list".to_string(),
                )
                .into());
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

fn sequence_or_scalar(values: Vec<Yaml<'static>>) -> Yaml<'static> {
    match values.len() {
        1 => values.into_iter().next().unwrap(),
        _ => Yaml::Sequence(values),
    }
}

fn wrap_unsupported(err: RUnwindError) -> RUnwindError {
    match err {
        RUnwindError::Api(inner) => {
            RUnwindError::Api(Error::Other(format!("Unsupported YAML: {inner}")))
        }
        jump => jump,
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
        return Err(
            Error::Other("`yaml_tag` attribute must not be the empty string".to_string()).into(),
        );
    }
    if let Some(pos) = tag.rfind('!') {
        if pos + 1 >= tag.len() {
            return Err(Error::Other(format!("Invalid YAML tag `{tag}`")).into());
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
        Err(Error::Other(format!("Invalid YAML tag `{tag}`")).into())
    }
}

fn load_yaml_documents<'input>(text: &'input str, multi: bool) -> Fallible<Vec<Yaml<'input>>> {
    let mut parser = Parser::new_from_str(text);
    let mut loader = YamlLoader::default();
    parser
        .load(&mut loader, multi)
        .map_err(|err| Error::Other(format!("YAML parse error: {err}")))?;
    Ok(loader.into_documents())
}

fn parse_yaml_impl(text: Strings, multi: bool) -> Fallible<Robj> {
    match text.len() {
        0 => Ok(NULL.into()),
        1 => {
            let first = text.elt(0);
            if first.is_na() {
                return Err(Error::Other("`text` must not contain NA strings".to_string()).into());
            }
            let docs = load_yaml_documents(first.as_ref(), multi)?;
            docs_to_robj(docs, multi)
        }
        _ => {
            let joined_iter = joined_lines_iter(&text)?;
            let docs = load_yaml_documents_iter(joined_iter, multi)?;
            docs_to_robj(docs, multi)
        }
    }
}

fn docs_to_robj(docs: Vec<Yaml<'_>>, multi: bool) -> Fallible<Robj> {
    if multi {
        let mut values = Vec::with_capacity(docs.len());
        for doc in docs {
            values.push(
                yaml_to_robj(&doc).map_err(wrap_unsupported)?,
            );
        }
        Ok(List::from_values(values).into())
    } else {
        match docs.first() {
            Some(doc) => yaml_to_robj(doc).map_err(wrap_unsupported),
            None => Ok(NULL.into()),
        }
    }
}

fn joined_lines_iter<'a>(text: &'a Strings) -> Fallible<JoinedLinesIter<'a>> {
    let mut lines = Vec::with_capacity(text.len());
    for line in text.iter() {
        if line.is_na() {
            return Err(Error::Other("`text` must not contain NA strings".to_string()).into());
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
    parser
        .load(&mut loader, multi)
        .map_err(|err| Error::Other(format!("YAML parse error: {err}")))?;
    Ok(loader.into_documents())
}

fn encode_yaml_impl(value: &Robj, multi: bool) -> Fallible<String> {
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

fn read_yaml_impl(path: &str, multi: bool) -> Fallible<Robj> {
    let contents = fs::read_to_string(path)
        .map_err(|err| Error::Other(format!("Failed to read `{path}`: {err}")))?;
    let docs = load_yaml_documents(&contents, multi)?;
    docs_to_robj(docs, multi)
}

fn write_yaml_impl(value: &Robj, path: &str, multi: bool) -> Fallible<()> {
    let yaml = encode_yaml_impl(value, multi)?;
    fs::write(path, yaml)
        .map_err(|err| Error::Other(format!("Failed to write `{path}`: {err}")))?;
    Ok(())
}

/// Encode an R object as YAML 1.2.
///
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return A scalar character string containing YAML.
/// @export
#[extendr]
fn encode_yaml(value: Robj, #[extendr(default = "FALSE")] multi: bool) -> String {
    unwrap_or_resume(encode_yaml_impl(&value, multi))
}

/// Parse YAML 1.2 document(s) into base R structures.
///
/// Supports scalars, sequences, and mappings; YAML tags are preserved in a
/// `yaml_tag` attribute when possible. YAML aliases are rejected.
/// @param text Character vector; elements are concatenated with `"\n"`.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @export
#[extendr]
fn parse_yaml(text: Strings, #[extendr(default = "FALSE")] multi: bool) -> Robj {
    unwrap_or_resume(parse_yaml_impl(text, multi))
}

/// Read YAML 1.2 document(s) from a file path.
///
/// @param path Scalar string path to a YAML file.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @export
#[extendr]
fn read_yaml(path: &str, #[extendr(default = "FALSE")] multi: bool) -> Robj {
    unwrap_or_resume(read_yaml_impl(path, multi))
}

/// Write an R object as YAML 1.2 to a file.
///
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param path Scalar string file path to write YAML to.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return Invisibly returns `NULL`.
/// @export
#[extendr]
fn write_yaml(value: Robj, path: &str, #[extendr(default = "FALSE")] multi: bool) -> Robj {
    unwrap_or_resume(write_yaml_impl(&value, path, multi));
    NULL.into()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod yaml12;
    fn parse_yaml;
    fn encode_yaml;
    fn read_yaml;
    fn write_yaml;
}
