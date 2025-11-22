use crate::handlers::HandlerRegistry;
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

    let is_plain_empty = style == ScalarStyle::Plain && value.trim().is_empty();

    let parsed = match tag {
        Some(tag) => {
            let owned_tag = tag.into_owned();
            match classify_tag(&owned_tag) {
                TagClass::Canonical(kind) => {
                    if kind == CanonicalTagKind::CoreNull && is_plain_empty {
                        Yaml::Value(Scalar::Null)
                    } else {
                        let canonical_tag = Cow::Owned(make_canonical_tag(kind));
                        Yaml::value_from_cow_and_metadata(value, style, Some(&canonical_tag))
                    }
                }
                TagClass::Core => {
                    let core_tag = Cow::Owned(owned_tag);
                    Yaml::value_from_cow_and_metadata(value, style, Some(&core_tag))
                }
                TagClass::NonCore => Yaml::Tagged(
                    Cow::Owned(owned_tag),
                    Box::new(Yaml::Value(Scalar::String(value))),
                ),
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
                    if !matches!(
                        classify_tag(tag),
                        TagClass::Canonical(CanonicalTagKind::CoreString)
                    ) {
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

    if is_timestamp_tag(tag) {
        if let Some(timestamp) = parse_timestamp_node(node)? {
            return Ok(timestamp);
        }
    }

    let value = yaml_to_robj(node, simplify, handlers)?;
    if matches!(classify_tag(tag), TagClass::Canonical(_)) {
        return Ok(value);
    }

    let rendered_tag = render_tag(tag);
    set_yaml_tag_attr(value, &rendered_tag)
}

fn render_tag(tag: &Tag) -> String {
    let mut rendered = String::with_capacity(tag.handle.len() + tag.suffix.len());
    rendered.push_str(tag.handle.as_str());
    rendered.push_str(tag.suffix.as_str());
    rendered
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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

#[derive(Copy, Clone, Debug)]
enum TimestampValue {
    Date(i64),
    DateTime(f64),
}

fn is_timestamp_tag(tag: &Tag) -> bool {
    matches!(
        (tag.handle.as_str(), tag.suffix.as_str()),
        ("tag:yaml.org,2002:", "timestamp")
            | ("!", "timestamp")
            | ("", "timestamp")
            | ("", "!timestamp")
            | ("", "!!timestamp")
            | ("", "tag:yaml.org,2002:timestamp")
    )
}

fn parse_timestamp_node(node: &mut Yaml) -> Fallible<Option<Robj>> {
    if let Yaml::Value(Scalar::String(value)) = node {
        if let Some(parsed) = parse_timestamp_scalar(value.as_ref()) {
            return timestamp_to_robj(parsed).map(Some);
        }
    }
    Ok(None)
}

fn timestamp_to_robj(value: TimestampValue) -> Fallible<Robj> {
    match value {
        TimestampValue::Date(days) => {
            let mut robj = r!(days as f64);
            robj.set_class(&["Date"])
                .map_err(|err| api_other(err.to_string()))?;
            Ok(robj)
        }
        TimestampValue::DateTime(seconds) => {
            let mut robj = r!(seconds);
            robj.set_class(&["POSIXct", "POSIXt"])
                .map_err(|err| api_other(err.to_string()))?;
            let tzone = Strings::from_values(["UTC"]);
            robj.set_attrib("tzone", tzone)
                .map_err(|err| api_other(err.to_string()))?;
            Ok(robj)
        }
    }
}

fn parse_timestamp_scalar(input: &str) -> Option<TimestampValue> {
    let trimmed = input.trim();
    let bytes = trimmed.as_bytes();
    let len = bytes.len();
    let mut idx = 0usize;

    let (year_raw, next) = parse_exact_digits(bytes, idx, 4)?;
    let year = year_raw as i32;
    idx = next;
    if bytes.get(idx)? != &b'-' {
        return None;
    }
    idx += 1;
    let (month, next) = parse_exact_digits(bytes, idx, 2)?;
    idx = next;
    if bytes.get(idx)? != &b'-' {
        return None;
    }
    idx += 1;
    let (day, next) = parse_exact_digits(bytes, idx, 2)?;
    idx = next;

    let date_days = days_from_civil(year, month, day)?;
    if idx == len {
        return Some(TimestampValue::Date(date_days));
    }

    let sep = bytes[idx];
    if sep != b'T' && sep != b't' && sep != b' ' {
        return None;
    }
    idx += 1;

    let (hour, next) = parse_exact_digits(bytes, idx, 2)?;
    idx = next;
    if bytes.get(idx)? != &b':' {
        return None;
    }
    idx += 1;

    let (minute, next) = parse_exact_digits(bytes, idx, 2)?;
    idx = next;
    if bytes.get(idx)? != &b':' {
        return None;
    }
    idx += 1;

    let (second, next) = parse_exact_digits(bytes, idx, 2)?;
    idx = next;

    let mut fraction = 0.0f64;
    if matches!(bytes.get(idx), Some(b'.')) {
        idx += 1;
        let start = idx;
        while idx < len && bytes[idx].is_ascii_digit() {
            idx += 1;
        }
        if idx == start {
            return None;
        }
        let mut place = 0.1f64;
        for &digit in &bytes[start..idx] {
            fraction += (digit - b'0') as f64 * place;
            place *= 0.1;
        }
    }

    while matches!(bytes.get(idx), Some(b' ')) {
        idx += 1;
    }

    let mut offset_minutes: i32 = 0;
    if idx < len {
        match bytes[idx] {
            b'Z' | b'z' => {
                idx += 1;
            }
            b'+' | b'-' => {
                let sign = if bytes[idx] == b'+' { 1i32 } else { -1i32 };
                idx += 1;
                let (hours_off, next) = parse_one_or_two_digits(bytes, idx)?;
                idx = next;
                let mut minutes_off = 0u32;
                if matches!(bytes.get(idx), Some(b':')) {
                    idx += 1;
                    let (mins, next) = parse_exact_digits(bytes, idx, 2)?;
                    minutes_off = mins;
                    idx = next;
                } else if idx + 2 <= len
                    && bytes[idx].is_ascii_digit()
                    && bytes[idx + 1].is_ascii_digit()
                {
                    let (mins, next) = parse_exact_digits(bytes, idx, 2)?;
                    minutes_off = mins;
                    idx = next;
                }
                offset_minutes = sign * (hours_off as i32 * 60 + minutes_off as i32);
            }
            _ => return None,
        }

        while matches!(bytes.get(idx), Some(b' ')) {
            idx += 1;
        }

        if idx != len {
            return None;
        }
    }

    if hour > 23 || minute > 59 || second > 59 {
        return None;
    }

    let seconds = hour as f64 * 3600.0 + minute as f64 * 60.0 + second as f64 + fraction;
    let total = date_days as f64 * 86_400.0 + seconds - offset_minutes as f64 * 60.0;
    Some(TimestampValue::DateTime(total))
}

fn parse_exact_digits(bytes: &[u8], start: usize, count: usize) -> Option<(u32, usize)> {
    let end = start.checked_add(count)?;
    if end > bytes.len() {
        return None;
    }
    let mut value = 0u32;
    for &digit in &bytes[start..end] {
        if !digit.is_ascii_digit() {
            return None;
        }
        value = value * 10 + (digit - b'0') as u32;
    }
    Some((value, end))
}

fn parse_one_or_two_digits(bytes: &[u8], start: usize) -> Option<(u32, usize)> {
    let mut idx = start;
    let mut value = 0u32;
    let mut count = 0usize;
    while idx < bytes.len() && count < 2 && bytes[idx].is_ascii_digit() {
        value = value * 10 + (bytes[idx] - b'0') as u32;
        idx += 1;
        count += 1;
    }
    if count == 0 {
        None
    } else {
        Some((value, idx))
    }
}

fn days_from_civil(year: i32, month: u32, day: u32) -> Option<i64> {
    if !(1..=12).contains(&month) || day == 0 {
        return None;
    }
    let dim = days_in_month(year, month);
    if day > dim {
        return None;
    }

    let y = year - i32::from(month <= 2);
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let doy = (153 * (month as i32 + if month > 2 { -3 } else { 9 }) + 2) / 5 + day as i32 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    Some((era as i64) * 146_097 + (doe as i64) - 719_468)
}

fn days_in_month(year: i32, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap_year(year) => 29,
        2 => 28,
        _ => 0,
    }
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
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
    use saphyr::YamlLoader;
    use saphyr_parser::Parser;

    fn tag_from_scalar(input: &str) -> Tag {
        let mut loader = YamlLoader::default();
        loader.early_parse(false);

        let mut parser = Parser::new_from_str(input);
        parser
            .load(&mut loader, false)
            .expect("parser should load tagged scalar");

        let mut docs = loader.into_documents();
        let doc = docs.pop().expect("expected one document");
        match doc {
            Yaml::Representation(_, _, Some(tag)) => tag.into_owned(),
            other => panic!("expected tagged scalar representation, got {other:?}"),
        }
    }

    #[test]
    fn canonical_string_tags_cover_all_forms() {
        let cases = [
            ("!!str example", ("tag:yaml.org,2002:", "str")),
            ("!str example", ("!", "str")),
            ("!<str> example", ("", "str")),
            ("!<!str> example", ("", "!str")),
            ("!<!!str> example", ("", "!!str")),
            (
                "!<tag:yaml.org,2002:str> example",
                ("", "tag:yaml.org,2002:str"),
            ),
        ];

        for (input, (handle, suffix)) in cases {
            let tag = tag_from_scalar(input);
            assert_eq!(tag.handle, handle);
            assert_eq!(tag.suffix, suffix);
            assert_eq!(
                canonical_tag_kind(&tag),
                Some(CanonicalTagKind::CoreString),
                "input `{input}` should map to canonical string tag"
            );
        }
    }

    #[test]
    fn canonical_null_tags_cover_all_forms() {
        let cases = [
            ("!!null null", ("tag:yaml.org,2002:", "null")),
            ("!<null> null", ("", "null")),
            ("!<!null> null", ("", "!null")),
            ("!<!!null> null", ("", "!!null")),
            (
                "!<tag:yaml.org,2002:null> null",
                ("", "tag:yaml.org,2002:null"),
            ),
        ];

        for (input, (handle, suffix)) in cases {
            let tag = tag_from_scalar(input);
            assert_eq!(tag.handle, handle);
            assert_eq!(tag.suffix, suffix);
            assert_eq!(
                canonical_tag_kind(&tag),
                Some(CanonicalTagKind::CoreNull),
                "input `{input}` should map to canonical null tag"
            );
        }
    }
}
