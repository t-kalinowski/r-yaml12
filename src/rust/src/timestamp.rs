use crate::{api_other, Fallible};
use extendr_api::prelude::*;
use saphyr::{Scalar, Tag, Yaml};
use std::borrow::Cow;

#[derive(Copy, Clone, Debug)]
pub(crate) enum ParsedTz {
    None,
    Z,
    Offset { minutes: i32 },
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct DateTimeValue {
    pub(crate) seconds: f64,
    pub(crate) tz: ParsedTz,
    pub(crate) space_separated: bool,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum TimestampValue {
    Date(i64),
    DateTime(DateTimeValue),
}

pub(crate) fn is_timestamp_tag(tag: &Tag) -> bool {
    matches!(
        (tag.handle.as_str(), tag.suffix.as_str()),
        ("tag:yaml.org,2002:", "timestamp")
            | ("!!", "timestamp")
            | ("!", "timestamp")
            | ("", "timestamp")
            | ("", "!timestamp")
            | ("", "!!timestamp")
            | ("", "tag:yaml.org,2002:timestamp")
            | ("", "<tag:yaml.org,2002:timestamp>")
            | ("!", "<tag:yaml.org,2002:timestamp>")
    )
}

pub(crate) fn parse_timestamp_scalar(input: &str) -> Option<TimestampValue> {
    let trimmed = input.trim_matches(|ch| ch == ' ' || ch == '\t');
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
    let (month, next) = parse_digit_range(bytes, idx, 1, 2)?;
    idx = next;
    if bytes.get(idx)? != &b'-' {
        return None;
    }
    idx += 1;
    let (day, next) = parse_digit_range(bytes, idx, 1, 2)?;
    idx = next;

    let date_days = days_from_civil(year, month, day)?;
    if idx == len {
        return Some(TimestampValue::Date(date_days));
    }

    let sep = bytes[idx];
    if sep == b'T' || sep == b't' {
        idx += 1;
    } else if is_space_or_tab(sep) {
        idx += 1;
        while idx < len && is_space_or_tab(bytes[idx]) {
            idx += 1;
        }
    } else {
        return None;
    }

    let space_separated = is_space_or_tab(sep);

    let (hour, next) = parse_digit_range(bytes, idx, 1, 2)?;
    idx = next;
    if bytes.get(idx)? != &b':' {
        return None;
    }
    idx += 1;

    let (minute, next) = parse_digit_range(bytes, idx, 1, 2)?;
    idx = next;
    if bytes.get(idx)? != &b':' {
        return None;
    }
    idx += 1;

    let (second, next) = parse_digit_range(bytes, idx, 1, 2)?;
    idx = next;

    let mut fraction = 0.0f64;
    if matches!(bytes.get(idx), Some(b'.')) {
        idx += 1;
        let start = idx;
        while idx < len && bytes[idx].is_ascii_digit() {
            idx += 1;
        }
        let mut place = 0.1f64;
        for &digit in &bytes[start..idx] {
            fraction += (digit - b'0') as f64 * place;
            place *= 0.1;
        }
    }

    while idx < len && is_space_or_tab(bytes[idx]) {
        idx += 1;
    }

    let mut parsed_tz = ParsedTz::None;
    if idx < len {
        match bytes[idx] {
            b'Z' | b'z' => {
                idx += 1;
                parsed_tz = ParsedTz::Z;
            }
            b'+' | b'-' => {
                let sign = if bytes[idx] == b'+' { 1i32 } else { -1i32 };
                idx += 1;
                let (hours_off, next) = parse_digit_range(bytes, idx, 1, 2)?;
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
                parsed_tz =
                    ParsedTz::Offset { minutes: sign * (hours_off as i32 * 60 + minutes_off as i32) };
            }
            _ => return None,
        }

        while idx < len && is_space_or_tab(bytes[idx]) {
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
    let offset_minutes = match parsed_tz {
        ParsedTz::Offset { minutes } => minutes,
        ParsedTz::Z => 0,
        ParsedTz::None => 0,
    };
    let total = date_days as f64 * 86_400.0 + seconds - offset_minutes as f64 * 60.0;
    Some(TimestampValue::DateTime(DateTimeValue {
        seconds: total,
        tz: parsed_tz,
        space_separated,
    }))
}

pub(crate) fn timestamp_to_robj(
    value: TimestampValue,
    preserve_tzone: bool,
    keep_empty_tzone: bool,
) -> Fallible<Robj> {
    match value {
        TimestampValue::Date(days) => {
            let mut robj = r!(days as f64);
            robj.set_class(&["Date"])
                .map_err(|err| api_other(err.to_string()))?;
            Ok(robj)
        }
        TimestampValue::DateTime(DateTimeValue {
            seconds,
            tz,
            space_separated,
        }) => {
            let mut robj = r!(seconds);
            robj.set_class(&["POSIXct", "POSIXt"])
                .map_err(|err| api_other(err.to_string()))?;
            if preserve_tzone {
                match tz {
                    ParsedTz::Z => {
                        robj.set_attrib("tzone", Strings::from_values(["UTC"]))
                            .map_err(|err| api_other(err.to_string()))?;
                    }
                    ParsedTz::Offset { minutes } if !space_separated => {
                        if let Some(tzone) = tzone_from_offset_minutes(minutes) {
                            let tzone = Strings::from_values([tzone]);
                            robj.set_attrib("tzone", tzone)
                                .map_err(|err| api_other(err.to_string()))?;
                        } else if keep_empty_tzone {
                            robj.set_attrib("tzone", Strings::from_values([""]))
                                .map_err(|err| api_other(err.to_string()))?;
                        }
                    }
                    ParsedTz::Offset { .. } | ParsedTz::None => {
                        // For space-separated or missing offsets, intentionally omit tzone.
                    }
                }
            }
            Ok(robj)
        }
    }
}

pub(crate) fn tzone_from_offset_minutes(minutes: i32) -> Option<String> {
    if minutes == 0 {
        return Some("UTC".to_string());
    }
    if minutes % 60 != 0 {
        return None;
    }
    let hours = minutes / 60;
    if !(-14..=14).contains(&hours) {
        return None;
    }
    Some(format!("Etc/GMT{hours:+}", hours = -hours))
}

#[allow(dead_code)]
pub(crate) fn olson_from_offset_minutes(minutes: i32) -> Option<String> {
    if minutes % 60 != 0 {
        return None;
    }
    let hours = minutes / 60;
    if hours == 0 {
        return Some("UTC".to_string());
    }
    let etc_hours = -hours;
    Some(format!("Etc/GMT{etc_hours:+}"))
}

pub(crate) fn format_offset(minutes: i32) -> String {
    let sign = if minutes < 0 { '-' } else { '+' };
    let mins = minutes.abs();
    let hours = mins / 60;
    let mins = mins % 60;
    format!("{sign}{hours:02}:{mins:02}")
}

pub(crate) fn offset_minutes_from_tzone(tz: &str) -> Option<i32> {
    if tz.eq_ignore_ascii_case("utc") || tz.eq_ignore_ascii_case("gmt") {
        return Some(0);
    }
    if let Some(stripped) = tz.strip_prefix("Etc/GMT") {
        if stripped.is_empty() {
            return Some(0);
        }
        if let Some(sign) = stripped.chars().next() {
            let rest = &stripped[sign.len_utf8()..];
            if let Ok(hours) = rest.parse::<i32>() {
                return Some(-sign_to_mult(sign) * hours * 60);
            }
        }
    }
    if let Some(sign) = tz.chars().next() {
        if sign == '+' || sign == '-' {
            let mut hours = 0i32;
            let mut mins = 0i32;
            let mut idx = 1usize;
            let bytes = tz.as_bytes();
            while idx < bytes.len() && idx < 3 && bytes[idx].is_ascii_digit() {
                hours = hours * 10 + (bytes[idx] - b'0') as i32;
                idx += 1;
            }
            if idx < bytes.len() && (bytes[idx] == b':' || bytes[idx] == b'.') {
                idx += 1;
            }
            let mut digits = 0;
            while idx < bytes.len() && digits < 2 && bytes[idx].is_ascii_digit() {
                mins = mins * 10 + (bytes[idx] - b'0') as i32;
                idx += 1;
                digits += 1;
            }
            if hours <= 14 && mins < 60 && idx == bytes.len() {
                return Some(sign_to_mult(sign) * (hours * 60 + mins));
            }
        }
    }
    None
}

fn sign_to_mult(sign: char) -> i32 {
    if sign == '-' {
        -1
    } else {
        1
    }
}

pub(crate) fn timestamp_tag() -> Tag {
    Tag {
        handle: String::new(),
        suffix: "<tag:yaml.org,2002:timestamp>".to_string(),
    }
}

pub(crate) fn core_timestamp_tag() -> Tag {
    Tag {
        handle: String::new(),
        suffix: "<tag:yaml.org,2002:timestamp>".to_string(),
    }
}

pub(crate) fn tagged_timestamp_with_tag(value: String, tag: Tag) -> Yaml<'static> {
    Yaml::Tagged(
        Cow::Owned(tag),
        Box::new(Yaml::Value(Scalar::String(Cow::Owned(value)))),
    )
}

pub(crate) fn yaml_from_formatted_timestamp(formatted: Vec<Option<String>>) -> Yaml<'static> {
    yaml_from_formatted_timestamp_with_tag(formatted, timestamp_tag())
}

pub(crate) fn yaml_from_formatted_timestamp_with_tag(
    formatted: Vec<Option<String>>,
    tag: Tag,
) -> Yaml<'static> {
    let mut values = Vec::with_capacity(formatted.len());
    for item in formatted {
        match item {
            None => values.push(Yaml::Value(Scalar::Null)),
            Some(text) => values.push(tagged_timestamp_with_tag(text, tag.clone())),
        }
    }
    if values.len() == 1 {
        values
            .into_iter()
            .next()
            .expect("vector length of 1 should yield one element")
    } else {
        Yaml::Sequence(values)
    }
}

pub(crate) fn format_r_time(
    robj: &Robj,
    format: &str,
    tz: Option<&str>,
) -> Fallible<Vec<Option<String>>> {
    with_digits_secs(9, || {
        let call = match tz {
            Some(tz) => lang!(
                "format",
                robj.clone(),
                format = format,
                tz = tz,
                usetz = false
            ),
            None => lang!("format", robj.clone(), format = format),
        };
        let res = call.eval()?;
        let strings: Strings = res.try_into()?;
        Ok(strings
            .iter()
            .map(|s| if s.is_na() { None } else { Some(s.to_string()) })
            .collect())
    })
}

pub(crate) fn format_posix_precise(
    robj: &Robj,
    offset_minutes: i32,
    space_separated: bool,
    append_z: bool,
) -> Fallible<Vec<Option<String>>> {
    let values = posix_seconds_from_robj(robj)?;
    let sep = if space_separated { ' ' } else { 'T' };
    let offset_suffix = if append_z {
        "Z".to_string()
    } else {
        format_offset(offset_minutes)
    };

    let mut out = Vec::with_capacity(values.len());
    for value in values {
        match value {
            None => out.push(None),
            Some(seconds_utc) => {
                let adjusted = seconds_utc + offset_minutes as f64 * 60.0;
                let (days, hour, minute, second, nanos) = split_seconds(adjusted);
                let (year, month, day) = civil_from_days(days);
                let mut formatted = format!(
                    "{year:04}-{month:02}-{day:02}{sep}{hour:02}:{minute:02}:{second:02}.{nanos:09}"
                );
                if append_z {
                    formatted.push('Z');
                } else {
                    if space_separated {
                        formatted.push(' ');
                    }
                    formatted.push_str(&offset_suffix);
                }
                out.push(Some(formatted));
            }
        }
    }
    Ok(out)
}

struct DigitsSecsGuard {
    old: Robj,
}

impl Drop for DigitsSecsGuard {
    fn drop(&mut self) {
        // Ignore restoration errors; best-effort reset.
        let _ = set_digits_secs_option(self.old.clone());
    }
}

fn with_digits_secs<F, T>(digits: i32, f: F) -> Fallible<T>
where
    F: FnOnce() -> Fallible<T>,
{
    let old = call!("getOption", "digits.secs")?;
    set_digits_secs_option(r!(digits))?;
    let guard = DigitsSecsGuard { old };
    let result = f();
    drop(guard);
    result
}

fn set_digits_secs_option(value: Robj) -> Fallible<()> {
    let opts = List::from_names_and_values(&["digits.secs"], vec![value].into_iter())
        .map_err(|err| api_other(err.to_string()))?;
    call!("do.call", "options", opts)?;
    Ok(())
}

fn posix_seconds_from_robj(robj: &Robj) -> Fallible<Vec<Option<f64>>> {
    if let Some(real) = robj.as_real_slice() {
        return Ok(real
            .iter()
            .map(|v| if v.is_nan() { None } else { Some(*v) })
            .collect());
    }
    if let Some(ints) = robj.as_integer_slice() {
        return Ok(ints
            .iter()
            .map(|v| if *v == i32::MIN { None } else { Some(*v as f64) })
            .collect());
    }
    Err(api_other("Expected a numeric POSIXct vector"))
}

fn split_seconds(secs: f64) -> (i64, u32, u32, u32, u32) {
    let mut whole = secs.floor();
    let mut frac = secs - whole;
    if frac < 0.0 {
        whole -= 1.0;
        frac += 1.0;
    }

    let mut nanos = (frac * 1_000_000_000.0).round() as i64;
    let mut total_seconds = whole as i64;
    if nanos == 1_000_000_000 {
        nanos = 0;
        total_seconds += 1;
    }

    let mut days = total_seconds.div_euclid(86_400);
    let mut secs_of_day = total_seconds.rem_euclid(86_400);
    if secs_of_day == 86_400 {
        secs_of_day = 0;
        days += 1;
    }

    let hour = (secs_of_day / 3_600) as u32;
    let minute = ((secs_of_day % 3_600) / 60) as u32;
    let second = (secs_of_day % 60) as u32;
    (days, hour, minute, second, nanos as u32)
}

fn civil_from_days(days: i64) -> (i32, u32, u32) {
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365;
    let mut y = yoe as i32 + era as i32 * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = mp + if mp < 10 { 3 } else { -9 };
    if m <= 2 {
        y += 1;
    }
    (y, m as u32, d as u32)
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

fn parse_digit_range(bytes: &[u8], start: usize, min: usize, max: usize) -> Option<(u32, usize)> {
    let mut idx = start;
    let mut value = 0u32;
    let mut count = 0usize;
    while idx < bytes.len() && count < max && bytes[idx].is_ascii_digit() {
        value = value * 10 + (bytes[idx] - b'0') as u32;
        idx += 1;
        count += 1;
    }
    if count < min {
        None
    } else {
        Some((value, idx))
    }
}

fn is_space_or_tab(byte: u8) -> bool {
    byte == b' ' || byte == b'\t'
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
