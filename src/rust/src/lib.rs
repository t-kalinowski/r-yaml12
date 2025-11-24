mod handlers;
mod r_to_yaml;
mod timestamp;
mod unwind;
mod warning;
mod yaml_to_r;

use crate::r_to_yaml::yaml_body;
use extendr_api::prelude::*;
use saphyr::{LoadableYamlNode, Yaml};
use std::result::Result as StdResult;
use std::{cell::OnceCell, thread_local};
use unwind::EvalError;

type Fallible<T> = StdResult<T, EvalError>;

pub(crate) const R_STRING_MAX_BYTES: usize = i32::MAX as usize;

fn api_other(msg: impl Into<String>) -> EvalError {
    EvalError::Api(Error::Other(msg.into()))
}

/// Map an `EvalError` back into R control flow.
///
/// Call this only after the entrypoint's Rust scope is clear of owned locals,
/// because `EvalError::Jump` resumes R's continuation and skips the rest of the
/// current frame. Wrap per-call work in a block that produces the `Fallible`
/// result so drops occur before delegating here.
///
/// Good:
/// ```
/// fn entrypoint() -> Robj {
///     let result: Fallible<_> = {
///         let _buf = String::from("tmp"); // drops before handle_eval_error
///         do_work()
///     };
///     match result {
///         Ok(val) => val,
///         Err(err) => handle_eval_error(err),
///     }
/// }
/// ```
///
/// Bad (skips `_buf` drop if a jump occurs):
/// ```
/// fn entrypoint_bad() -> Robj {
///     let _buf = String::from("tmp");
///     let result = do_work();
///     match result {
///         Ok(val) => val,
///         Err(err) => handle_eval_error(err), // jumps before _buf can drop
///     }
/// }
/// ```
fn handle_eval_error<T>(err: EvalError) -> T {
    match err {
        EvalError::Jump(token) => unsafe { token.resume() },
        EvalError::Api(err) => throw_r_error(err.to_string()),
    }
}

macro_rules! cached_sym {
    ($cell:ident, $name:ident, $getter:ident) => {
        thread_local! {
            static $cell: OnceCell<Robj> = OnceCell::new();
        }

        #[inline]
        pub(crate) fn $getter() -> Robj {
            $cell.with(|cell| cell.get_or_init(|| sym!($name)).clone())
        }
    };
}

cached_sym!(YAML_KEYS_SYM, yaml_keys, sym_yaml_keys);
cached_sym!(YAML_TAG_SYM, yaml_tag, sym_yaml_tag);

/// Format an R object as YAML 1.2.
///
/// When `multi = TRUE`, emits document start markers (`---`) and ends the
/// stream with a trailing newline. When `multi = FALSE`, writes a single
/// document node without a start marker and no final newline. Honors a
/// `yaml_tag` attribute on values (see examples).
///
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return A scalar character string containing YAML.
/// @export
/// @examples
/// cat(format_yaml(list(foo = 1, bar = list(TRUE, NA))))
///
/// docs <- list("first", "second")
/// cat(format_yaml(docs, multi = TRUE))
///
/// tagged <- structure("1 + 1", yaml_tag = "!expr")
/// cat(tagged_yaml <- format_yaml(tagged), "\n")
/// attr(parse_yaml(tagged_yaml), "yaml_tag")
#[extendr]
fn format_yaml(value: Robj, #[extendr(default = "FALSE")] multi: bool) -> Robj {
    let result = { r_to_yaml::format_yaml_impl(&value, multi) };
    if let Ok(yaml) = result {
        let body = yaml_body(&yaml, multi);
        if body.len() > R_STRING_MAX_BYTES {
            return handle_eval_error(api_other(
                "Formatted YAML exceeds R's 2^31-1 byte string limit",
            ));
        }
        return Robj::from(body);
    }
    // Safe: entrypoint holds no owned locals; work lives in format_yaml_impl.
    // If adding locals here, wrap work in a block so drops happen before this call.
    handle_eval_error(result.unwrap_err())
}

/// Parse YAML 1.2 document(s) into base R structures.
///
/// Supports scalars, sequences, and mappings; YAML tags are preserved in a
/// `yaml_tag` attribute when possible. YAML anchors and aliases are resolved
/// by the parser, so aliases behave like copies of their target nodes.
/// @param text Character vector; elements are concatenated with `"\n"`.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @param simplify When `FALSE`, keep YAML sequences as R lists instead of simplifying to atomic vectors.
/// @param handlers Named list of R functions keyed by YAML tag strings; matching handlers transform tagged values.
/// @export
#[extendr]
fn parse_yaml(
    text: Strings,
    #[extendr(default = "FALSE")] multi: bool,
    #[extendr(default = "TRUE")] simplify: bool,
    #[extendr(default = "NULL")] handlers: Robj,
) -> Robj {
    let result = { yaml_to_r::parse_yaml_impl(text, multi, simplify, handlers) };
    if let Ok(value) = result {
        return value;
    }
    // Safe: entrypoint holds no owned locals; work lives in parse_yaml_impl.
    // If adding locals here, wrap work in a block so drops happen before this call.
    handle_eval_error(result.unwrap_err())
}

/// Debug helper: print saphyr `Yaml` nodes without converting to R objects.
///
/// @noRd
#[extendr(invisible)]
fn dbg_yaml(text: Strings) -> Robj {
    let result: Fallible<()> = (|| -> Fallible<()> {
        if text.len() == 0 {
            return Ok(());
        }

        let mut joined = String::new();
        for (idx, part) in text.iter().enumerate() {
            if part.is_na() {
                Err(api_other("`text` must not contain NA strings"))?;
            }
            if idx > 0 {
                joined.push('\n');
            }
            joined.push_str(part.as_ref());
        }

        let docs = Yaml::load_from_str(&joined)
            .map_err(|err| api_other(format!("YAML parse error: {err}")))?;
        rprintln!("{:#?}", docs);
        Ok(())
    })();

    match result {
        Ok(()) => NULL.into(),
        Err(err) => handle_eval_error(err),
    }
}

/// Read YAML 1.2 document(s) from a file path.
///
/// @param path Scalar string path to a YAML file.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @param simplify When `FALSE`, keep YAML sequences as R lists instead of simplifying to atomic vectors.
/// @param handlers Named list of R functions keyed by YAML tag strings; matching handlers transform tagged values.
/// @export
#[extendr]
fn read_yaml(
    path: &str,
    #[extendr(default = "FALSE")] multi: bool,
    #[extendr(default = "TRUE")] simplify: bool,
    #[extendr(default = "NULL")] handlers: Robj,
) -> Robj {
    let result = { yaml_to_r::read_yaml_impl(path, multi, simplify, handlers) };
    if let Ok(value) = result {
        return value;
    }
    // Safe: entrypoint holds no owned locals; work lives in read_yaml_impl.
    // If adding locals here, wrap work in a block so drops happen before this call.
    handle_eval_error(result.unwrap_err())
}

/// Write an R object as YAML 1.2 to a file.
///
/// Writes the YAML stream to a file or stdout and always emits explicit document
/// start (`---`) markers and a final end (`...`) marker. Respects `yaml_tag`
/// attributes when encoding values.
///
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param path Scalar string file path to write YAML to. When `NULL` (the default),
///   write to R's standard output connection.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return Invisibly returns `value`.
/// @export
/// @examples
/// write_yaml(list(foo = 1, bar = list(2, "baz")))
///
/// write_yaml(list("foo", "bar"), multi = TRUE)
///
/// tagged <- structure("1 + 1", yaml_tag = "!expr")
/// write_yaml(tagged)
#[extendr(invisible)]
fn write_yaml(
    value: Robj,
    #[extendr(default = "NULL")] path: Robj,
    #[extendr(default = "FALSE")] multi: bool,
) -> Robj {
    let path_opt = if path.is_null() {
        None
    } else {
        match path.as_str() {
            Some(path) => Some(path),
            None => {
                return handle_eval_error(api_other(
                    "`path` must be NULL or a single, non-missing string",
                ));
            }
        }
    };
    let result: Fallible<()> = { r_to_yaml::write_yaml_impl(&value, path_opt, multi) };
    match result {
        Ok(()) => value,
        Err(err) => handle_eval_error(err),
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod yaml12;
    fn parse_yaml;
    fn dbg_yaml;
    fn format_yaml;
    fn read_yaml;
    fn write_yaml;
}
