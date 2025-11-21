mod handlers;
mod r_to_yaml;
mod unwind;
mod warning;
mod yaml_to_r;

use crate::r_to_yaml::yaml_body;
use crate::r_to_yaml::R_STRING_MAX_BYTES;
use extendr_api::prelude::*;
use std::result::Result as StdResult;
use std::{cell::OnceCell, thread_local};
use unwind::EvalError;

type Fallible<T> = StdResult<T, EvalError>;

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
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return A scalar character string containing YAML.
/// @export
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
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param path Scalar string file path to write YAML to.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return Invisibly returns `NULL`.
/// @export
#[extendr]
fn write_yaml(value: Robj, path: &str, #[extendr(default = "FALSE")] multi: bool) -> Robj {
    let result = { r_to_yaml::write_yaml_impl(&value, path, multi) };
    if result.is_ok() {
        return NULL.into();
    }
    // Safe: entrypoint holds no owned locals; work lives in write_yaml_impl.
    // If adding locals here, wrap work in a block so drops happen before this call.
    handle_eval_error(result.unwrap_err())
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod yaml12;
    fn parse_yaml;
    fn format_yaml;
    fn read_yaml;
    fn write_yaml;
}
