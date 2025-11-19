mod r_to_yaml;
mod unwind;
mod warning;
mod yaml_to_r;

use extendr_api::prelude::*;
use std::result::Result as StdResult;
use std::{cell::OnceCell, thread_local};
use unwind::EvalError;

const R_STRING_MAX_BYTES: usize = i32::MAX as usize;
type Fallible<T> = StdResult<T, EvalError>;

fn api_other(msg: impl Into<String>) -> EvalError {
    EvalError::Api(Error::Other(msg.into()))
}

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

/// Encode an R object as YAML 1.2.
///
/// @param value Any R object composed of lists, atomic vectors, and scalars.
/// @param multi When `TRUE`, treat `value` as a list of YAML documents and encode a stream.
/// @return A scalar character string containing YAML.
/// @export
#[extendr]
fn encode_yaml(value: Robj, #[extendr(default = "FALSE")] multi: bool) -> String {
    r_to_yaml::encode_yaml_impl(&value, multi).unwrap_or_else(handle_eval_error)
}

/// Parse YAML 1.2 document(s) into base R structures.
///
/// Supports scalars, sequences, and mappings; YAML tags are preserved in a
/// `yaml_tag` attribute when possible. YAML anchors and aliases are resolved
/// by the parser, so aliases behave like copies of their target nodes.
/// @param text Character vector; elements are concatenated with `"\n"`.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @param simplify When `FALSE`, keep YAML sequences as R lists instead of simplifying to atomic vectors.
/// @export
#[extendr]
fn parse_yaml(
    text: Strings,
    #[extendr(default = "FALSE")] multi: bool,
    #[extendr(default = "TRUE")] simplify: bool,
) -> Robj {
    yaml_to_r::parse_yaml_impl(text, multi, simplify).unwrap_or_else(handle_eval_error)
}

/// Read YAML 1.2 document(s) from a file path.
///
/// @param path Scalar string path to a YAML file.
/// @param multi When `TRUE`, return a list containing all documents in the stream.
/// @param simplify When `FALSE`, keep YAML sequences as R lists instead of simplifying to atomic vectors.
/// @export
#[extendr]
fn read_yaml(
    path: &str,
    #[extendr(default = "FALSE")] multi: bool,
    #[extendr(default = "TRUE")] simplify: bool,
) -> Robj {
    yaml_to_r::read_yaml_impl(path, multi, simplify).unwrap_or_else(handle_eval_error)
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
    r_to_yaml::write_yaml_impl(&value, path, multi).unwrap_or_else(handle_eval_error);
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
