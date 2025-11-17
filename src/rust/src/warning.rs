use crate::unwind::{run_with_unwind_protect, EvalError};
use extendr_api::prelude::*;
use extendr_ffi as ffi;
use std::ffi::CString;
use std::os::raw::c_char;
use std::result::Result as StdResult;

#[allow(improper_ctypes)]
extern "C" {
    fn Rf_warningcall(call: ffi::SEXP, format: *const c_char, ...);
}

pub(crate) fn emit_warning(message: &str) -> StdResult<(), EvalError> {
    let c_message = CString::new(message)
        .map_err(|_| Error::Other("Warning message contains interior nul byte".to_string()))?;

    run_with_unwind_protect(|| unsafe {
        Rf_warningcall(
            ffi::R_NilValue,
            b"%s\0".as_ptr() as *const c_char,
            c_message.as_ptr(),
        );
    })
    .map_err(EvalError::Jump)
}
