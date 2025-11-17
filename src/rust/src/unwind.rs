use extendr_api::prelude::*;
use extendr_ffi as ffi;
use std::fmt;
use std::os::raw::c_void;
use std::result::Result as StdResult;

#[allow(improper_ctypes)]
extern "C" {
    fn unwind_protect_wrapper(
        fun: Option<unsafe extern "C" fn(data: *mut c_void) -> ffi::SEXP>,
        data: *mut c_void,
    ) -> ffi::SEXP;
    fn R_ContinueUnwind(cont: ffi::SEXP) -> !;
}

#[derive(Debug, Clone)]
pub struct LongjmpToken {
    tagged_ptr: ffi::SEXP,
}

impl LongjmpToken {
    fn cont_handle(&self) -> ffi::SEXP {
        let raw = self.tagged_ptr as usize & !1usize;
        raw as ffi::SEXP
    }

    pub unsafe fn resume(self) -> ! {
        R_ContinueUnwind(self.cont_handle());
    }

    pub fn from_tagged_ptr(tagged_ptr: ffi::SEXP) -> Self {
        Self { tagged_ptr }
    }
}

#[derive(Debug)]
pub enum RUnwindError {
    Api(Error),
    Jump(LongjmpToken),
}

impl From<Error> for RUnwindError {
    fn from(err: Error) -> Self {
        RUnwindError::Api(err)
    }
}

impl fmt::Display for RUnwindError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RUnwindError::Api(err) => write!(f, "{err}"),
            RUnwindError::Jump(_) => write!(f, "R longjmp"),
        }
    }
}

pub type Fallible<T> = StdResult<T, RUnwindError>;

pub fn run_with_unwind_protect<F>(f: F) -> Fallible<()>
where
    F: FnOnce() + Copy,
{
    unsafe extern "C" fn trampoline<F>(data: *mut c_void) -> ffi::SEXP
    where
        F: FnOnce() + Copy,
    {
        let data = data as *const ();
        let f: &F = &*(data as *const F);
        f();
        ffi::R_NilValue
    }

    let f_ptr = &f as *const F as *mut c_void;
    let res = unsafe { unwind_protect_wrapper(Some(trampoline::<F>), f_ptr) };
    if (res as usize & 1) == 1 {
        Err(RUnwindError::Jump(LongjmpToken::from_tagged_ptr(res)))
    } else {
        Ok(())
    }
}
