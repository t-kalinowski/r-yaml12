// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.

#include <Rinternals.h>
#include <setjmp.h>
#include <stdint.h>

void R_init_yaml12_extendr(void *dll);
SEXP unwind_protect_wrapper(SEXP (*fun)(void *data), void *data);
void not_so_long_jump(void *jmpbuf, Rboolean jump);

void R_init_yaml12(void *dll) {
    R_init_yaml12_extendr(dll);
}

SEXP unwind_protect_wrapper(SEXP (*fun)(void *data), void *data) {
    SEXP token = R_MakeUnwindCont();
    PROTECT(token);
    jmp_buf jmpbuf;
    if (setjmp(jmpbuf)) {
        // tag pointer with low bit so rust can detect jump
        UNPROTECT(1);
        return (SEXP)((uintptr_t)token | 1);
    }
    SEXP res = R_UnwindProtect(fun, data, (void (*)(void *, Rboolean)) not_so_long_jump, &jmpbuf, token);
    UNPROTECT(1);
    return res;
}

void not_so_long_jump(void *jmpbuf, Rboolean jump) {
    if (jump == TRUE) {
        longjmp(*(jmp_buf *)jmpbuf, 1);
    }
}
