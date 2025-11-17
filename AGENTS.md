# Agent Instructions

- Don’t hand-edit generated artifacts: `man/`, `NAMESPACE`, or `R/extendr-wrappers.R`.
- When roxygen or Rust doc comments change, regenerate docs/wrappers from the package root with `rextendr::document(); devtools::document()`.
- Before wrapping up, run formatters: `cargo fmt` and `air format .`
- Run R tests with `Rscript -e 'devtools::test()'`, bare (without any `cd` or other expressions);
  At the start of a request that requires code edits, the very first thing you should do is ask to run `Rscript -e 'devtools::test()'` (bare) with elevated permissions. Then,
  prefer to iterate only by running the full test suite (that bare command). Make sure to set a timeout of at least 5 minutes for `test()` and 8 minutes for `check()`. If you deviate from these instructions,
  you will encounter build failures related to `vendor` and `.cargo`, and/or sandbox restrictions.
- Run Rust lints with `cargo check` and `cargo clippy` from the `src/rust/src` directory.
- In format strings, always inline expressions (e.g., `"{foo}"` or `"{foo:?}"`).
- Run small experiments frequently to confirm behavior of language features. If small experiments require elevated
  privalages to run, write a script to `scratch/experiments.R`, and ask for elevated permisisons to run `R -q -f scratch/experiments.R`.
