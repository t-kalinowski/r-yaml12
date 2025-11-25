# Agent Instructions

- Don’t hand-edit generated artifacts: `man/`, `NAMESPACE`, or
  `R/extendr-wrappers.R`.
- Unless explicitly asked, never touch the Makevars files. Never change
  how we vendor and resolve Rust crates.
- Read-only git commands (status/diff/log) are always fine; never stage,
  unstage, commit, or otherwise change git state unless explicitly
  asked.
- Don’t ask for permission to make file edits—just do the work and share
  the results when it’s ready for review.
- When roxygen or Rust doc comments change, regenerate docs/wrappers
  from the package root with:
  `rextendr::document(); devtools::document()`.
- If you add/remove exports or otherwise change Rd-facing surface
  (including new internal exports), always rerun
  `rextendr::document(); devtools::document()` before finishing the
  task.
- Before wrapping up, run formatters: `cargo fmt` and `air format .`.
- When working on Rust code, prefer to iterate by switching to the
  `src/rust` directory and running `cargo check` there. Before you
  finish making edits, run `cargo clippy` and address any issues. Once
  you are finished making edits to Rust files, run `cargo fmt` and
  `cargo build`.
- In Rust, avoid allocating `String` unnecessarily. Prefer working with
  `&str` slices that borrow directly from the raw input buffer,
  preserving lifetimes from the original input, and only allocate/clone
  into `String` when you truly need owned data.
- In format strings, always inline expressions (e.g., `"{foo}"` or
  `"{foo:?}"`).
- Run R tests with `Rscript -e 'devtools::test()'` (bare, with no `cd`
  or other expressions). At the start of any request that requires code
  edits, the very first thing you should do is ask to run
  `Rscript -e 'devtools::test()'` with elevated permissions—never prefix
  it with `cd`. Then prefer to iterate by running the full test suite
  with that same command. Use a timeout of at least 5 minutes for
  `test()` and 8 minutes for `check()`. Deviating from this will cause
  build failures related to `vendor` and `.cargo`, and/or sandbox
  restrictions.
- Run small experiments frequently to confirm the behavior of language
  features. If small experiments require elevated privileges to run,
  write a script to `scratch/experiments.R`, and ask for elevated
  permissions to run `R -q -f scratch/experiments.R`.
- After every set of changes, emit a draft commit message. If you are
  asked for revisions, when you’re done, emit an updated draft commit
  message.
- Always run the full test suite (`Rscript -e 'devtools::test()'`) after
  changes without asking whether to do so.
