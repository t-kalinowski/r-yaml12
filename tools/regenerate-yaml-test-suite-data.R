#!/usr/bin/env Rscript

# Regenerate the vendored yaml-test-suite data used by tests.
# Usage:
#   Rscript tools/regenerate-yaml-test-suite-data.R
#   YAML_TEST_SUITE_REF=<commit-ish> Rscript tools/regenerate-yaml-test-suite-data.R

pkg_name <- tryCatch(
  read.dcf("DESCRIPTION", "Package")[1],
  error = function(e) NULL
)
if (!identical(pkg_name, "yaml12")) {
  stop(
    "must be run from package root (DESCRIPTION not found or wrong package)",
    call. = FALSE
  )
}

repo_root <- getwd()

dest_dir <- file.path(repo_root, "tests", "testthat", "yaml-test-suite")
dest_data <- file.path(dest_dir, "data")

run <- function(cmd, args, wd = NULL) {
  if (!is.null(wd)) {
    owd <- setwd(wd)
    on.exit(setwd(owd), add = TRUE)
  }

  status <- system2(cmd, args)
  if (is.null(status)) {
    status <- 0L
  }
  if (!identical(status, 0L)) {
    stop(
      paste("command failed:", cmd, paste(args, collapse = " ")),
      call. = FALSE
    )
  }
}

tmp <- tempfile("yaml-test-suite-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

clone_dir <- file.path(tmp, "yaml-test-suite")
repo_url <- "https://github.com/yaml/yaml-test-suite.git"

run("git", c("clone", "--depth=1", "--no-single-branch", repo_url, clone_dir))
# Ensure the upstream data branch is available for make's worktree usage.
run("git", c("fetch", "origin", "data"), wd = clone_dir)
# Pre-create the local data branch if needed; ignore failure if it already exists.
tryCatch(
  run("git", c("branch", "--track", "data", "origin/data"), wd = clone_dir),
  error = function(e) NULL
)

ref <- Sys.getenv("YAML_TEST_SUITE_REF", unset = "")
if (nzchar(ref)) {
  run("git", c("checkout", ref), wd = clone_dir)
}

run("make", "data", wd = clone_dir)

license_src <- file.path(clone_dir, "License")
if (!file.exists(license_src)) {
  alt_license <- file.path(clone_dir, "LICENSE")
  if (file.exists(alt_license)) {
    license_src <- alt_license
  }
}

unlink(dest_data, recursive = TRUE)
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
rsync <- Sys.which("rsync")
if (!nzchar(rsync)) {
  stop(
    "rsync is required to copy yaml-test-suite data (preserves symlinks)",
    call. = FALSE
  )
}

# Preserve symlinks and delete stale entries; trailing slash copies contents.
run(rsync, c("-a", "--delete", file.path(clone_dir, "data"), dest_dir))

# Remove symlinks--keep only the actual target directories with data.
all_dirs <- list.dirs(dest_data, recursive = TRUE, full.names = TRUE)
real_dirs <- unique(normalizePath(all_dirs))
symlink_dirs <- setdiff(all_dirs, real_dirs)
if (length(symlink_dirs)) {
  unlink(symlink_dirs, recursive = TRUE, force = TRUE)
}

if (file.exists(license_src)) {
  file.copy(license_src, file.path(dest_dir, "License"), overwrite = TRUE)
}

message("yaml-test-suite data refreshed at ", dest_data)
