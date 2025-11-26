test_that("write_yaml writes and read_yaml reads single documents", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  value <- list(alpha = 1L, nested = c(TRUE, NA))
  encoded <- format_yaml(value)
  out <- write_yaml(value, path)

  expect_identical(out, value)
  expect_true(file.exists(path))
  file_lines <- readLines(path)
  body_lines <- strsplit(encoded, "\n", fixed = TRUE)[[1]]
  expect_identical(file_lines[[1]], "---")
  expect_identical(file_lines[[length(file_lines)]], "...")
  expect_identical(file_lines[seq_along(body_lines) + 1L], body_lines)
  expect_identical(read_yaml(path), value)
  expect_identical(read_yaml(path, simplify = TRUE), value)
  expect_identical(
    read_yaml(path, simplify = FALSE),
    list(alpha = 1L, nested = list(TRUE, NULL))
  )
  expect_identical(parse_yaml(encoded), value)
})

test_that("write_yaml defaults to R stdout when path is NULL", {
  value <- list(alpha = 1L, nested = c(TRUE, NA))
  encoded <- format_yaml(value)

  output <- paste0(
    capture.output(out <- write_yaml(value)),
    collapse = "\n"
  )
  expect_identical(out, value)
  expect_true(startsWith(output, "---\n"))
  expect_true(endsWith(output, "\n..."))
  expect_identical(output, paste0("---\n", encoded, "\n..."))
  expect_identical(parse_yaml(output), value)
})

test_that("write_yaml and read_yaml handle multi-document streams", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  docs <- list(list(foo = 1L), list(bar = list(2L, NULL)))
  encoded <- format_yaml(docs, multi = TRUE)
  write_yaml(docs, path, multi = TRUE)

  file_lines <- readLines(path)
  expected_lines <- strsplit(
    encoded,
    "\n",
    fixed = TRUE
  )[[1]]
  if (identical(expected_lines[[length(expected_lines)]], "")) {
    expected_lines <- expected_lines[-length(expected_lines)]
  }
  expected_lines <- c(expected_lines, "...")
  expect_identical(file_lines, expected_lines)

  expect_identical(
    parse_yaml(encoded, multi = TRUE, simplify = FALSE),
    docs
  )

  simplified_docs <- docs
  simplified_docs[[2]]$bar <- c(2L, NA)
  expect_identical(read_yaml(path, multi = TRUE), simplified_docs)
  expect_identical(
    read_yaml(path, multi = TRUE, simplify = TRUE),
    simplified_docs
  )
  expect_identical(
    read_yaml(path, multi = TRUE, simplify = FALSE),
    docs
  )
})

test_that("read_yaml respects simplify = FALSE for simple scalars", {
  path <- tempfile("yaml12-simplify-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("- 1", "- 2", "- null"), path)

  unsimplified <- read_yaml(path, simplify = FALSE)
  expect_identical(unsimplified, list(1L, 2L, NULL))

  simplified <- read_yaml(path, simplify = TRUE)
  expect_identical(simplified, c(1L, 2L, NA))
})

test_that("read_yaml omits yaml_keys for plain string mapping keys", {
  path <- tempfile("yaml12-keys-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("alpha: 1", "beta: true"), path)
  parsed <- read_yaml(path, simplify = FALSE)

  expect_null(attr(parsed, "yaml_keys", exact = TRUE))
  expect_identical(names(parsed), c("alpha", "beta"))
  expect_identical(parsed$alpha, 1L)
  expect_identical(parsed$beta, TRUE)
})

test_that("write_yaml emits empty multi-document streams", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  docs <- list()

  expect_identical(write_yaml(docs, path, multi = TRUE), docs)
  expect_identical(readChar(path, file.info(path)$size), "---\n...\n")

  expect_identical(read_yaml(path, multi = TRUE), list(NULL))
  expect_identical(read_yaml(path, multi = TRUE, simplify = TRUE), list(NULL))

  output <- paste0(
    capture.output(write_yaml(docs, multi = TRUE)),
    collapse = "\n"
  )
  expect_identical(output, "---\n...")
})

test_that("write_yaml flushes a final newline for files", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  value <- list(foo = 1L)

  write_yaml(value, path)
  expect_silent(readLines(path))
  expect_identical(read_yaml(path), value)
})

test_that("format_yaml multi-doc output stays stable", {
  docs <- list(list(foo = 1L), list(bar = list(2L, NULL)))
  expect_identical(
    parse_yaml(format_yaml(docs, multi = TRUE), multi = TRUE, simplify = FALSE),
    docs
  )
  expect_snapshot(format_yaml(docs, multi = TRUE))
})

test_that("write_yaml snapshot aids emitter regressions", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  multilines <- list(tail = "line1\nline2\n")
  write_yaml(multilines, path)
  expect_identical(read_yaml(path), multilines)
  expect_snapshot(readChar(path, file.info(path)$size))
})

test_that("read_yaml errors clearly when the file cannot be read", {
  path <- tempfile("yaml12-missing-", fileext = ".yaml")
  expect_error(read_yaml(path), "Failed to read")
})

test_that("read_yaml does not simplify mixed-type sequences", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("- true", "- 1"), path)
  result <- read_yaml(path)

  expect_type(result, "list")
  expect_identical(result, list(TRUE, 1L))

  simplified <- read_yaml(path, simplify = TRUE)
  expect_type(simplified, "list")
  expect_identical(simplified, list(TRUE, 1L))

  expect_identical(read_yaml(path, simplify = FALSE), list(TRUE, 1L))
})

test_that("read_yaml keeps tagged sequence elements as list values", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("- !foo 1", "- 2"), path)
  result <- read_yaml(path)
  first_tag <- attr(result[[1]], "yaml_tag")

  expect_type(result, "list")
  expect_type(first_tag, "character")
  expect_length(first_tag, 1L)
  expect_false(identical(first_tag, ""))
})

test_that("read_yaml applies handlers to tagged values", {
  path <- tempfile("yaml12-handlers-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines(c("foo: !double 21", "bar: !upper baz"), path)

  handlers <- list(
    "!double" = function(x) as.integer(x) * 2L,
    "!upper" = function(x) toupper(x)
  )

  expect_identical(
    read_yaml(path, handlers = handlers),
    list(foo = 42L, bar = "BAZ")
  )
})

test_that("read_yaml handler errors propagate", {
  path <- tempfile("yaml12-handlers-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  writeLines("foo: !err value", path)

  expect_error(
    read_yaml(path, handlers = list("!err" = function(x) stop("handler oops"))),
    "handler oops"
  )
})

test_that("read_yaml errors clearly on non-UTF-8 input", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  writeBin(as.raw(c(0x61, 0xe9, 0x0a)), "latin1.yaml")

  expect_snapshot(error = TRUE, read_yaml("latin1.yaml"))
  expect_snapshot(error = TRUE, read_yaml("latin1.yaml", multi = TRUE))
})
