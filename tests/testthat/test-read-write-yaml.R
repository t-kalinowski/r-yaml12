test_that("write_yaml writes and read_yaml reads single documents", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  value <- list(alpha = 1L, nested = c(TRUE, NA))
  out <- write_yaml(value, path)

  expect_identical(out, value)
  expect_true(file.exists(path))
  expect_identical(read_yaml(path), value)
  expect_identical(read_yaml(path, simplify = TRUE), value)
  expect_identical(
    read_yaml(path, simplify = FALSE),
    list(alpha = 1L, nested = list(TRUE, NULL))
  )
})

test_that("write_yaml defaults to R stdout when path is NULL", {
  value <- list(alpha = 1L, nested = c(TRUE, NA))

  output <- capture.output(out <- write_yaml(value))
  expect_identical(out, value)
  expect_identical(
    paste(output, collapse = "\n"),
    format_yaml(value)
  )
})

test_that("write_yaml and read_yaml handle multi-document streams", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  docs <- list(list(foo = 1L), list(bar = list(2L, NULL)))
  write_yaml(docs, path, multi = TRUE)

  docs[[2]]$bar <- c(2L, NA)
  expect_identical(read_yaml(path, multi = TRUE), docs)
  expect_identical(read_yaml(path, multi = TRUE, simplify = TRUE), docs)
  expect_identical(
    read_yaml(path, multi = TRUE, simplify = FALSE),
    list(list(foo = 1L), list(bar = list(2L, NULL)))
  )
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
