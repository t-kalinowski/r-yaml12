test_that("write_yaml writes and read_yaml reads single documents", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  value <- list(alpha = 1L, nested = list(TRUE, NULL))
  out <- write_yaml(value, path)

  expect_null(out)
  expect_true(file.exists(path))
  expect_identical(read_yaml(path), value)
})

test_that("write_yaml and read_yaml handle multi-document streams", {
  path <- tempfile("yaml12-", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  docs <- list(list(foo = 1L), list(bar = list(2L, NULL)))
  write_yaml(docs, path, multi = TRUE)

  expect_identical(read_yaml(path, multi = TRUE), docs)
})

test_that("read_yaml errors clearly when the file cannot be read", {
  path <- tempfile("yaml12-missing-", fileext = ".yaml")
  expect_error(read_yaml(path), "Failed to read")
})
