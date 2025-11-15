test_that("parse_yaml handles scalars", {
  expect_identical(parse_yaml("null"), NULL)
  expect_identical(parse_yaml("123"), 123L)
  expect_identical(parse_yaml("true"), TRUE)
  expect_identical(parse_yaml("hello"), "hello")
})

test_that("parse_yaml handles simple sequences and mappings", {
  expect_identical(
    parse_yaml("- a\n- b\n- c"),
    list("a", "b", "c")
  )

  expect_identical(
    parse_yaml("foo: 1\nbar: baz"),
    list(foo = 1L, bar = "baz")
  )

  expect_identical(
    parse_yaml(c("foo: 1", "bar: 2")),
    list(foo = 1L, bar = 2L)
  )

  expect_error(parse_yaml(c("foo: 1", NA_character_)), "must not contain NA")
})

test_that("parse_yaml ignores additional documents", {
  yaml <- "---\nfoo: 1\n---\nbar: 2\n"
  expect_identical(parse_yaml(yaml), list(foo = 1L))
})

test_that("parse_yaml handles trailing newlines", {
  expect_identical(parse_yaml("foo: 1\n"), list(foo = 1L))
})
