test_that("format_yaml round-trips basic R lists", {
  obj <- list(
    foo = "bar",
    baz = list(TRUE, 123L),
    qux = list(sub = list("nested", NULL))
  )

  encoded <- format_yaml(obj)
  expect_type(encoded, "character")

  expected <- list(
    foo = "bar",
    baz = list(TRUE, 123L),
    qux = list(sub = c("nested", NA))
  )
  reparsed <- parse_yaml(encoded)
  expect_identical(reparsed, expected)

  expect_identical(parse_yaml(encoded, simplify = FALSE), obj)
})

test_that("format_yaml preserves yaml_tag attribute", {
  obj <- structure(
    list(
      scalar = structure("bar", yaml_tag = "!expr"),
      seq = structure(list(1L, 2L), yaml_tag = "!seq")
    ),
    yaml_tag = "!custom"
  )
  encoded <- format_yaml(obj)
  expect_true(grepl("!custom", encoded, fixed = TRUE))
  expect_true(grepl("!expr", encoded, fixed = TRUE))
  expect_true(grepl("!seq", encoded, fixed = TRUE))

  reparsed <- parse_yaml(encoded)
  expect_identical(attr(reparsed, "yaml_tag"), "!custom")
  expect_identical(attr(reparsed$scalar, "yaml_tag"), "!expr")
  expect_identical(attr(reparsed$seq, "yaml_tag"), "!seq")
})

test_that("format_yaml ignores yaml_tag attributes using core schema handle", {
  obj <- structure(
    list(
      scalar = structure("bar", yaml_tag = "!!seq"),
      seq = structure(list(1L, 2L), yaml_tag = "!!map")
    ),
    yaml_tag = "!custom"
  )

  encoded <- format_yaml(obj)
  expect_false(grepl("!!seq", encoded, fixed = TRUE))
  expect_false(grepl("!!map", encoded, fixed = TRUE))
  expect_false(grepl("!seq", encoded, fixed = TRUE))
  expect_false(grepl("!map", encoded, fixed = TRUE))
  expect_true(grepl("!custom", encoded, fixed = TRUE))

  reparsed <- parse_yaml(encoded)
  expect_null(attr(reparsed$scalar, "yaml_tag", exact = TRUE))
  expect_null(attr(reparsed$seq, "yaml_tag", exact = TRUE))
  expect_identical(attr(reparsed, "yaml_tag", exact = TRUE), "!custom")
})

test_that("format_yaml round-trips multi-document streams", {
  docs <- list(list(foo = 1L), list(bar = list(2L, NULL)))
  encoded <- format_yaml(docs, multi = TRUE)
  expect_true(startsWith(encoded, "---"))
  expect_true(grepl("\n---\n", encoded, fixed = TRUE))
  expect_true(grepl("\n$", encoded))
  parsed <- parse_yaml(encoded, multi = TRUE)
  docs[[2]]$bar <- c(2L, NA)
  expect_identical(parsed, docs)
})

test_that("format_yaml single-document output has no header or trailing newline", {
  encoded <- format_yaml(list(foo = 1L))
  expect_false(startsWith(encoded, "---"))
  expect_false(grepl("\n$", encoded))
})

test_that("format_yaml validates yaml_tag attribute shape", {
  tagged <- structure("value", yaml_tag = c("!a", "!b"))
  expect_error(
    format_yaml(tagged),
    "Invalid `yaml_tag` attribute: expected a single, non-missing string"
  )

  bad_type <- structure("value", yaml_tag = 1L)
  expect_error(
    format_yaml(bad_type),
    "Invalid `yaml_tag` attribute: expected a single, non-missing string"
  )
})

test_that("format_yaml errors clearly when multi = TRUE without a list", {
  expect_error(
    format_yaml(1L, multi = TRUE),
    "`value` must be a list when `multi = TRUE`",
    fixed = TRUE
  )
})

test_that("format_yaml respects yaml_keys attribute", {
  parsed <- parse_yaml(
    r"--(
1: a
true: b
null: c
3.5: d
)--"
  )

  encoded <- format_yaml(parsed)
  reparsed <- parse_yaml(encoded)
  expect_identical(reparsed, parsed)
})

test_that("format_yaml returns visibly", {
  expect_visible(format_yaml(list(answer = 42L)))
  out <- format_yaml(list(answer = 42L))
  expect_true(is.character(out) && length(out) == 1)
})

test_that("format_yaml preserves single-length collections", {
  seq_out <- format_yaml(list(list(1L)))
  reparsed_seq <- parse_yaml(seq_out)
  expect_identical(reparsed_seq, list(1L))

  map_out <- format_yaml(list(list(key = 1L)))
  reparsed_map <- parse_yaml(map_out)
  expect_identical(reparsed_map, list(list(key = 1L)))
})

test_that("format_yaml retains partial names as mapping keys", {
  obj <- list(a = 1L, 2L)
  encoded <- format_yaml(obj)
  reparsed <- parse_yaml(encoded)
  expect_named(reparsed, c("a", ""))
  expect_identical(reparsed[[1]], 1L)
  expect_identical(reparsed[[2]], 2L)
})

test_that("format_yaml converts NA names to null YAML keys", {
  obj <- list(a = 1L, b = 2L)
  names(obj)[2] <- NA_character_
  encoded <- format_yaml(obj)
  reparsed <- parse_yaml(encoded)
  expect_named(reparsed, c("a", ""))
  yaml_keys <- attr(reparsed, "yaml_keys", exact = TRUE)
  expect_identical(yaml_keys[[1]], "a")
  expect_null(yaml_keys[[2]])
})

test_that("format_yaml errors clearly on invalid yaml_tag", {
  missing <- structure("value", yaml_tag = NA_character_)
  expect_error(
    format_yaml(missing),
    "non-missing string.*Must not be NA"
  )

  malformed <- structure("value", yaml_tag = "!")
  expect_error(
    format_yaml(malformed),
    "Invalid YAML tag `!`",
    fixed = TRUE
  )
})
