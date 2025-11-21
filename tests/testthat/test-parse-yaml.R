test_that("parse_yaml handles scalars", {
  expect_identical(parse_yaml("null"), NULL)
  expect_identical(parse_yaml("123"), 123L)
  expect_identical(parse_yaml("true"), TRUE)
  expect_identical(parse_yaml("hello"), "hello")
})

test_that("parse_yaml handles simple sequences and mappings", {
  simple_seq <- r"--(
- a
- b
- c
)--"

  expect_identical(
    parse_yaml(simple_seq),
    c("a", "b", "c")
  )

  expect_identical(
    parse_yaml(simple_seq, simplify = FALSE),
    list("a", "b", "c")
  )

  expect_identical(
    parse_yaml(simple_seq, simplify = TRUE),
    c("a", "b", "c")
  )

  expect_identical(
    parse_yaml(
      r"--(
foo: 1
bar: baz
)--"
    ),
    list(foo = 1L, bar = "baz")
  )

  expect_identical(
    parse_yaml(c("foo: 1", "bar: 2")),
    list(foo = 1L, bar = 2L)
  )

  expect_error(parse_yaml(c("foo: 1", NA_character_)), "must not contain NA")
})

test_that("parse_yaml handles multiple documents when requested", {
  yaml <- r"--(
---
foo: 1
---
bar: 2
)--"
  expect_identical(parse_yaml(yaml), list(foo = 1L))
  expect_identical(
    parse_yaml(yaml, multi = TRUE),
    list(list(foo = 1L), list(bar = 2L))
  )
})

test_that("parse_yaml ignores errors in later documents when multi = FALSE", {
  yaml <- r"--(
---
foo: 1
...
---
not: [valid
)--"
  expect_identical(parse_yaml(yaml), list(foo = 1L))
  expect_error(parse_yaml(yaml, multi = TRUE), "YAML parse error")
})

test_that("parse_yaml errors on NA strings regardless of position or length", {
  expect_snapshot(error = TRUE, parse_yaml(NA_character_))
  expect_snapshot(error = TRUE, parse_yaml(c(NA_character_, "foo: 1")))
  expect_snapshot(error = TRUE, parse_yaml(c("foo: 1", NA_character_)))
  expect_snapshot(error = TRUE, parse_yaml(NA))
  expect_snapshot(error = TRUE, parse_yaml(NA_integer_))
  expect_snapshot(error = TRUE, parse_yaml(NA_real_))
  expect_snapshot(error = TRUE, parse_yaml(NA_complex_))
  expect_identical(parse_yaml(character()), NULL)
  expect_snapshot(
    error = TRUE,
    parse_yaml(c(NA_character_, NA_character_, "foo: 1"))
  )
  expect_snapshot(
    error = TRUE,
    parse_yaml(c("foo: 1", "bar: 2", NA_character_))
  )
})

test_that("parse_yaml simplifies mixed numeric sequences", {
  yaml <- "[1, 2.5, 0x10, .inf, null]"

  simplified <- parse_yaml(yaml, simplify = TRUE)
  expect_type(simplified, "double")
  expect_identical(simplified, c(1, 2.5, 16, Inf, NA_real_))

  unsimplified <- parse_yaml(yaml, simplify = FALSE)
  expect_identical(unsimplified, list(1L, 2.5, 16L, Inf, NULL))
})

test_that("parse_yaml handles trailing newlines", {
  expect_identical(parse_yaml("foo: 1\n"), list(foo = 1L))
})

test_that("parse_yaml preserves YAML tags", {
  expect_identical(
    parse_yaml(r"--(!custom 3)--"),
    structure("3", yaml_tag = "!custom")
  )

  tagged <- parse_yaml(r"--(values: !seq [1, 2])--")
  expect_identical(tagged$values, structure(c(1L, 2L), yaml_tag = "!seq"))
})

test_that("parse_yaml applies handlers to tagged nodes", {
  handlers <- list(
    "!expr" = function(x) eval(str2lang(x), baseenv()),
    "!wrap" = function(x) list(value = x)
  )

  expect_identical(
    parse_yaml("foo: !expr 1+1", handlers = handlers),
    list(foo = 2)
  )

  expect_identical(
    parse_yaml("items: !wrap [a, b]", handlers = handlers),
    list(items = list(value = c("a", "b")))
  )
})

test_that("parse_yaml applies handlers to tagged mapping keys", {
  handlers <- list(
    "!upper" = function(x) toupper(x)
  )

  result <- parse_yaml("!upper key: value", handlers = handlers)
  expect_identical(result, list(KEY = "value"))
})

test_that("parse_yaml keeps original name when key handler returns non-string", {
  handlers <- list(
    "!meta" = function(x) list(label = toupper(x))
  )

  result <- parse_yaml("!meta key: value", handlers = handlers)
  expect_identical(names(result), "key")
  expect_true(!is.null(attr(result, "yaml_keys")))
  expect_identical(attr(result, "yaml_keys")[[1]], list(label = "KEY"))
})

test_that("parse_yaml applies handlers inside sequences before returning", {
  handlers <- list(
    "!double" = function(x) as.integer(x) * 2L
  )

  result <- parse_yaml("items: [!double 2, 3, !double 5]", handlers = handlers)
  expect_identical(result, list(items = list(4L, 3L, 10L)))
})

test_that("parse_yaml handler errors propagate", {
  expect_error(
    parse_yaml(
      "foo: !boom bar",
      handlers = list("!boom" = function(x) stop("boom"))
    ),
    "boom"
  )
})

test_that("parse_yaml validates handlers argument", {
  expect_error(parse_yaml("foo: !expr 1", handlers = 12), "named list")
  expect_error(
    parse_yaml("foo: !expr 1", handlers = list("!expr" = "not a function")),
    "must be a function"
  )
})

test_that("parse_yaml() warnings are catchable and respect options(warn)", {
  expect_no_warning(parse_yaml("!custom null"))
  expect_identical(
    parse_yaml("!custom null"),
    structure("null", yaml_tag = "!custom")
  )
  expect_no_warning(suppressWarnings(parse_yaml("!custom null")))
  expect_no_error(withr::with_options(
    list(warn = 2L),
    parse_yaml("!custom null")
  ))
  expect_no_warning(parse_yaml("!!null null"))
  expect_no_warning(parse_yaml("!<tag:yaml.org,2002:null> null"))
})

test_that("parse_yaml resolves all canonical null tag spellings", {
  canonical_cases <- c(
    "!!null ~",
    "!<!!null> ~",
    "!<tag:yaml.org,2002:null> ~"
  )
  for (yaml in canonical_cases) {
    parsed <- parse_yaml(yaml, simplify = FALSE)
    expect_identical(parsed, NULL)
    expect_null(attr(parsed, "yaml_tag", exact = TRUE))
  }

  informative <- parse_yaml("!<!null> ~", simplify = FALSE)
  expect_identical(informative, NULL)
  expect_null(attr(informative, "yaml_tag", exact = TRUE))

  custom <- parse_yaml("!null ~", simplify = FALSE)
  expect_identical(custom, structure("~", yaml_tag = "!null"))
})

test_that("parse_yaml errors clearly on invalid canonical tags", {
  expect_snapshot(error = TRUE, parse_yaml("!!int foo"))
  expect_snapshot(error = TRUE, parse_yaml("!!null foo"))
})

test_that("parse_yaml renders non-string mapping keys", {
  yaml <- r"--(
1: a
true: b
null: c
3.5: d
)--"
  result <- parse_yaml(yaml)

  expected <- structure(
    list("a", "b", "c", "d"),
    names = c("", "", "", ""),
    yaml_keys = list(1L, TRUE, NULL, 3.5)
  )

  expect_identical(result, expected)
})

test_that("parse_yaml stores non-string mapping keys in yaml_key attribute", {
  yaml <- r"--(
1: a
true: b
3.5: c
string: d
)--"
  result <- parse_yaml(yaml)

  expected <- structure(
    list("a", "b", "c", "d"),
    names = c("", "", "", "string"),
    yaml_keys = list(1L, TRUE, 3.5, "string")
  )

  expect_identical(result, expected)
})

test_that("parse_yaml mapping key tags respect simplify flag", {
  yaml <- "!<tag:yaml.org,2002:str> foo: 1\n"

  expected <- list(foo = 1L)

  expect_identical(parse_yaml(yaml, simplify = TRUE), expected)
  expect_identical(parse_yaml(yaml, simplify = FALSE), expected)

  expect_snapshot(str(parse_yaml(
    "!<tag:yaml.org,2002:str> foo: 1\n",
    simplify = TRUE
  )))
  expect_snapshot(str(parse_yaml(
    "!<tag:yaml.org,2002:str> foo: 1\n",
    simplify = FALSE
  )))
})

test_that("parse_yaml preserves non-core tags on mapping keys via yaml_keys", {
  yaml <- "!custom foo: 1\n"

  expected <- structure(
    list(1L),
    names = "foo",
    yaml_keys = list(structure("foo", yaml_tag = "!custom"))
  )

  simplified <- parse_yaml(yaml, simplify = TRUE)
  expect_identical(simplified, expected)

  unsimplified <- parse_yaml(yaml, simplify = FALSE)
  expect_identical(unsimplified, expected)

  encoded <- format_yaml(unsimplified)
  roundtrip <- parse_yaml(encoded, simplify = FALSE)
  expect_identical(roundtrip, expected)

  expect_snapshot(str(parse_yaml("!custom foo: 1\n", simplify = TRUE)))
  expect_snapshot(str(parse_yaml("!custom foo: 1\n", simplify = FALSE)))
})

test_that("parse_yaml does not set yaml_keys when all mapping keys are strings", {
  yaml <- r"--(
foo: 1
bar: 2
)--"

  result <- parse_yaml(yaml)
  expect_null(attr(result, "yaml_keys", exact = TRUE))
  expect_named(result, c("foo", "bar"))
})

test_that("parse_yaml yaml_keys align with positions when names are empty", {
  yaml <- r"--(
1: a
2: b
)--"
  result <- parse_yaml(yaml)

  expected <- structure(
    list("a", "b"),
    names = c("", ""),
    yaml_keys = list(1L, 2L)
  )

  expect_identical(result, expected)
})

test_that("parse_yaml returns visibly", {
  expect_visible(parse_yaml("answer: 42"))
  expect_identical(parse_yaml("answer: 42"), list(answer = 42L))
})

test_that("parse_yaml keeps sequences/mappings of length 1 as collections", {
  single_seq <- parse_yaml("- 1")
  expect_type(single_seq, "integer")
  expect_identical(single_seq, 1L)

  single_map <- parse_yaml("key: 1")
  expect_type(single_map, "list")
  expect_identical(single_map, list(key = 1L))
})

test_that("roundtrip newline in short string scalar", {
  og <- list(foo = "bar!\nbar!", baz = 42L)
  rt <- parse_yaml(format_yaml(og))
  expect_identical(og, rt)
})

test_that("parse_yaml resolves simple anchors and aliases", {
  yaml <- "a1: &DEFAULT\n  b1: 4\na2: *DEFAULT\n"

  parsed <- parse_yaml(yaml, simplify = FALSE)

  expect_identical(parsed$a1$b1, 4L)
  expect_identical(parsed$a2$b1, 4L)
})

test_that("parse_yaml resolves anchors and aliases inside sequences", {
  yaml <- r"--(
- &A 1
- 2
- *A
)--"

  simplified <- parse_yaml(yaml, simplify = TRUE)
  expect_identical(simplified, c(1L, 2L, 1L))

  unsimplified <- parse_yaml(yaml, simplify = FALSE)
  expect_identical(unsimplified, list(1L, 2L, 1L))
})
