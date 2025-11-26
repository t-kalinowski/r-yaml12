
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yaml12

<!-- badges: start -->

[![R-CMD-check](https://github.com/posit-dev/r-yaml12/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/posit-dev/r-yaml12/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

A YAML 1.2 parser/formatter for R, implemented in Rust for speed and
correctness. Built on the excellent
[`saphyr`](https://github.com/saphyr-rs/saphyr) crate.

## Installation

You can install the development version of yaml12 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("posit-dev/r-yaml12")
```

## Quick start

``` r
library(yaml12)

yaml <- "
title: A modern YAML parser and emitter written in Rust
properties: [fast, correct, safe, simple]
sequences:
  simplify: true
"

doc <- parse_yaml(yaml)
str(doc)
#> List of 3
#>  $ title     : chr "A modern YAML parser and emitter written in Rust"
#>  $ properties: chr [1:4] "fast" "correct" "safe" "simple"
#>  $ sequences :List of 1
#>   ..$ simplify: logi TRUE
```

### Reading and writing files

``` r
value_out <- list(alpha = 1L, nested = c(TRUE, NA))

write_yaml(value_out, "my.yaml")
value_in <- read_yaml("my.yaml")

stopifnot(identical(value_out, value_in))

# Multi-document streams
docs_out <- list(list(foo = 1L), list(bar = c(2L, NA)))

write_yaml(docs_out, "my-multi.yaml", multi = TRUE)
docs_in <- read_yaml("my-multi.yaml", multi = TRUE)

stopifnot(identical(docs_in, docs_out))
```

### Tag handlers

Handlers let you opt into custom behavior for tagged nodes while keeping
the default parser strict and safe.

``` r
yaml <- "
- !upper [rust, r]
- !expr 6 * 7
"

handlers <- list(
  "!expr"  = function(x) eval(str2lang(x), baseenv()),
  "!upper" = toupper
)

parse_yaml(yaml, handlers = handlers)
#> [[1]]
#> [1] "RUST" "R"
#>
#> [[2]]
#> [1] 42
```

### Formatting and round-tripping

``` r
obj <- list(
  seq = 1:2,
  map = list(key = "value"),
  tagged = structure("1 + 1", yaml_tag = "!expr")
)

yaml <- format_yaml(obj)
cat(yaml)
#> seq:
#>   - 1
#>   - 2
#> map:
#>   key: value
#> tagged: !expr 1 + 1

str(parse_yaml(yaml))  # tags preserved in `yaml_tag`
#> List of 3
#>  $ seq   : int [1:2] 1 2
#>  $ map   :List of 1
#>   ..$ key: chr "value"
#>  $ tagged: chr "1 + 1"
#>   ..- attr(*, "yaml_tag")= chr "!expr"
```

## Documentation

- `vignette("yaml-2-minute-intro", package = "yaml12")` for a quick
  primer.
- `vignette("yaml-tags-and-advanced-features", package = "yaml12")` for
  tags, handlers, anchors, and advanced YAML features.
