
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yaml12

<!-- badges: start -->

<!-- badges: end -->

A YAML 1.2 parser/formatter for R, implemented in Rust for speed and
correctness.

## Installation

You can install the development version of yaml12 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("t-kalinowski/r-yaml12")
```

## Quick start

``` r
library(yaml12)

yaml <- "
title: A modern YAML parser written in Rust
properties: [fast, correct, stable]
sequences:
  simplify: true
"

doc <- parse_yaml(yaml)
str(doc)
#> List of 3
#>  $ title     : chr "A modern YAML parser written in Rust"
#>  $ properties: chr [1:3] "fast" "correct" "stable"
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
docs <- list(list(foo = 1), list(bar = c(2, NA)))
write_yaml(docs, "my-multi.yaml", multi = TRUE)
read_yaml("my-multi.yaml", multi = TRUE)
#> [[1]]
#> [[1]]$foo
#> [1] 1
#> 
#> 
#> [[2]]
#> [[2]]$bar
#> [1]  2 NA
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
