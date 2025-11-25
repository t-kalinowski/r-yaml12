# Format or write R objects as YAML 1.2.

`format_yaml()` returns YAML as a character string. `write_yaml()`
writes a YAML stream to a file or stdout and always emits document start
(`---`) markers and a final end (`...`) marker. Both functions honor a
`yaml_tag` attribute on values (see examples).

## Usage

``` r
format_yaml(value, multi = FALSE)

write_yaml(value, path = NULL, multi = FALSE)
```

## Arguments

- value:

  Any R object composed of lists, atomic vectors, and scalars.

- multi:

  When `TRUE`, treat `value` as a list of YAML documents and encode a
  stream.

- path:

  Scalar string file path to write YAML to when using `write_yaml()`.
  When `NULL` (the default), write to R's standard output connection.

## Value

`format_yaml()` returns a scalar character string containing YAML.
`write_yaml()` invisibly returns `value`.

## Examples

``` r
cat(format_yaml(list(foo = 1, bar = list(TRUE, NA))))
#> foo: 1
#> bar:
#>   - true
#>   - ~

docs <- list("first", "second")
cat(format_yaml(docs, multi = TRUE))
#> ---
#> first
#> ---
#> second

tagged <- structure("1 + 1", yaml_tag = "!expr")
cat(tagged_yaml <- format_yaml(tagged), "\n")
#> !expr 1 + 1 

dput(parse_yaml(tagged_yaml))
#> structure("1 + 1", yaml_tag = "!expr")


write_yaml(list(foo = 1, bar = list(2, "baz")))
#> ---
#> foo: 1
#> bar:
#>   - 2
#>   - baz
#> ...

write_yaml(list("foo", "bar"), multi = TRUE)
#> ---
#> foo
#> ---
#> bar
#> ...

tagged <- structure("1 + 1", yaml_tag = "!expr")
write_yaml(tagged)
#> ---
#> !expr 1 + 1
#> ...
```
