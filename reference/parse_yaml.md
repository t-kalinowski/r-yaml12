# Parse YAML 1.2 document(s) into base R structures.

`parse_yaml()` takes strings of YAML; `read_yaml()` reads from a file
path.

## Usage

``` r
parse_yaml(text, multi = FALSE, simplify = TRUE, handlers = NULL)

read_yaml(path, multi = FALSE, simplify = TRUE, handlers = NULL)
```

## Arguments

- text:

  Character vector; elements are concatenated with `"\n"`.

- multi:

  When `TRUE`, return a list containing all documents in the stream.

- simplify:

  When `FALSE`, keep YAML sequences as R lists instead of simplifying to
  atomic vectors.

- handlers:

  Named list of R functions with names corresponding to YAML tags;
  matching handlers transform tagged values.

- path:

  Scalar string path to a YAML file\`.

## Value

When `multi = FALSE`, returns a parsed R object for the first document.
When `multi = TRUE`, returns a list of parsed documents.

## Details

YAML tags without a corresponding `handler` are preserved in a
`yaml_tag` attribute. Mappings with keys that are not all simple scalar
strings are returned as a named list with a `yaml_keys` attribute.

## Examples

``` r
dput(parse_yaml("foo: [1, 2, 3]"))
#> list(foo = 1:3)

# homogeneous sequences simplify by default.
# YAML null maps to NA in otherwise homogeneous sequences.
dput(parse_yaml("foo: [1, 2, 3, null]"))
#> list(foo = c(1L, 2L, 3L, NA))

# mixed type sequence never simplify
dput(parse_yaml("[1, true, cat]"))
#> list(1L, TRUE, "cat")

# use `simplify=FALSE` to always return sequences as lists.
str(parse_yaml("foo: [1, 2, 3, null]", simplify = FALSE))
#> List of 1
#>  $ foo:List of 4
#>   ..$ : int 1
#>   ..$ : int 2
#>   ..$ : int 3
#>   ..$ : NULL

# Parse multiple documents when requested.
stream <- "
---
first: 1
---
second: 2
"
str(parse_yaml(stream, multi = TRUE))
#> List of 2
#>  $ :List of 1
#>   ..$ first: int 1
#>  $ :List of 1
#>   ..$ second: int 2

# Read from a file; keep sequences as lists.
path <- tempfile(fileext = ".yaml")
writeLines("alpha: [true, null]\nbeta: 3.5", path)
str(read_yaml(path, simplify = FALSE))
#> List of 2
#>  $ alpha:List of 2
#>   ..$ : logi TRUE
#>   ..$ : NULL
#>  $ beta : num 3.5
```
