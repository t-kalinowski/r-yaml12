# YAML Tags, Anchors, and Advanced Features with yaml12

``` r
library(yaml12)
```

This vignette picks up where the “YAML in 2 Minutes” intro leaves off.
It shows what YAML tags are and how to work with them in yaml12 with tag
handlers. Along the way we also cover complex-valued keys and node
anchors, so you can work with any advanced YAML (version 1.2) you might
see in the wild.

## Tags in YAML and how yaml12 handles them

Tags annotate any YAML node with extra meaning. In YAML syntax a tag
always starts with `!`, and it appears before the node’s value; it is
not part of the scalar text itself.

*yaml12* attaches tags as a `yaml_tag` attribute (always a string). The
most common form is a simple local short tag that starts with `!`:

``` r
dput(parse_yaml("!some_tag some_value"))
#> structure("some_value", yaml_tag = "!some_tag")
```

The presence of a custom tag bypasses normal scalar node type inference;
the scalar is always returned as a string even when the content looks
like another type.

``` r
parse_yaml("! true")
#> [1] "true"
#> attr(,"yaml_tag")
#> [1] "!"
parse_yaml("true")
#> [1] TRUE
```

## Using handlers to transform tagged nodes while parsing

[`parse_yaml()`](https://posit-dev.github.io/r-yaml12/reference/parse_yaml.md)
and
[`read_yaml()`](https://posit-dev.github.io/r-yaml12/reference/parse_yaml.md)
accept `handlers`: a named list of functions whose names are YAML tag
strings. Handlers run on any matching tagged node. For tagged scalars,
the handler always receives a length-1 string; for tagged sequences or
mappings, it receives an R vector representing that node.

Here is an example of using a handler to evaluate `!expr` nodes.

``` r
handlers <- list(
  "!expr" = function(x) eval(str2lang(x), globalenv())
)
parse_yaml("!expr 1+1", handlers = handlers)
#> [1] 2
```

Any errors from a handler stop parsing:

``` r
parse_yaml("!expr stop('boom')", handlers = handlers)
#> Error in eval(str2lang(x), globalenv()): boom
```

Any tag without a matching handler is left preserved as `yaml_tag`
attribute, and handlers without matching tags are left unused.

``` r
handlers <- list(
  "!expr" = function(x) eval(str2lang(x), globalenv()),
  "!upper" = toupper,
  "!lower" = tolower # unused
)

str(parse_yaml(handlers = handlers, "
- !expr 1+1
- !upper r is awesome
- !note this tag has no handler
"))
#> List of 3
#>  $ : num 2
#>  $ : chr "R IS AWESOME"
#>  $ : chr "this tag has no handler"
#>   ..- attr(*, "yaml_tag")= chr "!note"
```

With a tagged sequence, the handler is called with an unnamed R list, or
an atomic vector if `simplify = TRUE` and all the sequence elements are
a common type. With tagged mappings the handler is called with a named R
list, potentially with a `yaml_keys` attribute (more on this in the next
section).

``` r
handlers <- list(
  "!some_seq_tag" = function(x) {
    stopifnot(identical(x, c("a", "b")))
    "some handled value"
  },
  "!some_map_tag" = function(x) {
    stopifnot(identical(x, list(key1 = 1L, key2 = 2L)))
    "some other handled value"
  }
)

yaml_tagged_containers <- "
- !some_seq_tag [a, b]
- !some_map_tag {key1: 1, key2: 2}
"

str(parse_yaml(yaml_tagged_containers, handlers = handlers))
#> List of 2
#>  $ : chr "some handled value"
#>  $ : chr "some other handled value"
```

Handlers make it easy to opt into powerful behaviors (like evaluating
`!expr` nodes) while keeping the default parser strict and safe.

### Post-process tags yourself

If you want more control, you can parse first without `handlers` and
then walk the result yourself. For example, you can process
`!expr`-tagged scalars yourself by walking the yaml nodes simply like
this:

``` r
eval_yaml_expr_nodes <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, eval_yaml_expr_nodes)
  } else if (identical(attr(x, "yaml_tag", TRUE), "!expr")) {
    x <- eval(str2lang(x), globalenv())
  }
  x
}

safe_loaded <- parse_yaml("!expr 1 + 1")
dput(safe_loaded)
#> structure("1 + 1", yaml_tag = "!expr")
eval_yaml_expr_nodes(safe_loaded)
#> [1] 2
```

## Mappings revisited: non-string keys and `yaml_keys`

In YAML, mapping keys do not have to be plain scalar strings; any
arbitrary YAML node can be a key: including other scalar types,
sequences, and even other mappings. For example, this is valid YAML even
though the key is a boolean:

``` yaml
true: true
```

When *yaml12* sees a mapping key that is not a untagged string scalar,
it keeps the original keys in a `yaml_keys` attribute next to the
values:

``` r
dput(parse_yaml("true: true"))
#> structure(list(TRUE), names = "", yaml_keys = list(TRUE))
```

For complex key values, YAML uses the explicit mapping-key indicator
`?`. A line starting with ? introduces the key node (of any type) of a
mapping, and the following line that starts with `:` holds its value:

``` yaml
? [a, b]
: value with a sequence key
? {x: 1, y: 2}
: value with a mapping key
```

In yaml12 you can see those keys via the `yaml_keys` attribute:

``` r
yaml <- "
true: true
? [a, b]
: tuple
? {x: 1, y: 2}
: map-key
"

str(parse_yaml(yaml))
#> List of 3
#>  $ : logi TRUE
#>  $ : chr "tuple"
#>  $ : chr "map-key"
#>  - attr(*, "yaml_keys")=List of 3
#>   ..$ : logi TRUE
#>   ..$ : chr [1:2] "a" "b"
#>   ..$ :List of 2
#>   .. ..$ x: int 1
#>   .. ..$ y: int 2
```

### Tagged mapping keys

If you supply handlers, they run on keys as well, so a handler can turn
tagged keys into friendly R names before `yaml_keys` needs to be
attached. If all the mapping keys resolve to bare scalar strings, then a
`yaml_keys` attribute is not attached.

``` r
handlers <- list(
  "!upper" = toupper,
  "!airport" = function(x) paste0("IATA:", toupper(x))
)

yaml_tagged_key <- "
!upper newyork: !airport jfk
!upper warsaw: !airport waw
"

str(parse_yaml(yaml_tagged_key, handlers = handlers))
#> List of 2
#>  $ NEWYORK: chr "IATA:JFK"
#>  $ WARSAW : chr "IATA:WAW"
```

If you anticipate tagged mapping keys that you want to process yourself,
you’ll need a bit more bookkeeping. The `yaml_keys` attribute is
materialized whenever any key is not a plain, untagged string scalar;
you’ll want to walk those keys alongside the values and optionally
collapse `yaml_keys` back to `NULL` if all keys become plain strings
after handling tagged nodes. For example, here is the earlier
`eval_yaml_expr_nodes` expanded to also handle tagged mapping keys.
(This expanded postprocessor is equivalent to passing
`handlers = list("!expr" = \(x) eval(str2lang(x), globalenv()))`)

``` r
is_bare_string <- \(x) {
  is.character(key) && length(key) == 1L && is.null(attributes(key))
}

eval_yaml_expr_nodes <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, eval_yaml_expr_nodes)

    if (!is.null(keys <- attr(x, "yaml_keys", TRUE))) {
      keys <- lapply(keys, eval_yaml_expr_nodes)
      names(x) <- sapply(
        \(name, key) if (name == "" && is_bare_string(key)) key else name,
        names(x),
        keys
      )
      attr(x, "yaml_keys") <-
        if (all(sapply(keys, is_bare_string))) NULL else keys
    }
  }
  if (identical(attr(x, "yaml_tag", TRUE), "!expr")) {
    x <- eval(str2lang(x), globalenv())
  }

  x
}
```

Because you control the traversal, you can add extra checks (for
example, only allowing expressions under certain mapping keys).

## Document Streams and Markers

Most YAML files contain a single YAML *document*. YAML also supports
*document streams*, where a file or string holds multiple YAML
documents. Documents are separated by a start marker (`---`) and may
optionally include an end marker (`...`).

### Reading Multiple Documents

For the reading functions
([`read_yaml()`](https://posit-dev.github.io/r-yaml12/reference/parse_yaml.md),
[`parse_yaml()`](https://posit-dev.github.io/r-yaml12/reference/parse_yaml.md)),
the `multi` argument defaults to `FALSE`. In this mode, only the first
YAML document is read. If an end marker (`...`) or a new start marker
(`---`) is encountered, the parser stops and returns only the first
document. When `multi = TRUE`, all documents in the stream are returned.

``` r
doc_stream <- "
---
doc 1
---
doc 2
"
parse_yaml(doc_stream)
#> [1] "doc 1"
parse_yaml(doc_stream, multi = TRUE)
#> [[1]]
#> [1] "doc 1"
#> 
#> [[2]]
#> [1] "doc 2"
```

### Writing Multiple Documents

For the writing functions
([`write_yaml()`](https://posit-dev.github.io/r-yaml12/reference/format_yaml.md),
[`format_yaml()`](https://posit-dev.github.io/r-yaml12/reference/format_yaml.md)),
`multi` also defaults to `FALSE`, producing a single YAML document. When
`multi = TRUE`, the provided R object is treated as a list of documents
and written as a YAML document stream, with documents separated by the
start marker `---`. Regardless of `multi`,
[`write_yaml()`](https://posit-dev.github.io/r-yaml12/reference/format_yaml.md)
always includes an initial start marker and a final end marker.

``` r
write_yaml(list("foo", "bar"))
#> ---
#> - foo
#> - bar
#> ...
write_yaml(list("foo", "bar"), multi = TRUE)
#> ---
#> foo
#> ---
#> bar
#> ...
```

When `multi = FALSE`, parsing stops after the first document—even if
later content is not valid YAML. That makes it easy to extract front
matter from files that mix YAML with other text (like R Markdown):

``` r
rmd_lines <- c(
  "---",
  "title: Front matter only",
  "params:",
  "  answer: 42",
  "---",
  "# Body that is not YAML"
)
parse_yaml(rmd_lines)
#> $title
#> [1] "Front matter only"
#> 
#> $params
#> $params$answer
#> [1] 42
```

Here the parser returns just the YAML frontmatter because the second
`---` technically ends the first *YAML document* in a *YAML document
stream*; with `multi = FALSE` the parser stops there and returns just
the first YAML document.

## Writing YAML with tags

To emit a tag, attach `yaml_tag` to an R value before calling
[`format_yaml()`](https://posit-dev.github.io/r-yaml12/reference/format_yaml.md)
or
[`write_yaml()`](https://posit-dev.github.io/r-yaml12/reference/format_yaml.md).

``` r
tagged <- structure("1 + x", yaml_tag = "!expr")
write_yaml(tagged)
#> ---
#> !expr 1 + x
#> ...
```

## Anchors

Anchors (`&id`) name a node; aliases (`*id`) copy it. yaml12 resolves
aliases before returning R objects.

``` r
str(parse_yaml("
recycle-me: &anchor-name
  a: b
  c: d

recycled:
  - *anchor-name
  - *anchor-name
"))
#> List of 2
#>  $ recycle-me:List of 2
#>   ..$ a: chr "b"
#>   ..$ c: chr "d"
#>  $ recycled  :List of 2
#>   ..$ :List of 2
#>   .. ..$ a: chr "b"
#>   .. ..$ c: chr "d"
#>   ..$ :List of 2
#>   .. ..$ a: chr "b"
#>   .. ..$ c: chr "d"
```

## (Very) Advanced Tags

The following are some YAML features that are rarely used, but are
supported for 100% compliance with the YAML 1.2 spec.

### Tag directives (`%TAG`)

YAML lets you declare tag handles at the top of a document with `%TAG`
directives. The syntax is `%TAG !<name>! <handle>`, and it applies to
the rest of the document. The `!name!` is then automatically expanded in
named tags:

``` yaml
%TAG !e! tag:example.com,2024:widgets/
---
item: !e!gizmo
```

Here the tag prefix `!e!` is automatically expanded to the full form
upon parsing.

``` r
dput(parse_yaml('
%TAG !e! tag:example.com,2024:widgets/
---
item: !e!gizmo foo
'))
#> list(item = structure("foo", yaml_tag = "tag:example.com,2024:widgets/gizmo"))
```

You can also declare a global tag prefix, which will expand a bare “!”

``` r
dput(parse_yaml('
%TAG ! tag:example.com,2024:widgets/
---
item: !gizmo foo
'))
#> list(item = structure("foo", yaml_tag = "tag:example.com,2024:widgets/gizmo"))
```

### TAG URIs

The above two forms are actually shorthands for resolving a tag “URI”.
You can bypass handle resolution by using the following tag syntax:
`!<...>` Anything in `...` will not be expanded, but must be a valid URI
(e.g., spaces must be escaped, like in a URL).

``` r
dput(parse_yaml('
%TAG ! tag:example.com,2024:widgets/
---
item: !<gizmo> foo
'))
#> list(item = structure("foo", yaml_tag = "gizmo"))
```

### Core schema tags

You may also encounter tags that start with two `!!`. This is a special
case of the `!name!suffix` tag syntax, where `name` is missing and
undefined and implicitly resolved to the YAML Core schema handle:
`tag:yaml.org,2002:`.

The following three tags all resolve to the same internal representation
and parse the same way:

``` r
'
- foo
- !!str foo
- !<tag:yaml.org,2002:str> foo
' |> parse_yaml() |> dput()
#> c("foo", "foo", "foo")
```

Core schema tags are generally unnecessary since all nodes are resolved
using the core schema already. However, they can be an alternative way
to declare node types. The valid set of core schema tags: `map`, `seq`,
`str`, `int`, `float`, `bool`, `null`.

Note that YAML 1.2 [removed](https://yaml.org/spec/1.2.2/ext/changes/)
some built-in types that were present in YAML 1.1.

> The !!pairs, !!omap, !!set, !!timestamp and !!binary types have been
> dropped.

Correspondingly, in *yaml12* these formerly core tags come into R as any
other unhandled tagged scalar, as strings with a `yaml_tag` attribute.
Note that the `pairs`, `omap`, and `set` are generally not meaningful in
R, since all the R objects returned are ordered (and *yaml12*
automatically preserves the order of mapping entries).

The [`!!timestamp`](https://yaml.org/type/timestamp.html) and
[`!!binary`](https://yaml.org/type/binary.html) tags are occasionally
useful, but the logic for handing richer types is encouraged to live at
the application level, and not in the core schema.

Note that with `!!`, the parser expands the first global prefix to
`tag:yaml.org,2002:` (unless a tag directive changed the meaning of
`!!`), and the tags come in with a fully resolved core schema URI.

``` r
yaml <- "
- !!timestamp 2025-01-01
- !!timestamp 2025-01-01 21:59:43.10 -5
- !!binary UiBpcyBBd2Vzb21l
"
str(parse_yaml(yaml))
#> List of 3
#>  $ : chr "2025-01-01"
#>   ..- attr(*, "yaml_tag")= chr "tag:yaml.org,2002:timestamp"
#>  $ : chr "2025-01-01 21:59:43.10 -5"
#>   ..- attr(*, "yaml_tag")= chr "tag:yaml.org,2002:timestamp"
#>  $ : chr "UiBpcyBBd2Vzb21l"
#>   ..- attr(*, "yaml_tag")= chr "tag:yaml.org,2002:binary"
```

You can supply a handler for them if you want to convert them from a
character string to some other R object:

``` r
# Timestamp handler: Convert date-only into Date, otherwise try (some of) the
# YAML 1.1 spec valid timestamp formats as POSIX formats.
# return NA on failure.
timestamp_handler <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    return(as.Date(x))
  }
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M"
  )
  as.POSIXct(x, tryFormats = formats, optional = TRUE)
}

# Binary handler: decode Base64 into raw
binary_handler <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  jsonlite::base64_dec(gsub("[ \n]", "", x))
}
```

``` r
str(parse_yaml(yaml, handlers = list(
  "tag:yaml.org,2002:timestamp" = timestamp_handler,
  "tag:yaml.org,2002:binary" = binary_handler
)))
#> List of 3
#>  $ : Date[1:1], format: "2025-01-01"
#>  $ : POSIXct[1:1], format: "2025-01-01 21:59:43"
#>  $ : raw [1:12] 52 20 69 73 ...
```
