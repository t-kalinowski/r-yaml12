# parse_yaml errors on NA strings regardless of position or length

    Code
      parse_yaml(NA_character_)
    Condition
      Error in `parse_yaml()`:
      ! `text` must not contain NA strings

---

    Code
      parse_yaml(c(NA_character_, "foo: 1"))
    Condition
      Error in `parse_yaml()`:
      ! `text` must not contain NA strings

---

    Code
      parse_yaml(c("foo: 1", NA_character_))
    Condition
      Error in `parse_yaml()`:
      ! `text` must not contain NA strings

---

    Code
      parse_yaml(NA)
    Condition
      Error in `parse_yaml()`:
      ! Expected Strings got Logicals

---

    Code
      parse_yaml(NA_integer_)
    Condition
      Error in `parse_yaml()`:
      ! Expected Strings got Integers

---

    Code
      parse_yaml(NA_real_)
    Condition
      Error in `parse_yaml()`:
      ! Expected Strings got Doubles

---

    Code
      parse_yaml(NA_complex_)
    Condition
      Error in `parse_yaml()`:
      ! Expected Strings got Complexes

---

    Code
      parse_yaml(c(NA_character_, NA_character_, "foo: 1"))
    Condition
      Error in `parse_yaml()`:
      ! `text` must not contain NA strings

---

    Code
      parse_yaml(c("foo: 1", "bar: 2", NA_character_))
    Condition
      Error in `parse_yaml()`:
      ! `text` must not contain NA strings

# parse_yaml mapping key tags respect simplify flag

    Code
      str(parse_yaml("!<tag:yaml.org,2002:str> foo: 1\n", simplify = TRUE))
    Output
      List of 1
       $ foo: int 1

---

    Code
      str(parse_yaml("!<tag:yaml.org,2002:str> foo: 1\n", simplify = FALSE))
    Output
      List of 1
       $ foo: int 1

# parse_yaml preserves non-core tags on mapping keys via yaml_keys

    Code
      str(parse_yaml("!custom foo: 1\n", simplify = TRUE))
    Output
      List of 1
       $ foo: int 1
       - attr(*, "yaml_keys")=List of 1
        ..$ : chr "foo"
        .. ..- attr(*, "yaml_tag")= chr "!custom"

---

    Code
      str(parse_yaml("!custom foo: 1\n", simplify = FALSE))
    Output
      List of 1
       $ foo: int 1
       - attr(*, "yaml_keys")=List of 1
        ..$ : chr "foo"
        .. ..- attr(*, "yaml_tag")= chr "!custom"

