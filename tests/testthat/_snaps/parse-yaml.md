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

