# has_dep

    Code
      desc$has_dep(123)
    Condition
      Error in `idesc_has_dep()`:
      ! is_string(package) is not TRUE

---

    Code
      desc$has_dep("testthat", "xxx")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "any", "Imports", "Depends", "Suggests", "Enhances", "LinkingTo"

