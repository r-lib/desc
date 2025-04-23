# get_field works

    Code
      desc$get_field("package")
    Condition
      Error in `val %||% default`:
      ! Field 'package' not found

# get_or_fail works

    Code
      desc$get_or_fail("package")
    Condition
      Error:
      ! Could not find DESCRIPTION field: 'package'.

---

    Code
      desc$get_or_fail(c("Package", "versionx"))
    Condition
      Error:
      ! Could not find DESCRIPTION field: 'versionx'.

---

    Code
      desc$get_or_fail(c("Package", "versionx", "foobar"))
    Condition
      Error:
      ! Could not find DESCRIPTION fields: 'versionx', 'foobar'.

# set errors on invalid input

    Code
      desc$set("foobar")
    Condition
      Error in `idesc_set()`:
      ! $set needs two unnamed args, or all named args, see docs

