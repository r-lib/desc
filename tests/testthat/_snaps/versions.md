# get_version

    Code
      desc$get_version()
    Condition
      Error in `idesc_get_version()`:
      ! No 'Version' field found

# set_version

    Code
      desc$set_version("1")
    Condition
      Error in `idesc_set_version()`:
      ! is_package_version(version) is not TRUE

---

    Code
      desc$set_version("1.0.0-dev")
    Condition
      Error in `idesc_set_version()`:
      ! is_package_version(version) is not TRUE

