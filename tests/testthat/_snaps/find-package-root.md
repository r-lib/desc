# find_package_root errors

    Code
      find_package_root("file83b937011726")
    Condition
      Error in `find_package_root()`:
      ! Path does not exist: file83b937011726

---

    Code
      find_package_root("/")
    Condition
      Error in `find_package_root()`:
      ! Could not find R package in `/` or its parent directories.

