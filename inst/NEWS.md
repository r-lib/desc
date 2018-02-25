
# devel

* Use the `Encoding` field to read and write DESCRIPTION with the
  corect encoding. UTF-8 is always used internall by desc. (#52, #53)

* Add `get_built()` function to parse the Built field used in package
  binaries. (#48, @jimhester)

* `get_deps()` (and `desc_get_deps()`) return a zero-row data frame
  instead of `NULL` for packages without any dependencies, for consistency.

* Empty `DESCRIPTION` files are handled more gracefully, as are querying
  no fields with `desc_get()`

# 1.1.1

* Relax the R >= 3.2.0 dependency, R 3.1.0 is enough now.

# 1.1.0

* Fix bug when adding authors and there is no `Authors@R` field

* Get `DESCRIPTION` from package archives (#40)

* Fix but in `del_dep()` and `has_dep()`, they only worked if the package
  was attached.

# 1.0.1

* Fix formatting of `Collate` fields, they always start at a new line now.

* Fix formatting of `Authors@R` fields, when changed.

* Keep trailing space after the `:` character, see #14

# 1.0.0

First public release.
