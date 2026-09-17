# Changelog

## desc (development version)

- New `add_ror()` method and
  [`desc_add_ror()`](https://desc.r-lib.org/dev/reference/desc_add_ror.md)
  functions make it possible to add ROR IDs to authors directly instead
  of via the `comment` argument. ([@maelle](https://github.com/maelle),
  [\#158](https://github.com/r-lib/desc/issues/158))

- All functions and methods managing possibly non individual authors
  (`add_author()`, `del_author()`, `add_role()`, `del_role()`,
  `search_for_author()`, etc.) gain a `ror` argument.
  ([@maelle](https://github.com/maelle),
  [\#158](https://github.com/r-lib/desc/issues/158))

- [`desc_get_built()`](https://desc.r-lib.org/dev/reference/desc_get_built.md)
  no longer fails if Built spans multiple lines
  ([\#145](https://github.com/r-lib/desc/issues/145),
  [@seankross](https://github.com/seankross)).

- An empty `Depends` field is now properly normalized and formatted.
  ([\#148](https://github.com/r-lib/desc/issues/148),
  [@kevinushey](https://github.com/kevinushey))

- [`desc_coerce_authors_at_r()`](https://desc.r-lib.org/dev/reference/desc_coerce_authors_at_r.md)
  now works correctly for authors with multiple given names.

## desc 1.4.3

CRAN release: 2023-12-10

- `$set()` and
  [`desc_set()`](https://desc.r-lib.org/dev/reference/desc_set.md) now
  can omit checks if `check = FALSE` is set.

## desc 1.4.2

CRAN release: 2022-09-08

- The `description$write()` method, and thus all `desc_*()` functions
  work correctly now on R 4.3.x for packages that declare a non-UTF-8
  encoding.

## desc 1.4.1

CRAN release: 2022-03-06

- The `$coerce_authors_at_r()` method now does a much better job at
  setting the authors’ roles
  ([\#114](https://github.com/r-lib/desc/issues/114),
  [@dpprdan](https://github.com/dpprdan)).

## desc 1.4.0

CRAN release: 2021-09-28

- DESCRIPTION objects created with the `!new` command now omit
  `LazyData: true` to match new CRAN checks
  ([\#105](https://github.com/r-lib/desc/issues/105),
  [@malcolmbarrett](https://github.com/malcolmbarrett))

- `description$write()` now writes out the file in the correct encoding
  ([\#109](https://github.com/r-lib/desc/issues/109)).

- `Authors@R` fields are now formatted differently when normalizing a
  DESCRIPTION file ([\#78](https://github.com/r-lib/desc/issues/78)).

- New `description$get_list()`, `description$set_list()` and
  corresponding
  [`desc_get_list()`](https://desc.r-lib.org/dev/reference/desc_get_field.md)
  and
  [`desc_set_list()`](https://desc.r-lib.org/dev/reference/desc_set.md)
  values to query and create comma separated fields
  ([\#86](https://github.com/r-lib/desc/issues/86)).

### Breaking change

- [`desc_get_field()`](https://desc.r-lib.org/dev/reference/desc_get_field.md)
  gains a boolean `squish_ws` parameter to normalize whitespace within
  the retrieved value. It defaults to the value of `trim_ws` (`TRUE` by
  default). Example with desc’s current DESCRIPTION:

  Old behaviour:

  ``` r
  > desc::desc_get_field("Description")
  [1] "... DESCRIPTION files.\n    It is intended for packages ..."
  ```

  New behaviour:

  ``` r
  > desc::desc_get_field("Description")
  [1] "... DESCRIPTION files. It is intended for packages ..."
  ```

  If you want the old behaviour, just set `squish_ws = FALSE`.
