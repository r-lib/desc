# Get a single field from a DESCRIPTION file, fail if not found

`desc_get_list()` parses a comma separated list into a character vector.

## Usage

``` r
desc_get_field(
  key,
  default = stop("Field '", key, "' not found"),
  trim_ws = TRUE,
  squish_ws = trim_ws,
  file = "."
)

desc_get_or_fail(keys, file = ".")

desc_get_list(
  key,
  default = stop("Field '", key, "' not found"),
  sep = ",",
  trim_ws = TRUE,
  squish_ws = trim_ws,
  file = "."
)
```

## Arguments

- key:

  The field to query.

- default:

  Value to return if `key` is not found. By default it throws an error.

- trim_ws:

  Whether to trim leading and trailing whitespace from the value.
  Defaults to `TRUE`.

- squish_ws:

  Whether to reduce repeated whitespace in the value. Defaults to
  `trim_ws`.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- keys:

  Character vector of fields to get.

- sep:

  Separator string for `desc_get_list()`.

## Value

Character string, the value of `key`, or `default` if `key` is not found
and `default` is specified.

## See also

Other simple queries:
[`desc_del()`](https://desc.r-lib.org/dev/reference/desc_del.md),
[`desc_fields()`](https://desc.r-lib.org/dev/reference/desc_fields.md),
[`desc_get()`](https://desc.r-lib.org/dev/reference/desc_get.md),
[`desc_has_fields()`](https://desc.r-lib.org/dev/reference/desc_has_fields.md),
[`desc_set()`](https://desc.r-lib.org/dev/reference/desc_set.md)
