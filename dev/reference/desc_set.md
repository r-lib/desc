# Set one or more fields in a DESCRIPTION file

Set one or more fields in a DESCRIPTION file

## Usage

``` r
desc_set(..., check = TRUE, file = ".", normalize = FALSE)

desc_set_list(key, list_value, sep = ", ", file = ".", normalize = FALSE)
```

## Arguments

- ...:

  Values to set, see details below.

- check:

  Whether to check the validity of the new fields.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).
  Note: if `Config/desc/tidy` is set to a truth value in the DESCRIPTION
  file, auto-tidy will occur regardless of this parameter.

- key:

  Key to set in `desc_set_list()`.

- list_value:

  Character vector, to collapse in `desc_set_list()`.

- sep:

  Separator string for `desc_set_list()` list fields.

## Details

`desc_set()` supports two forms, the first is two unnamed arguments: the
key and its value to set.

The second form requires named arguments: names are used as keys and
values as values to set.

`desc_set_list()` collapses a character vector into string, separating
the elements by commas.

## See also

Other simple queries:
[`desc_del()`](https://desc.r-lib.org/dev/reference/desc_del.md),
[`desc_fields()`](https://desc.r-lib.org/dev/reference/desc_fields.md),
[`desc_get()`](https://desc.r-lib.org/dev/reference/desc_get.md),
[`desc_get_field()`](https://desc.r-lib.org/dev/reference/desc_get_field.md),
[`desc_has_fields()`](https://desc.r-lib.org/dev/reference/desc_has_fields.md)
