# Remove fields from a DESCRIPTION file

Remove fields from a DESCRIPTION file

## Usage

``` r
desc_del(keys, file = ".", normalize = FALSE)
```

## Arguments

- keys:

  Character vector of keys to remove.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## See also

Other simple queries:
[`desc_fields()`](https://desc.r-lib.org/dev/reference/desc_fields.md),
[`desc_get()`](https://desc.r-lib.org/dev/reference/desc_get.md),
[`desc_get_field()`](https://desc.r-lib.org/dev/reference/desc_get_field.md),
[`desc_has_fields()`](https://desc.r-lib.org/dev/reference/desc_has_fields.md),
[`desc_set()`](https://desc.r-lib.org/dev/reference/desc_set.md)
