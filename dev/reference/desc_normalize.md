# Normalize a DESCRIPTION file

Re-formats and re-orders fields in DESCRIPTION in a standard way.
Reorders the packages alphabetically.

## Usage

``` r
desc_normalize(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## See also

Other repair functions:
[`desc_reformat_fields()`](https://desc.r-lib.org/dev/reference/desc_reformat_fields.md),
[`desc_reorder_fields()`](https://desc.r-lib.org/dev/reference/desc_reorder_fields.md)
