# Remove files from the Collate field.

Remove files from the Collate field.

## Usage

``` r
desc_del_from_collate(
  files,
  which = c("all", "main", "windows", "unix"),
  file = ".",
  normalize = FALSE
)
```

## Arguments

- files:

  Files to remove from the Collate field.

- which:

  Which collate field to use. Collate fields can be operating system
  type specific.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## See also

Other Collate field:
[`desc_add_to_collate()`](https://desc.r-lib.org/dev/reference/desc_add_to_collate.md),
[`desc_del_collate()`](https://desc.r-lib.org/dev/reference/desc_del_collate.md),
[`desc_get_collate()`](https://desc.r-lib.org/dev/reference/desc_get_collate.md),
[`desc_set_collate()`](https://desc.r-lib.org/dev/reference/desc_set_collate.md)
