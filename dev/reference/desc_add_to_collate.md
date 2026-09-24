# Add one or more files to the Collate field, in DESCRIPTION

Add one or more files to the Collate field, in DESCRIPTION

## Usage

``` r
desc_add_to_collate(
  files,
  which = c("default", "all", "main", "windows", "unix"),
  file = ".",
  normalize = FALSE
)
```

## Arguments

- files:

  Character vector, files to add.

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
  Note: if `Config/desc/tidy` is set to a truth value in the DESCRIPTION
  file, auto-tidy will occur regardless of this parameter.

## See also

Other Collate field:
[`desc_del_collate()`](https://desc.r-lib.org/dev/reference/desc_del_collate.md),
[`desc_del_from_collate()`](https://desc.r-lib.org/dev/reference/desc_del_from_collate.md),
[`desc_get_collate()`](https://desc.r-lib.org/dev/reference/desc_get_collate.md),
[`desc_set_collate()`](https://desc.r-lib.org/dev/reference/desc_set_collate.md)
