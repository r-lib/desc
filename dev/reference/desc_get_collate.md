# Query the Collate field in DESCRIPTION

Query the Collate field in DESCRIPTION

## Usage

``` r
desc_get_collate(which = c("main", "windows", "unix"), file = ".")
```

## Arguments

- which:

  Which collate field to use. Collate fields can be operating system
  type specific.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

Character vector of file names.

## See also

Other Collate field:
[`desc_add_to_collate()`](https://desc.r-lib.org/dev/reference/desc_add_to_collate.md),
[`desc_del_collate()`](https://desc.r-lib.org/dev/reference/desc_del_collate.md),
[`desc_del_from_collate()`](https://desc.r-lib.org/dev/reference/desc_del_from_collate.md),
[`desc_set_collate()`](https://desc.r-lib.org/dev/reference/desc_set_collate.md)
