# Query the built field in DESCRIPTION

If the file has no `Built` field then it throws an error.

## Usage

``` r
desc_get_built(file = ".", normalize = FALSE)
```

## Arguments

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

## Value

A list with fields `R`, `Platform`, `Date`, `OStype`.
