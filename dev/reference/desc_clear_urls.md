# Remove all URLs from the URL field of DESCRIPTION

Remove all URLs from the URL field of DESCRIPTION

## Usage

``` r
desc_clear_urls(file = ".", normalize = FALSE)
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
