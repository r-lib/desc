# Set the Remotes field in DESCRIPTION

The specified locations replace the current ones. The Remotes field is
created if it does not exist currently.

## Usage

``` r
desc_set_remotes(remotes, file = ".", normalize = FALSE)
```

## Arguments

- remotes:

  A character vector of remote locations to set.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).
