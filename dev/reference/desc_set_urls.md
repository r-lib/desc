# Set the URL field in DESCRIPTION

The specified urls replace the current ones. The URL field is created if
it does not exist currently.

## Usage

``` r
desc_set_urls(urls, file = ".", normalize = FALSE)
```

## Arguments

- urls:

  A character vector of urls to set.

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
