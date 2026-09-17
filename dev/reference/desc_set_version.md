# Set the package version in DESCRIPTION

Both `$set_version()` and `$bump_version()` use dots to separate the
version number components.

## Usage

``` r
desc_set_version(version, file = ".", normalize = FALSE)
```

## Arguments

- version:

  A string or a
  [base::package_version](https://rdrr.io/r/base/numeric_version.html)
  object.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## See also

Other version numbers:
[`desc_bump_version()`](https://desc.r-lib.org/dev/reference/desc_bump_version.md),
[`desc_get_version()`](https://desc.r-lib.org/dev/reference/desc_get_version.md)
