# Query the package version in DESCRIPTION

If the file has no `Version` field, or it is an invalid version string,
then it throws an error.

## Usage

``` r
desc_get_version(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

A [base::package_version](https://rdrr.io/r/base/numeric_version.html)
object.

## See also

Other version numbers:
[`desc_bump_version()`](https://desc.r-lib.org/dev/reference/desc_bump_version.md),
[`desc_set_version()`](https://desc.r-lib.org/dev/reference/desc_set_version.md)
