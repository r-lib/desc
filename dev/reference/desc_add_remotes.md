# Add locations in the Remotes field in DESCRIPTION

Add locations in the Remotes field in DESCRIPTION

## Usage

``` r
desc_add_remotes(remotes, file = ".", normalize = FALSE)
```

## Arguments

- remotes:

  Character vector of remote locations to add. Duplicate locations are
  eliminated. Note that existing locations are not updated, so if you
  want to *change* a remote location of a package, you need to delete
  the old location first and then add the new one.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).
