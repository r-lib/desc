# Delete locations from the Remotes field in DESCRIPTION

All locations matching the specified pattern are deleted.

## Usage

``` r
desc_del_remotes(pattern, file = ".", normalize = FALSE)
```

## Arguments

- pattern:

  Perl-compatible regular expression, all locations matching this
  expression will be deleted.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).
