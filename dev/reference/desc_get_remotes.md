# List the locations in the Remotes field in DESCRIPTION

List the locations in the Remotes field in DESCRIPTION

## Usage

``` r
desc_get_remotes(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

A character vectors or remote locations. A length zero vector is
returned if there is no Remotes field in the package.
