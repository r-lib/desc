# Query the URL field in DESCRIPTION

Query the URL field in DESCRIPTION

## Usage

``` r
desc_get_urls(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

A character vectors or URLs. A length zero vector is returned if there
is no URL field in the package.
