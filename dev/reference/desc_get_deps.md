# List all package dependencies from a DESCRIPTION file

List all package dependencies from a DESCRIPTION file

## Usage

``` r
desc_get_deps(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

Data frame with columns: `type` (dependency type), `package`, and
`version`. For non-versioned dependencies `version` is `*`.

## See also

Other dependencies:
[`desc_del_dep()`](https://desc.r-lib.org/dev/reference/desc_del_dep.md),
[`desc_del_deps()`](https://desc.r-lib.org/dev/reference/desc_del_deps.md),
[`desc_has_dep()`](https://desc.r-lib.org/dev/reference/desc_has_dep.md),
[`desc_set_dep()`](https://desc.r-lib.org/dev/reference/desc_set_dep.md),
[`desc_set_deps()`](https://desc.r-lib.org/dev/reference/desc_set_deps.md)
