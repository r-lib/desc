# Check for a dependency

Check for a dependency

## Usage

``` r
desc_has_dep(package, type = c("any", desc::dep_types), file = ".")
```

## Arguments

- package:

  The package name.

- type:

  A dependency type or “any\`.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

A logical scalar.

## See also

Other dependencies:
[`desc_del_dep()`](https://desc.r-lib.org/dev/reference/desc_del_dep.md),
[`desc_del_deps()`](https://desc.r-lib.org/dev/reference/desc_del_deps.md),
[`desc_get_deps()`](https://desc.r-lib.org/dev/reference/desc_get_deps.md),
[`desc_set_dep()`](https://desc.r-lib.org/dev/reference/desc_set_dep.md),
[`desc_set_deps()`](https://desc.r-lib.org/dev/reference/desc_set_deps.md)
