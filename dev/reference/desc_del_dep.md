# Remove a package dependency from DESCRIPTION

Remove a package dependency from DESCRIPTION

## Usage

``` r
desc_del_dep(
  package,
  type = c("all", desc::dep_types),
  file = ".",
  normalize = FALSE
)
```

## Arguments

- package:

  Package dependency to remove.

- type:

  Dependency type to remove. Sometimes a package is depended on via
  multiple dependency types, e.g. `LinkingTo` and `Imports`. Defaults to
  all types.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## See also

Other dependencies:
[`desc_del_deps()`](https://desc.r-lib.org/dev/reference/desc_del_deps.md),
[`desc_get_deps()`](https://desc.r-lib.org/dev/reference/desc_get_deps.md),
[`desc_has_dep()`](https://desc.r-lib.org/dev/reference/desc_has_dep.md),
[`desc_set_dep()`](https://desc.r-lib.org/dev/reference/desc_set_dep.md),
[`desc_set_deps()`](https://desc.r-lib.org/dev/reference/desc_set_deps.md)
