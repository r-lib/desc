# Remove all dependencies from DESCRIPTION

Remove all dependencies from DESCRIPTION

## Usage

``` r
desc_del_deps(file = ".", normalize = FALSE)
```

## Arguments

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

## See also

Other dependencies:
[`desc_del_dep()`](https://desc.r-lib.org/dev/reference/desc_del_dep.md),
[`desc_get_deps()`](https://desc.r-lib.org/dev/reference/desc_get_deps.md),
[`desc_has_dep()`](https://desc.r-lib.org/dev/reference/desc_has_dep.md),
[`desc_set_dep()`](https://desc.r-lib.org/dev/reference/desc_set_dep.md),
[`desc_set_deps()`](https://desc.r-lib.org/dev/reference/desc_set_deps.md)
