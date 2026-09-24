# Coerce Author and Maintainer Fields to Authors@R

Convert the `Author` and `Maintainer` fields to `Authors@R`, which is
necessary for other functions such as
[`desc_get_authors()`](https://desc.r-lib.org/dev/reference/desc_get_authors.md).

## Usage

``` r
desc_coerce_authors_at_r(file = ".", normalize = FALSE)
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

## Details

If the `Authors@R` field does not exist, `desc_coerce_authors_at_r`
tries to parse the `Author` and `Maintainer` fields with
[`utils::as.person()`](https://rdrr.io/r/utils/person.html) and writes
them to the `Authors@R` field. Note that `Author` and `Maintainer` are
free-form fields, so parsing them may fail.

## See also

Other Authors@R:
[`desc_add_author()`](https://desc.r-lib.org/dev/reference/desc_add_author.md),
[`desc_add_author_gh()`](https://desc.r-lib.org/dev/reference/desc_add_author_gh.md),
[`desc_add_me()`](https://desc.r-lib.org/dev/reference/desc_add_me.md),
[`desc_add_orcid()`](https://desc.r-lib.org/dev/reference/desc_add_orcid.md),
[`desc_add_role()`](https://desc.r-lib.org/dev/reference/desc_add_role.md),
[`desc_add_ror()`](https://desc.r-lib.org/dev/reference/desc_add_ror.md),
[`desc_change_maintainer()`](https://desc.r-lib.org/dev/reference/desc_change_maintainer.md),
[`desc_del_author()`](https://desc.r-lib.org/dev/reference/desc_del_author.md),
[`desc_del_role()`](https://desc.r-lib.org/dev/reference/desc_del_role.md),
[`desc_get_author()`](https://desc.r-lib.org/dev/reference/desc_get_author.md),
[`desc_get_authors()`](https://desc.r-lib.org/dev/reference/desc_get_authors.md),
[`desc_get_maintainer()`](https://desc.r-lib.org/dev/reference/desc_get_maintainer.md),
[`desc_set_authors()`](https://desc.r-lib.org/dev/reference/desc_set_authors.md)
