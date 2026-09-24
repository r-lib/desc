# Change maintainer of the package, in DESCRIPTION

Only works with the Authors@R field.

## Usage

``` r
desc_change_maintainer(
  given = NULL,
  family = NULL,
  email = NULL,
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)
```

## Arguments

- given:

  Given name.

- family:

  Family name.

- email:

  Email address.

- comment:

  Comment.

- orcid:

  ORCID.

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

The current maintainer is kept if they have at least another role.

## See also

Other Authors@R:
[`desc_add_author()`](https://desc.r-lib.org/dev/reference/desc_add_author.md),
[`desc_add_author_gh()`](https://desc.r-lib.org/dev/reference/desc_add_author_gh.md),
[`desc_add_me()`](https://desc.r-lib.org/dev/reference/desc_add_me.md),
[`desc_add_orcid()`](https://desc.r-lib.org/dev/reference/desc_add_orcid.md),
[`desc_add_role()`](https://desc.r-lib.org/dev/reference/desc_add_role.md),
[`desc_add_ror()`](https://desc.r-lib.org/dev/reference/desc_add_ror.md),
[`desc_coerce_authors_at_r()`](https://desc.r-lib.org/dev/reference/desc_coerce_authors_at_r.md),
[`desc_del_author()`](https://desc.r-lib.org/dev/reference/desc_del_author.md),
[`desc_del_role()`](https://desc.r-lib.org/dev/reference/desc_del_role.md),
[`desc_get_author()`](https://desc.r-lib.org/dev/reference/desc_get_author.md),
[`desc_get_authors()`](https://desc.r-lib.org/dev/reference/desc_get_authors.md),
[`desc_get_maintainer()`](https://desc.r-lib.org/dev/reference/desc_get_maintainer.md),
[`desc_set_authors()`](https://desc.r-lib.org/dev/reference/desc_set_authors.md)
