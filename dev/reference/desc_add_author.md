# Add an author to Authors@R in DESCRIPTION

Add an author to Authors@R in DESCRIPTION

## Usage

``` r
desc_add_author(
  given = NULL,
  family = NULL,
  email = NULL,
  role = NULL,
  comment = NULL,
  orcid = NULL,
  ror = NULL,
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

- role:

  Role.

- comment:

  Comment.

- orcid:

  ORCID.

- ror:

  ROR ID.

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

Other Authors@R:
[`desc_add_author_gh()`](https://desc.r-lib.org/dev/reference/desc_add_author_gh.md),
[`desc_add_me()`](https://desc.r-lib.org/dev/reference/desc_add_me.md),
[`desc_add_orcid()`](https://desc.r-lib.org/dev/reference/desc_add_orcid.md),
[`desc_add_role()`](https://desc.r-lib.org/dev/reference/desc_add_role.md),
[`desc_add_ror()`](https://desc.r-lib.org/dev/reference/desc_add_ror.md),
[`desc_change_maintainer()`](https://desc.r-lib.org/dev/reference/desc_change_maintainer.md),
[`desc_coerce_authors_at_r()`](https://desc.r-lib.org/dev/reference/desc_coerce_authors_at_r.md),
[`desc_del_author()`](https://desc.r-lib.org/dev/reference/desc_del_author.md),
[`desc_del_role()`](https://desc.r-lib.org/dev/reference/desc_del_role.md),
[`desc_get_author()`](https://desc.r-lib.org/dev/reference/desc_get_author.md),
[`desc_get_authors()`](https://desc.r-lib.org/dev/reference/desc_get_authors.md),
[`desc_get_maintainer()`](https://desc.r-lib.org/dev/reference/desc_get_maintainer.md),
[`desc_set_authors()`](https://desc.r-lib.org/dev/reference/desc_set_authors.md)
