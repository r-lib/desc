# Add an ROR to one or more authors in Authors@R, in DESCRIPTION

The author(s) can be specified by a combination of the `given`,
`family`, `email`, `comment` and `role` fields. If multiple filters are
specified, then all must match to identify the author(s).

## Usage

``` r
desc_add_ror(
  ror,
  given = NULL,
  family = NULL,
  email = NULL,
  comment = NULL,
  role = NULL,
  file = ".",
  normalize = FALSE
)
```

## Arguments

- ror:

  ror to add.

- given:

  Given name to filter on. Regular expression.

- family:

  Family name to filter on. Regular expression.

- email:

  Email address to filter on. Regular expression.

- comment:

  Comment field to filter on. Regular expression.

- role:

  Role field to filter on.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## See also

Other Authors@R:
[`desc_add_author()`](https://desc.r-lib.org/dev/reference/desc_add_author.md),
[`desc_add_author_gh()`](https://desc.r-lib.org/dev/reference/desc_add_author_gh.md),
[`desc_add_me()`](https://desc.r-lib.org/dev/reference/desc_add_me.md),
[`desc_add_orcid()`](https://desc.r-lib.org/dev/reference/desc_add_orcid.md),
[`desc_add_role()`](https://desc.r-lib.org/dev/reference/desc_add_role.md),
[`desc_change_maintainer()`](https://desc.r-lib.org/dev/reference/desc_change_maintainer.md),
[`desc_coerce_authors_at_r()`](https://desc.r-lib.org/dev/reference/desc_coerce_authors_at_r.md),
[`desc_del_author()`](https://desc.r-lib.org/dev/reference/desc_del_author.md),
[`desc_del_role()`](https://desc.r-lib.org/dev/reference/desc_del_role.md),
[`desc_get_author()`](https://desc.r-lib.org/dev/reference/desc_get_author.md),
[`desc_get_authors()`](https://desc.r-lib.org/dev/reference/desc_get_authors.md),
[`desc_get_maintainer()`](https://desc.r-lib.org/dev/reference/desc_get_maintainer.md),
[`desc_set_authors()`](https://desc.r-lib.org/dev/reference/desc_set_authors.md)
