# Add a GitHub user as an author to DESCRIPTION

Uses the Authors@R field.

## Usage

``` r
desc_add_author_gh(
  username,
  role = "ctb",
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)
```

## Arguments

- username:

  GitHub username of the GitHub user

- role:

  Role to set for the user, defaults to contributor.

- comment:

  Comment, empty by default.

- orcid:

  ORCID, empty by default.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

- normalize:

  Whether to "normalize" (reorder and reformat) the fields when writing
  back the result. See
  [`desc_normalize()`](https://desc.r-lib.org/dev/reference/desc_normalize.md).

## Details

`desc_add_author_gh` is a convenience function, it adds the GitHub user
as an author, and it needs the `gh` package to be installed. The full
name is parsed using `as.person` and collapsing the given name and the
family name in order to e.g. have the first and middle names together as
given name. This approach might be limited to some full name structures.

## See also

Other Authors@R:
[`desc_add_author()`](https://desc.r-lib.org/dev/reference/desc_add_author.md),
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
