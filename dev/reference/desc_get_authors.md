# Query all authors in Authors@R, in DESCRIPTION

Query all authors in Authors@R, in DESCRIPTION

## Usage

``` r
desc_get_authors(file = ".")
```

## Arguments

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

A person object, see
[`utils::person()`](https://rdrr.io/r/utils/person.html).

## See also

Other Authors@R:
[`desc_add_author()`](https://desc.r-lib.org/dev/reference/desc_add_author.md),
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
[`desc_get_maintainer()`](https://desc.r-lib.org/dev/reference/desc_get_maintainer.md),
[`desc_set_authors()`](https://desc.r-lib.org/dev/reference/desc_set_authors.md)
