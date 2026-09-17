# Get a field from a DESCRIPTION file

Get a field from a DESCRIPTION file

## Usage

``` r
desc_get(keys, file = ".")
```

## Arguments

- keys:

  Character vector of fields to get.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

Character vector, values of the specified keys. Non-existing keys return
`NA`.

## See also

Other simple queries:
[`desc_del()`](https://desc.r-lib.org/dev/reference/desc_del.md),
[`desc_fields()`](https://desc.r-lib.org/dev/reference/desc_fields.md),
[`desc_get_field()`](https://desc.r-lib.org/dev/reference/desc_get_field.md),
[`desc_has_fields()`](https://desc.r-lib.org/dev/reference/desc_has_fields.md),
[`desc_set()`](https://desc.r-lib.org/dev/reference/desc_set.md)
