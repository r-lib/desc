# Check if some fields are present in a DESCRIPTION file

Check if some fields are present in a DESCRIPTION file

## Usage

``` r
desc_has_fields(keys, file = ".")
```

## Arguments

- keys:

  Character vector of keys to check.

- file:

  DESCRIPTION file to use. By default the DESCRIPTION file of the
  current package (i.e. the package the working directory is part of) is
  used.

## Value

Logical vector.

## See also

Other simple queries:
[`desc_del()`](https://desc.r-lib.org/dev/reference/desc_del.md),
[`desc_fields()`](https://desc.r-lib.org/dev/reference/desc_fields.md),
[`desc_get()`](https://desc.r-lib.org/dev/reference/desc_get.md),
[`desc_get_field()`](https://desc.r-lib.org/dev/reference/desc_get_field.md),
[`desc_set()`](https://desc.r-lib.org/dev/reference/desc_set.md)
