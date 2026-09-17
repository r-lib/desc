# Check encoding of new or existing fields

If `new_fields` is `NULL`, then the existing fields are checked.
Otherwise `new_fields` are checked.

## Usage

``` r
check_encoding(self, private, new_fields)
```

## Arguments

- self:

  Object.

- private:

  Private env.

- new_fields:

  New fields, or `NULL` to check existing fields.

## Value

Object, invisibly.

## Details

Warnings are given for non-ascii fields, if the `Encoding` field is not
set.
