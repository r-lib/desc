# Increase the version number in DESCRIPTION

The `which` parameter specifies which component to increase. It can be a
string referring to a component: `major`, `minor`, `patch` or `dev`, or
an integer scalar, for the latter components are counted from one, and
the beginning. I.e. component one is equivalent to `major`.

## Usage

``` r
desc_bump_version(which, file = ".", normalize = FALSE)
```

## Arguments

- which:

  Which component to increase. See details below.

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

If a component is bumped, then the ones after it are zeroed out.
Trailing zero components are omitted from the new version number, but if
the old version number had at least two or three components, then the
one will also have two or three.

The bumping of the `dev` version (the fourth component) is special: if
the original version number had less than four components, and the `dev`
version is bumped, then it is set to `9000` instead of `1`. This is a
convention often used by R developers, it was originally invented by
Winston Chang.

Both `$set_version()` and `$bump_version()` use dots to separate the
version number components.

## See also

Other version numbers:
[`desc_get_version()`](https://desc.r-lib.org/dev/reference/desc_get_version.md),
[`desc_set_version()`](https://desc.r-lib.org/dev/reference/desc_set_version.md)
