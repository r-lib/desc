# Read, write, update, validate DESCRIPTION files

Read, write, update, validate DESCRIPTION files

## Format

An R6 class.

## Constructors

There are two ways of creating a description object. The first is
reading an already existing `DESCRIPTION` file; simply give the name of
the file as an argument. The default is `DESCRIPTION`:

    x <- description$new()
    x2 <- description$new("path/to/DESCRIPTION")

The second way is creating a description object from scratch, supply
`"!new"` as an argument to do this.

    x3 <- description$new("!new")

The complete API reference:

    description$new(cmd = NULL, file = NULL, text = NULL,
        package = NULL)

- `cmd`: a command to create a description from scratch. Currently only
  `"!new"` is implemented. If it does not start with an exclamation
  mark, it will be interpreted as a `file` argument.

- `file`: name of the `DESCRIPTION` file to load. If it is a directory,
  then we assume that it is inside an R package and conduct a search for
  the package root directory, i.e. the first directory up the tree that
  contains a `DESCRIPTION` file. If `cmd`, `file`, `text` and `package`
  are all `NULL` (the default), then the search is started from the
  working directory. The file can also be an R package (source, or
  binary), in which case the `DESCRIPTION` file is extracted from it,
  but note that in this case `$write()` cannot write the file back in
  the package archive.

- `text`: a character scalar containing the full DESCRIPTION, or a named
  character vector with names as DESCRIPTION field names and values as
  field values. Unnamed character vectors are collapsed into a character
  scalar, with newline as the separator.

- `package`: if not NULL, then the name of an installed package and the
  DESCRIPTION file of this package will be loaded.

## Setting and Querying fields

Set a field with `$set` and query it with `$get`:

    x <- description$new("!new")
    x$get("Package")
    x$set("Package", "foobar")
    x$set(Title = "Example Package for 'description'")
    x$get("Package")

Note that `$set` has two forms. You can either give the field name and
new value as two arguments; or you can use a single named argument, the
argument name is the field name, the argument value is the field value.

The `$fields` method simply lists the fields in the object:

    x$fields()

The `$has_fields` method checks if one or multiple fields are present in
a description object:

    x$has_fields("Package")
    x$has_fields(c("Title", "foobar"))

The `$del` method removes the specified fields:

    x$set(foo = "bar")
    x$del("foo")

`$get_field` is similar to `$get`, but it queries a single field, it
returns an unnamed vector if found, and returns the specified `default`
value if not. By default it throws an error if the field is not found.

The complete API reference:

    description$get(keys)
    description$get_field(key, default, trim_ws = TRUE, squish_ws = trim_ws)
    description$set(..., check = TRUE)
    description$fields()
    description$has_fields(keys)
    description$del(keys)

- `key`: a character string (length one), the key to query.

- `default`: If specified and `key` is missing, this value is returned.
  If not specified, an error is thrown.

- `trim_ws`: whether to trim leading and trailing whitespace from the
  returned value.

- `squish_ws`: whether to reduce repeated whitespace in the returned
  value.

- `keys`: a character vector of keys to query, check or delete.

- `...`: this must be either two unnamed arguments, the key and and the
  value to set; or an arbitrary number of named arguments, names are
  used as keys, values as values to set.

- `check`: A logical scalar. Whether to check the validity of the new
  values.

## Normalizing

Format DESCRIPTION in a standard way. `$str` formats each field in a
standard way and returns them (it does not change the object itself),
`$print` is used to print it to the screen. The `$normalize` function
normalizes each field (i.e. it changes the object). Normalization means
reformatting the fields, via `$reformat_fields()` and also reordering
them via `$reorder_fields()`. The format of the various fields is
opinionated and you might like it or not. Note that `desc` only
re-formats fields that it updates, and only on demand, so if your
formatting preferences differ, you can still manually edit `DESCRIPTION`
and `desc` will respect your edits.

    description$str(by_field = FALSE, normalize = TRUE,
        mode = c("file", "screen"))
    description$normalize()
    description$reformat_fields()
    description$reorder_fields()
    description$print()

- `by_field`: whether to return the normalized format by field, or
  collapsed into a character scalar.

- `normalize`: whether to reorder and reformat the fields.

- `mode`: `file` mode formats the fields as they are written to a file
  with the `write` method. `screen` mode adds extra markup to some
  fields, e.g. formats the `Authors@R` field in a readable way.

## Writing it to file

The `$write` method writes the description to a file. By default it
writes it to the file it was created from, if it was created from a
file. Otherwise giving a file name is compulsory:

    x$write(file = "DESCRIPTION")

The API:

    description$write(file = NULL)

- `file`: path to write the description to. If it was created from a
  file in the first place, then it is written to the same file.
  Otherwise this argument must be specified.

**Auto-tidy**: If `Config/desc/tidy` is set to a truth value (such as
`"true"`, `"yes"`, or `"1"`), the description will be automatically
normalized (tidied) before writing. This includes reordering and
reformatting fields according to standard conventions.

## Version numbers

    description$get_version()
    description$set_version(version)
    description$bump_version(which = c("patch", "minor", "major", "dev"))

- `version`: a string or a
  [base::package_version](https://rdrr.io/r/base/numeric_version.html)
  object.

- `which`: which component of the version number to increase. See
  details just below.

These functions are simple helpers to make it easier to query, set and
increase the version number of a package.

`$get_version()` returns the version number as a
[base::package_version](https://rdrr.io/r/base/numeric_version.html)
object. It throws an error if the package does not have a `Version`
field.

`$set_version()` takes a string or a
[base::package_version](https://rdrr.io/r/base/numeric_version.html)
object and sets the `Version` field to it.

`$bump_version()` increases the version number. The `which` parameter
specifies which component to increase. It can be a string referring to a
component: `major`, `minor`, `patch` or `dev`, or an integer scalar, for
the latter components are counted from one, and the beginning. I.e.
component one is equivalent to `major`.

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

## Dependencies

These functions handle the fields that define how the R package uses
another R packages. See
[dep_types](https://desc.r-lib.org/dev/reference/dep_types.md) for the
list of fields in this group.

The `$get_deps` method returns all declared dependencies, in a data
frame with columns: `type`, `package` and `version`. `type` is the name
of the dependency field, `package` is the name of the R package, and
`version` is the required version. If no specific versions are required,
then this is a `"*"`.

The `$set_deps` method is the opposite of `$get_deps` and it sets all
dependencies. The input is a data frame, with the same structure as the
return value of `$get_deps`.

The `$has_dep` method checks if a package is included in the
dependencies. It returns a logical scalar. If `type` is not `any`, then
it has to match as well.

The `$del_deps` method removes all declared dependencies.

The `$set_dep` method adds or updates a single dependency. By default it
adds the package to the `Imports` field.

The API:

    description$set_dep(package, type = dep_types, version = "*")
    description$set_deps(deps)
    description$get_deps()
    description$has_dep(package, type = c("any", dep_types))
    description$del_dep(package, type = c("all", dep_types))
    description$del_deps()

- `package`: name of the package to add to or remove from the
  dependencies.

- `type`: dependency type, see
  [dep_types](https://desc.r-lib.org/dev/reference/dep_types.md). For
  `$del_dep` it may also be `"all"`, and then the package will be
  deleted from all dependency types.

- `version`: required version. Defaults to `"*"`, which means no
  explicit version requirements.

- `deps`: a data frame with columns `type`, `package` and `version`.
  `$get_deps` returns the same format.

## Collate fields

Collate fields contain lists of file names with R source code, and the
package has a separate API for them. In brief, you can use
`$add_to_collate` to add one or more files to the main or other collate
field. You can use `$del_from_collate` to remove it from there.

The API:

    description$set_collate(files, which = c("main", "windows", "unix"))
    description$get_collate(which = c("main", "windows", "unix"))
    description$del_collate(which = c("all", "main", "windows", "unix"))
    description$add_to_collate(files, which = c("default", "all", "main",
      "windows", "unix"))
    description$del_from_collate(files, which = c("all", "main",
      "windows", "unix"))

- `iles`: the files to add or remove, in a character vector.

- `which: which collate field to manipulate. `"default"`for`\$add_to_collate\`
  means all existing collate fields, or the main one if none exist.

## Authors

There is a specialized API for the `Authors@R` field, to add and remove
authors, update their roles, change the maintainer, etc.

The API:

    description$get_authors()
    description$set_authors(authors)
    description$get_author(role)
    description$get_maintainer()
    description$coerce_authors_at_r()

- `authors`: a `person` object, a list of authors.

- `role`: The role to query. See `person` for details.

`$get_authors` returns a `person` object, the parsed authors. See
[`utils::person()`](https://rdrr.io/r/utils/person.html) for details.

`$get_author` returns a `person` object, all authors with the specified
role.

`$get_maintainer` returns the maintainer of the package. It works with
`Authors@R` fields and with traditional `Maintainer` fields as well.

`$coerce_authors_at_r` converts an `Author` field to one with a `person`
object. This coercion may be necessary for other functions such as
`$get_authors`.

    description$add_author(given = NULL, family = NULL, email = NULL,
        role = NULL, comment = NULL, orcid = NULL, ror = NULL)
    description$add_me(role = "ctb", comment = NULL, orcid = NULL)
    description$add_author_gh(username, role = "ctb", comment = NULL, orcid = NULL)

Add a new author. The arguments correspond to the arguments of the
[`utils::person()`](https://rdrr.io/r/utils/person.html) function.
`add_me` is a convenience function, it adds the current user as an
author, and it needs the `whoami` package to be installed. It'll add
your ORCID ID if you provide it as argument or save it as `ORCID_ID`
environment variable in .Renviron. The full name is parsed by `add_me`
and `add_author_gh` using `as.person` and collapsing the given name and
the family name in order to e.g. have the first and middle names
together as given name. This approach might be limited to some full name
structures.

    description$del_author(given = NULL, family = NULL, email = NULL,
        role = NULL, comment = NULL, orcid = NULL, ror = NULL)

Remove an author, or multiple authors. The author(s) to be removed can
be specified via any field(s). All authors matching all specifications
will be removed. E.g. if only `given = "Joe"` is supplied, then all
authors whole given name matches `Joe` will be removed. The
specifications can be (PCRE) regular expressions.

    description$add_role(role, given = NULL, family = NULL, email = NULL,
        comment = NULL, orcid = NULL, ror = NULL)
    description$add_orcid(orcid, given = NULL, family = NULL, email = NULL,
        comment = NULL, role = NULL)
    description$add_ror(ror, given = NULL, family = NULL, email = NULL,
        comment = NULL, role = NULL)
    description$del_role(role, given = NULL, family = NULL, email = NULL,
        comment = NULL, orcid = NULL, ror = NULL)
    description$change_maintainer(given = NULL, family = NULL,
        email = NULL, comment = NULL, orcid = NULL, ror = NULL)

`role` is the role to add or delete. The other arguments are used to
select a subset of the authors, on which the operation is performed,
similarly to `$del_author`.

## URLs

We provide helper functions for manipulating URLs in the `URL` field:

    description$get_urls()
    description$set_urls(urls)
    description$add_urls(urls)
    description$del_urls(pattern)
    description$clear_urls()

- `urls`: character vector of URLs to set or add.

- `pattern`: Perl compatible regular expression to specify the URLs to
  be removed.

`$get_urls()` returns all urls in a character vector. If no URL fields
are present, a zero length vector is returned.

`$set_urls()` sets the URL field to the URLs specified in the character
vector argument.

`$add_urls()` appends the specified URLs to the URL field. It creates
the field if it does not exists. Duplicate URLs are removed.

`$del_urls()` deletes the URLs that match the specified pattern.

`$clear_urls()` deletes all URLs.

## Remotes

`devtools`, `remotes` and some other packages support the non-standard
`Remotes` field in `DESCRIPTION`. This field can be used to specify
locations of dependent packages: GitHub or BitBucket repositories,
generic git repositories, etc. Please see the `Package remotes` vignette
in the `devtools` package.

`desc` has helper functions for manipulating the `Remotes` field:

    description$get_remotes()
    description$get_remotes()
    description$set_remotes(remotes)
    description$add_remotes(remotes)
    description$del_remotes(pattern)
    description$clear_remotes()

- `remotes`: character vector of remote dependency locations to set or
  add.

- `pattern`: Perl compatible regular expression to specify the remote
  dependency locations to remove.

`$get_remotes()` returns all remotes in a character vector. If no URL
fields are present, a zero length vector is returned.

`$set_remotes()` sets the URL field to the Remotes specified in the
character vector argument.

`$add_remotes()` appends the specified remotes to the `Remotes` field.
It creates the field if it does not exists. Duplicate remotes are
removed.

`$del_remotes()` deletes the remotes that match the specified pattern.

`$clear_remotes()` deletes all remotes.

## Config

Configuration fields (`Config/*`) provide package-specific settings that
control various behaviors. The `$get_config()` method provides access to
these configuration values with automatic boolean parsing.

    description$get_config(key)

- `key`: the configuration field to retrieve (e.g.,
  `"Config/desc/tidy"`).

`$get_config()` returns `TRUE` for values like `"true"`, `"TRUE"`,
`"yes"`, or `"1"`, and `FALSE` for values like `"false"`, `"FALSE"`,
`"no"`, `"0"`, invalid values, or missing fields.

**Auto-tidy configuration**: Setting `Config/desc/tidy` to a truth value
enables automatic normalization when writing DESCRIPTION files. This
ensures consistent formatting across all operations that modify and save
the description.

## Built

The `Built` field is used in binary packages to store information about
when and how a binary package was built.

`$get_built()` returns the built information as a list with fields `R`,
`Platform`, `Date`, `OStype`. It throws an error if the package does not
have a `Built` field.

## Encodings

When creating a `description` object, `desc` observes the `Encoding`
field, if present, and uses the specified encoding to parse the file.
Internally, it converts all fields to UTF-8.

When writing a `description` object to a file, `desc` uses the
`Encoding` field (if present), and converts all fields to the specified
encoding.

We suggest that whenever you need to use non-ASCII characters in your
package, you use the UTF-8 encoding, for maximum portability.

## Examples

``` r
## Create a template
desc <- description$new("!new")
desc
#> Package: {{ Package }}
#> Title: {{ Title }}
#> Version: 1.0.0
#> Authors@R (parsed):
#>     * Jo Doe <jodoe@dom.ain> [aut, cre]
#> Maintainer: {{ Maintainer }}
#> Description: {{ Description }}
#> License: {{ License }}
#> URL: {{ URL }}
#> BugReports: {{ BugReports }}
#> Encoding: UTF-8

## Read a file
desc2 <- description$new(file = system.file("DESCRIPTION",
                           package = "desc"))
desc2
#> Package: desc
#> Title: Manipulate DESCRIPTION Files
#> Version: 1.4.3.9000
#> Authors@R (parsed):
#>     * Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]
#>     * Kirill Müller [aut]
#>     * Jim Hester <james.f.hester@gmail.com> [aut]
#>     * Maëlle Salmon [ctb] (ORCID: <https://orcid.org/0000-0002-2815-0399>)
#>     * Posit Software, PBC [cph, fnd] (ROR: <https://ror.org/03wc8by49>)
#> Author: Gábor Csárdi [aut, cre], Kirill Müller [aut], Jim Hester
#>     [aut], Maëlle Salmon [ctb] (ORCID:
#>     <https://orcid.org/0000-0002-2815-0399>), Posit Software, PBC [cph,
#>     fnd] (ROR: <https://ror.org/03wc8by49>)
#> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
#> Description: Tools to read, write, create, and manipulate
#>     DESCRIPTION files.  It is intended for packages that create or
#>     manipulate other packages.
#> License: MIT + file LICENSE
#> URL: https://desc.r-lib.org/, https://github.com/r-lib/desc
#> BugReports: https://github.com/r-lib/desc/issues
#> Depends:
#>     R (>= 3.4)
#> Imports:
#>     R6,
#>     cli,
#>     utils
#> Suggests:
#>     callr,
#>     covr,
#>     gh,
#>     spelling,
#>     testthat,
#>     whoami,
#>     withr
#> Built: R 4.6.1; ; 2026-09-24 15:49:51 UTC; unix
#> Config/Needs/website: tidyverse/tidytemplate
#> Config/roxygen2/version: 8.1.0
#> Config/testthat/edition: 3
#> Config/usethis/last-upkeep: 2025-04-23
#> Encoding: UTF-8
#> Language: en-US
#> NeedsCompilation: no
#> Packaged: 2026-09-24 15:49:49 UTC; runner
#> RemotePkgRef: local::.
#> RemoteType: local
#> Roxygen: list(r6 = FALSE, load = "installed", markdown = TRUE)
#> Collate:
#>     'assertions.R'
#>     'authors-at-r.R'
#>     'built.R'
#>     'classes.R'
#>     'collate.R'
#>     'constants.R'
#>     'deps.R'
#>     'desc-package.R'
#>     'description.R'
#>     'encoding.R'
#>     'find-package-root.R'
#>     'latex.R'
#>     'non-oo-api.R'
#>     'package-archives.R'
#>     'read.R'
#>     'remotes.R'
#>     'str.R'
#>     'syntax_checks.R'
#>     'urls.R'
#>     'utils.R'
#>     'validate.R'
#>     'version.R'

## Remove a field
desc2$del("LazyData")

## Add another one
desc2$set(VignetteBuilder = "knitr")
desc2$get("VignetteBuilder")
#> VignetteBuilder 
#>         "knitr" 
desc2
#> Package: desc
#> Title: Manipulate DESCRIPTION Files
#> Version: 1.4.3.9000
#> Authors@R (parsed):
#>     * Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]
#>     * Kirill Müller [aut]
#>     * Jim Hester <james.f.hester@gmail.com> [aut]
#>     * Maëlle Salmon [ctb] (ORCID: <https://orcid.org/0000-0002-2815-0399>)
#>     * Posit Software, PBC [cph, fnd] (ROR: <https://ror.org/03wc8by49>)
#> Author: Gábor Csárdi [aut, cre], Kirill Müller [aut], Jim Hester
#>     [aut], Maëlle Salmon [ctb] (ORCID:
#>     <https://orcid.org/0000-0002-2815-0399>), Posit Software, PBC [cph,
#>     fnd] (ROR: <https://ror.org/03wc8by49>)
#> Maintainer: Gábor Csárdi <csardi.gabor@gmail.com>
#> Description: Tools to read, write, create, and manipulate
#>     DESCRIPTION files.  It is intended for packages that create or
#>     manipulate other packages.
#> License: MIT + file LICENSE
#> URL: https://desc.r-lib.org/, https://github.com/r-lib/desc
#> BugReports: https://github.com/r-lib/desc/issues
#> Depends:
#>     R (>= 3.4)
#> Imports:
#>     R6,
#>     cli,
#>     utils
#> Suggests:
#>     callr,
#>     covr,
#>     gh,
#>     spelling,
#>     testthat,
#>     whoami,
#>     withr
#> VignetteBuilder:
#>     knitr
#> Built: R 4.6.1; ; 2026-09-24 15:49:51 UTC; unix
#> Config/Needs/website: tidyverse/tidytemplate
#> Config/roxygen2/version: 8.1.0
#> Config/testthat/edition: 3
#> Config/usethis/last-upkeep: 2025-04-23
#> Encoding: UTF-8
#> Language: en-US
#> NeedsCompilation: no
#> Packaged: 2026-09-24 15:49:49 UTC; runner
#> RemotePkgRef: local::.
#> RemoteType: local
#> Roxygen: list(r6 = FALSE, load = "installed", markdown = TRUE)
#> Collate:
#>     'assertions.R'
#>     'authors-at-r.R'
#>     'built.R'
#>     'classes.R'
#>     'collate.R'
#>     'constants.R'
#>     'deps.R'
#>     'desc-package.R'
#>     'description.R'
#>     'encoding.R'
#>     'find-package-root.R'
#>     'latex.R'
#>     'non-oo-api.R'
#>     'package-archives.R'
#>     'read.R'
#>     'remotes.R'
#>     'str.R'
#>     'syntax_checks.R'
#>     'urls.R'
#>     'utils.R'
#>     'validate.R'
#>     'version.R'

## Enable auto-tidy for consistent formatting
desc3 <- description$new("!new")
desc3$set("Config/desc/tidy", "true")
desc3$set("Imports", "zzz, aaa, mmm")  # Will be auto-sorted when written
desc3$get_config("Config/desc/tidy")   # Returns TRUE
#> [1] TRUE
```
