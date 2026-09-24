# Read a DESCRIPTION file

This is a convenience wrapper for `description$new()`. Very often you
want to read an existing `DESCRIPTION` file, and to do this you can just
supply the path to the file or its directory to `desc()`.

## Usage

``` r
desc(cmd = NULL, file = NULL, text = NULL, package = NULL)
```

## Arguments

- cmd:

  A command to create a description from scratch. Currently only
  `"!new"` is implemented. If it does not start with an exclamation
  mark, it will be interpreted as the `file` argument.

- file:

  Name of the `DESCRIPTION` file to load. If all of `cmd`, `file` and
  `text` are `NULL` (the default), then the `DESCRIPTION` file in the
  current working directory is used. The file can also be an R package
  (source, or binary), in which case the `DESCRIPTION` file is extracted
  from it, but note that in this case `$write()` cannot write the file
  back in the package archive.

- text:

  A character scalar containing the full DESCRIPTION, or a named
  character vector with names as DESCRIPTION field names and values as
  field values. Unnamed character vectors are collapsed into a character
  scalar, with newline as the separator.

- package:

  If not NULL, then the name of an installed package and the DESCRIPTION
  file of this package will be loaded.

## Examples

``` r
desc(package = "desc")
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
#> Built: R 4.6.1; ; 2026-09-24 15:03:03 UTC; unix
#> Config/Needs/website: tidyverse/tidytemplate
#> Config/roxygen2/version: 8.1.0
#> Config/testthat/edition: 3
#> Config/usethis/last-upkeep: 2025-04-23
#> Encoding: UTF-8
#> Language: en-US
#> NeedsCompilation: no
#> Packaged: 2026-09-24 15:03:01 UTC; runner
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
DESCRIPTION <- system.file("DESCRIPTION", package = "desc")
desc(DESCRIPTION)
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
#> Built: R 4.6.1; ; 2026-09-24 15:03:03 UTC; unix
#> Config/Needs/website: tidyverse/tidytemplate
#> Config/roxygen2/version: 8.1.0
#> Config/testthat/edition: 3
#> Config/usethis/last-upkeep: 2025-04-23
#> Encoding: UTF-8
#> Language: en-US
#> NeedsCompilation: no
#> Packaged: 2026-09-24 15:03:01 UTC; runner
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
```
