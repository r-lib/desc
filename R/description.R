
#' Read a DESCRIPTION file
#'
#' This is a convenience wrapper for \code{description$new()}.
#' Very often you want to read an existing \code{DESCRIPTION}
#' file, and to do this you can just supply the path to the file or its
#' directory to \code{desc()}.
#'
#' @param cmd A command to create a description from scratch.
#'   Currently only \code{"!new"} is implemented. If it does not start
#'   with an exclamation mark, it will be interpreted as the \sQuote{file}
#'   argument.
#' @param file Name of the \code{DESCRIPTION} file to load. If all of
#'   \sQuote{cmd}, \sQuote{file} and \sQuote{text} are \code{NULL} (the
#'   default), then the \code{DESCRIPTION} file in the current working
#'   directory is used. The file can also be an R package (source, or
#'   binary), in which case the DESCRIPTION file is extracted from it, but
#'   note that in this case \code{$write()} cannot write the file back in
#'   the package archive.
#' @param text A character scalar containing the full DESCRIPTION.
#'   Character vectors are collapsed into a character scalar, with
#'   newline as the separator.
#' @param package If not NULL, then the name of an installed package
#'     and the DESCRIPTION file of this package will be loaded.
#'
#' @export
#' @examples
#' desc(package = "desc")
#' DESCRIPTION <- system.file("DESCRIPTION", package = "desc")
#' desc(DESCRIPTION)

desc <- function(cmd = NULL, file = NULL, text = NULL, package = NULL) {
  description$new(cmd, file, text, package)
}

#' Read, write, update, validate DESCRIPTION files
#'
#' @section Constructors:
#'
#' There are two ways of creating a description object. The first
#' is reading an already existing \code{DESCRIPTION} file; simply give
#' the name of the file as an argument. The default is
#' \code{DESCRIPTION}: \preformatted{  x <- description$new()
#'   x2 <- description$new("path/to/DESCRIPTION")}
#'
#' The second way is creating a description object from scratch,
#' supply \code{"!new"} as an argument to do this.
#' \preformatted{  x3 <- description$new("!new")}
#'
#' The complete API reference:
#' \preformatted{description$new(cmd = NULL, file = NULL, text = NULL,
#'     package = NULL)}
#' \describe{
#'   \item{cmd:}{A command to create a description from scratch.
#'     Currently only \code{"!new"} is implemented. If it does not start
#'     with an exclamation mark, it will be interpreted as a \sQuote{file}
#'     argument.}
#'   \item{file:}{Name of the \code{DESCRIPTION} file to load. If it is
#'     a directory, then we assume that it is inside an R package and
#'     conduct a search for the package root directory, i.e. the first
#'     directory up the tree that contains a \code{DESCRIPTION} file.
#'     If \sQuote{cmd}, \sQuote{file}, \sQuote{text} and \sQuote{package}
#'     are all \code{NULL} (the default), then the search is started from
#'     the working directory. The file can also be an R package (source, or
#'     binary), in which case the DESCRIPTION file is extracted from it,
#'     but note that in this case \code{$write()} cannot write the file
#'     back in the package archive.}
#'   \item{text:}{A character scalar containing the full DESCRIPTION.
#'     Character vectors are collapsed into a character scalar, with
#'     newline as the separator.}
#'   \item{package}{If not NULL, then the name of an installed package
#'     and the DESCRIPTION file of this package will be loaded.}
#' }
#'
#' @section Setting and Querying fields:
#' Set a field with \code{$set} and query it with \code{$get}:
#' \preformatted{  x <- description$new("!new")
#'   x$get("Package)
#'   x$set("Package", "foobar")
#'   x$set(Title = "Example Package for 'description'")
#'   x$get("Package")}
#' Note that \code{$set} has two forms. You can either give the field name
#' and new value as two arguments; or you can use a single named argument,
#' the argument name is the field name, the argument value is the field
#' value.
#'
#' The \code{$fields} method simply lists the fields in the object:
#' \preformatted{  x$fields()}
#'
#' The \code{$has_fields} method checks if one or multiple fields are
#' present in a description object: \preformatted{  x$has_fields("Package")
#'   x$has_fields(c("Title", "foobar"))}
#'
#' The \code{$del} method removes the specified fields:
#' \preformatted{  x$set(foo = "bar")
#'   x$del("foo")}
#'
#' \code{$get_or_fail} is similar to \code{$get}, but throws an error
#' if a field does not exist, except of silently returning
#' \code{NA_character}.
#'
#' The complete API reference:
#' \preformatted{  description$get(keys)
#'   description$get_or_fail(keys)
#'   description$set(...)
#'   description$fields()
#'   description$has_fields(keys)
#'   description$del(keys)}
#' \describe{
#'   \item{keys:}{A character vector of keys to query, check or delete.}
#'   \item{...:}{This must be either two unnamed arguments, the key and
#'     and the value to set; or an arbitrary number of named arguments,
#'     names are used as keys, values as values to set.}
#' }
#'
#' @section Normalizing:
#' Format DESCRIPTION in a standard way. \code{$str} formats each
#' field in a standard way and returns them (it does not change the
#' object itself), \code{$print} is used to print it to the
#' screen. The \code{$normalize} function normalizes each field (i.e.
#' it changes the object). Normalization means reformatting the fields,
#' via \code{$reformat_fields()} and also reordering them via
#' \code{$reorder_fields()}. The format of the various fields is
#' opinionated and you might like it or not. Note that \code{desc} only
#' reformats fields that it updates, and only on demand, so if your
#' formatting preferences differ, you can still manually edit
#' \code{DESCRIPTION} and \code{desc} will respect your edits.
#'
#' \preformatted{  description$str(by_field = FALSE, normalize = TRUE,
#'     mode = c("file", "screen"))
#'   description$normalize()
#'   description$reformat_fields()
#'   description$reorder_fields()
#'   description$print()
#' }
#' \describe{
#'   \item{by_field:}{Whether to return the normalized format
#'     by field, or collapsed into a character scalar.}
#'   \item{normalize:}{Whether to reorder and reformat the fields.}
#'   \item{mode:}{\sQuote{file} mode formats the fields as they are
#'     written to a file with the \code{write} method. \sQuote{screen}
#'     mode adds extra markup to some fields, e.g. formats the
#'     \code{Authors@R} field in a readable way.}
#' }
#'
#' @section Writing it to file:
#' The \code{$write} method writes the description to a file.
#' By default it writes it to the file it was created from, if it was
#' created from a file. Otherwise giving a file name is compulsary:
#' \preformatted{  x$write(file = "DESCRIPTION")}
#'
#' The \code{normalize} argument controls whether the fields are
#' reformatted according to a standard style. By default they are not.
#'
#' The API:
#' \preformatted{  description$write(file = NULL, normalize = NULL)}
#' \describe{
#'   \item{file:}{Path to write the description to. If it was created
#'      from a file in the first place, then it is written to the same
#'      file. Otherwise this argument must be specified.}
#'   \item{normalize:}{Whether to reformat the fields in a standard way.}
#' }
#'
#' @section Version numbers:
#'
#' \preformatted{  description$get_version()
#'   description$set_version(version)
#'   description$bump_version(which = c("patch", "minor", "major", "dev"))
#' }
#'
#' \describe{
#'   \item{version:}{A string or a \code{\link[base]{package_version}}
#'     object.}
#'   \item{which:}{Which component of the version number to increase.
#'     See details just below.}
#' }
#'
#' These functions are simple helpers to make it easier to query, set and
#' increase the version number of a package.
#'
#' \code{$get_version()} returns the version number as a
#' \code{\link[base]{package_version}} object. It throws an error if the
#' package does not have a \sQuote{Version} field.
#'
#' \code{$set_version()} takes a string or a
#' \code{\link[base]{package_version}} object and sets the \sQuote{Version}
#' field to it.
#'
#' \code{$bump_version()} increases the version number. The \code{which}
#' parameter specifies which component to increase.
#' It can be a string referring to a component: \sQuote{major},
#' \sQuote{minor}, \sQuote{patch} or \sQuote{dev}, or an integer
#' scalar, for the latter components are counted from one, and the
#' beginning. I.e. component one is equivalent to \sQuote{major}.
#'
#' If a component is bumped, then the ones after it are zeroed out.
#' Trailing zero components are omitted from the new version number,
#' but if the old version number had at least two or three components, then
#' the one will also have two or three.
#'
#' The bumping of the \sQuote{dev} version (the fourth component) is
#' special: if the original version number had less than four components,
#' and the \sQuote{dev} version is bumped, then it is set to \code{9000}
#' instead of \code{1}. This is a convention often used by R developers,
#' it was originally invented by Winston Chang.
#'
#' Both \code{$set_version()} and \code{$bump_version()} use dots to
#' separate the version number components.
#'
#' @section Dependencies:
#' These functions handle the fields that define how the R package
#' uses another R packages. See \code{\link{dep_types}} for the
#' list of fields in this group.
#'
#' The \code{$get_deps} method returns all declared dependencies, in a
#' data frame with columns: \code{type}, \code{package} and \code{version}.
#' \code{type} is the name of the dependency field, \code{package} is the
#' name of the R package, and \code{version} is the required version. If
#' no specific versions are required, then this is a \code{"\*"}.
#'
#' The \code{$set_deps} method is the opposite of \code{$get_deps} and
#' it sets all dependencies. The input is a data frame, with the same
#' structure as the return value of \code{$get_deps}.
#'
#' The \code{$has_dep} method checks if a package is included in the
#' dependencies. It returns a logical scalar. If \code{type} is not
#' \sQuote{any}, then it has to match as well.
#'
#' The \code{$del_deps} method removes all declared dependencies.
#'
#' The \code{$set_dep} method adds or updates a single dependency. By
#' default it adds the package to the \code{Imports} field.
#'
#' The API:
#' \preformatted{  description$set_dep(package, type = dep_types, version = "\*")
#'   description$set_deps(deps)
#'   description$get_deps()
#'   description$has_dep(package, type = c("any", dep_types))
#'   description$del_dep(package, type = c("all", dep_types))
#'   description$del_deps()
#' }
#' \describe{
#'   \item{package:}{Name of the package to add to or remove from the
#'     dependencies.}
#'   \item{type:}{Dependency type, see \code{\link{dep_types}}. For
#'     \code{$del_dep} it may also be \code{"all"}, and then the package
#'     will be deleted from all dependency types.}
#'   \item{version:}{Required version. Defaults to \code{"\*"}, which means
#'     no explicit version requirements.}
#'   \item{deps:}{A data frame with columns \code{type}, \code{package} and
#'     \code{version}. \code{$get_deps} returns the same format.}
#' }
#'
#' @section Collate fields:
#' Collate fields contain lists of file names with R source code,
#' and the package has a separate API for them. In brief, you can
#' use \code{$add_to_collate} to add one or more files to the main or
#' other collate field. You can use \code{$del_from_collate} to remove
#' it from there.
#'
#' The API:
#' \preformatted{  description$set_collate(files, which = c("main", "windows", "unix"))
#'   description$get_collate(which = c("main", "windows", "unix"))
#'   description$del_collate(which = c("all", "main", "windows", "unix"))
#'   description$add_to_collate(files, which = c("default", "all", "main",
#'     "windows", "unix"))
#'   description$del_from_collate(files, which = c("all", "main",
#'     "windows", "unix"))
#' }
#' \describe{
#'   \item{files:}{The files to add or remove, in a character vector.}
#'   \item{which:}{Which collate field to manipulate. \code{"default"} for
#'   \code{$add_to_collate} means all existing collate fields, or the
#'   main one if none exist.}
#' }
#'
#' @section Authors:
#' There is a specialized API for the \code{Authors@R} field,
#' to add and remove authors, udpate their roles, change the maintainer,
#' etc.
#'
#' The API:
#' \preformatted{  description$get_authors()
#'   description$set_authors(authors)
#'   description$get_author(role)
#'   description$get_maintainer()
#' }
#' \describe{
#'    \item{authors:}{A \code{person} object, a list of authors.}
#'    \item{role:}{The role to query. See \code{person} for details.}
#' }
#' \code{$get_authors} returns a \code{person} object, the parsed
#' authors. See \code{\link[utils]{person}} for details.
#'
#' \code{$get_author} returns a \code{person} object, all authors with
#' the specified role.
#'
#' \code{$get_maintainer} returns the maintainer of the package. It works
#' with \code{Authors@R} fields and with traditional \code{Maintainer}
#' fields as well.
#'
#' \preformatted{  description$add_author(given = NULL, family = NULL, email = NULL,
#'     role = NULL, comment = NULL)
#'   description$add_me(role = "ctb", comment = NULL)
#' }
#' Add a new author. The arguments correspond to the arguments of the
#' \code{\link[utils]{person}} function. \code{add_me} is a convenience
#' function, it adds the current user as an author, and it needs the
#' \code{whoami} package to be installed.
#'
#' \preformatted{  description$del_author(given = NULL, family = NULL, email = NULL,
#'     role = NULL, comment = NULL)
#' }
#' Remove an author, or multiple authors. The author(s) to be removed
#' can be specified via any field(s). All authors matching all
#' specifications will be removed. E.g. if only \code{given = "Joe"}
#' is supplied, then all authors whole given name matches \code{Joe} will
#' be removed. The specifications can be (PCRE) regular expressions.
#'
#' \preformatted{  description$add_role(role, given = NULL, family = NULL, email = NULL,
#'     comment = NULL)
#'   description$del_role(role, given = NULL, family = NULL, email = NULL,
#'      comment = NULL)
#'   description$change_maintainer(given = NULL, family = NULL,
#'     email = NULL, comment = NULL)
#' }
#' \code{role} is the role to add or delete. The other arguments
#' are used to select a subset of the authors, on which the operation
#' is performed, similarly to \code{$del_author}.
#'
#' @section URLs:
#'
#' We provide helper functions for manipulating URLs in the \code{URL}
#' field:
#'
#' \preformatted{  description$get_urls()
#'   description$set_urls(urls)
#'   description$add_urls(urls)
#'   description$del_urls(pattern)
#'   description$clear_urls()
#' }
#' \describe{
#'   \item{urls:}{Character vector of URLs to set or add.}
#'   \item{pattern:}{Perl compatible regular expression to specify the
#'     URLs to be removed.}
#' }
#' \code{$get_urls()} returns all urls in a character vector. If no URL
#' fields are present, a zero length vector is returned.
#'
#' \code{$set_urls()} sets the URL field to the URLs specified in the
#' character vector argument.
#'
#' \code{$add_urls()} appends the specified URLs to the URL field. It
#' creates the field if it does not exists. Duplicate URLs are removed.
#'
#' \code{$del_urls()} deletes the URLs that match the specified pattern.
#'
#' \code{$clear_urls()} deletes all URLs.
#'
#' @section Remotes:
#'
#' \code{devtools}, \code{remotes} and some other packages support the
#' non-standard \code{Remotes} field in \code{DESCRIPTION}. This field
#' can be used to specify locations of dependent packages: GitHub or
#' BitBucket repositories, generic git repositories, etc. Please see the
#' \sQuote{Package remotes} vignette in the \code{devtools} package.
#'
#' \code{desc} has helper functions for manipulating the \code{Remotes}
#' field:
#'
#' \preformatted{  description$get_remotes()
#'   description$get_remotes()
#'   description$set_remotes(remotes)
#'   description$add_remotes(remotes)
#'   description$del_remotes(pattern)
#'   description$clear_remotes()
#' }
#' \describe{
#'   \item{remotes:}{Character vector of remote dependency locations to
#'     set or add.}
#'   \item{pattern:}{Perl compatible regular expression to specify the
#'     remote dependency locations to remove.}
#' }
#' \code{$get_remotes()} returns all remotes in a character vector.
#' If no URL fields are present, a zero length vector is returned.
#'
#' \code{$set_remotes()} sets the URL field to the Remotes specified in the
#' character vector argument.
#'
#' \code{$add_remotes()} appends the specified remotes to the
#' \code{Remotes} field. It creates the field if it does not exists.
#' Duplicate remotes are removed.
#'
#' \code{$del_remotes()} deletes the remotes that match the specified
#' pattern.
#'
#' \code{$clear_remotes()} deletes all remotes.
#'
#' @section Built:
#'
#' The \sQuote{Built} field is used in binary packages to store information
#' about when and how a binary package was built.
#'
#' \code{$get_built()} returns the built information as a list with fields
#' \sQuote{R}, \sQuote{Platform}, \sQuote{Date}, \sQuote{OStype}. It throws an
#' error if the package does not have a \sQuote{Built} field.
#'
#' @section Encodings:
#' When creating a `description` object, `desc` observes the `Encoding`
#' field, if present, and uses the specified encoding to parse the file.
#' Internally, it converts all fields to UTF-8.
#'
#' When writing a `description` object to a file, `desc` uses the
#' `Encoding` field (if present), and converts all fields to the specified
#' encoding.
#'
#' We suggest that whenever you need to use non-ASCII characters in your
#' package, you use the UTF-8 encoding, for maximum portability.
#'
#' @export
#' @importFrom R6 R6Class
#' @docType class
#' @format An R6 class.
#'
#' @examples
#' ## Create a template
#' desc <- description$new("!new")
#' desc
#'
#' ## Read a file
#' desc2 <- description$new(file = system.file("DESCRIPTION",
#'                            package = "desc"))
#' desc2
#'
#' ## Remove a field
#' desc2$del("LazyData")
#'
#' ## Add another one
#' desc2$set(VignetteBuilder = "knitr")
#' desc2$get("VignetteBuilder")
#' desc2

description <- R6Class("description",
  public = list(

    ## Either from a file, or from a character vector
    initialize = function(cmd = NULL, file = NULL, text = NULL, package = NULL)
      idesc_create(self, private, cmd, file, text, package),

    write = function(file = NULL)
      idesc_write(self, private, file),

    fields = function()
      idesc_fields(self, private),

    has_fields = function(keys)
      idesc_has_fields(self, private, keys),

    get = function(keys)
      idesc_get(self, private, keys),

    get_or_fail = function(keys)
      idesc_get_or_fail(self, private, keys),

    set = function(...)
      idesc_set(self, private, ...),

    del = function(keys)
      idesc_del(self, private, keys),

    validate = function()
      idesc_validate(self, private),

    print = function()
      idesc_print(self, private),

    str = function(by_field = FALSE, normalize = TRUE,
      mode = c("file", "screen"))
      idesc_str(self, private, by_field, normalize, mode),

    to_latex = function()
      idesc_to_latex(self, private),

    normalize = function()
      idesc_normalize(self, private),

    reformat_fields = function()
      idesc_reformat_fields(self, private),

    reorder_fields = function()
      idesc_reorder_fields(self, private),

    ## -----------------------------------------------------------------
    ## Version numbers

    get_version = function()
      idesc_get_version(self, private),

    set_version = function(version)
      idesc_set_version(self, private, version),

    bump_version = function(which)
      idesc_bump_version(self, private, which),

    ## -----------------------------------------------------------------
    ## Package dependencies

    set_dep = function(package, type = desc::dep_types, version = "*")
      idesc_set_dep(self, private, package, match.arg(type), version),

    set_deps = function(deps)
      idesc_set_deps(self, private, deps),

    get_deps = function()
      idesc_get_deps(self, private),

    del_dep = function(package, type = c("all", desc::dep_types))
      idesc_del_dep(self, private, package, match.arg(type)),

    del_deps = function()
      idesc_del_deps(self, private),

    has_dep = function(package, type = c("any", desc::dep_types))
      idesc_has_dep(self, private, package, match.arg(type)),

    ## -----------------------------------------------------------------
    ## Collate fields

    set_collate = function(files, which = c("main", "windows", "unix"))
      idesc_set_collate(self, private, files, match.arg(which)),

    get_collate = function(which = c("main", "windows", "unix"))
      idesc_get_collate(self, private, match.arg(which)),

    del_collate = function(which = c("all", "main", "windows", "unix"))
      idesc_del_collate(self, private, match.arg(which)),

    add_to_collate = function(files,
      which = c("default", "all", "main", "windows", "unix"))
      idesc_add_to_collate(self, private, files, match.arg(which)),

    del_from_collate = function(files,
      which = c("all", "main", "windows", "unix"))
      idesc_del_from_collate(self, private, files, match.arg(which)),

    ## -----------------------------------------------------------------
    ## Authors@R

    get_authors = function()
      idesc_get_authors(self, private),

    get_author = function(role = "cre")
      idesc_get_author(self, private, role),

    set_authors = function(authors)
      idesc_set_authors(self, private, authors),

    add_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL)
      idesc_add_author(self, private, given, family, email, role, comment),

    add_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL)
      idesc_add_role(self, private, role, given, family, email, comment),

    del_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL)
      idesc_del_author(self, private, given, family, email, role, comment),

    del_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL)
      idesc_del_role(self, private, role, given, family, email, comment),

    change_maintainer = function(given = NULL, family = NULL, email = NULL,
                                 comment = NULL)
      idesc_change_maintainer(self, private, given, family, email, comment),

    add_me = function(role = "ctb", comment = NULL)
      idesc_add_me(self, private, role, comment),

    get_maintainer = function()
      idesc_get_maintainer(self, private),

    ## -----------------------------------------------------------------
    ## URL

    get_urls = function()
      idesc_get_urls(self, private),

    set_urls = function(urls)
      idesc_set_urls(self, private, urls),

    add_urls = function(urls)
      idesc_add_urls(self, private, urls),

    del_urls = function(pattern)
      idesc_del_urls(self, private, pattern),

    clear_urls = function()
      idesc_clear_urls(self, private),

    ## -----------------------------------------------------------------
    ## Remotes

    get_remotes = function()
      idesc_get_remotes(self, private),

    set_remotes = function(remotes)
      idesc_set_remotes(self, private, remotes),

    add_remotes = function(remotes)
      idesc_add_remotes(self, private, remotes),

    del_remotes = function(pattern)
      idesc_del_remotes(self, private, pattern),

    clear_remotes = function()
      idesc_clear_remotes(self, private),

    ## -----------------------------------------------------------------
    ## Built

    get_built = function()
      idesc_get_built(self, private)
  ),

  private = list(
    data = NULL,
    path = "DESCRIPTION",
    notws = character()                   # entries without trailing ws
  )
)

idesc_create <- function(self, private, cmd, file, text, package) {

  if (!is.null(cmd) && substring(cmd, 1, 1) != "!") {
    file <- cmd
    cmd <- NULL
  }

  if (!is.null(cmd)) {
    if (!is.null(file)) warning("file argument ignored")
    if (!is.null(text)) warning("text argument ignored")
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_cmd(self, private, cmd)

  } else if (is.null(cmd) && is.null(file) && is.null(text) &&
             is.null(package)) {
    idesc_create_file(self, private, ".")

  } else if (!is.null(file)) {
    if (!is.null(text)) warning("text argument ignored")
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_file(self, private, file)

  } else if (!is.null(text)) {
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_text(self, private, text)

  } else {
    idesc_create_package(self, private, package)
  }

  invisible(self)
}

idesc_create_cmd <- function(self, private, cmd = c("new")) {
  assert_that(is_constructor_cmd(cmd))

  if (cmd == "!new") {
    idesc_create_text(self, private, text =
'Package: {{ Package }}
Title: {{ Title }}
Version: 1.0.0
Authors@R: 
    c(person(given = "Jo", family = "Doe", email = "jodoe@dom.ain",
      role = c("aut", "cre")))
Maintainer: {{ Maintainer }}
Description: {{ Description }}
License: {{ License }}
LazyData: true
URL: {{ URL }}
BugReports: {{ BugReports }}
Encoding: UTF-8
')
  }

  invisible(self)
}

idesc_create_file <- function(self, private, file) {
  assert_that(is_path(file))

  if (file.exists(file) && is_dir(file)) file <- find_description(file)
  assert_that(is_existing_file(file))

  if (is_package_archive(file)) {
    file <- get_description_from_package(file)

  } else {
    private$path <- normalizePath(file)
  }

  tryCatch(
    lines <- readLines(file),
    error = function(e) stop("Cannot read ", file, ": ", e$message)
  )

  idesc_create_text(self, private, lines)
}

idesc_create_text <- function(self, private, text) {
  assert_that(is.character(text))
  con <- textConnection(text, local = TRUE)
  on.exit(close(con), add = TRUE)
  dcf <- read_dcf(con)
  private$notws <- dcf$notws
  private$data <- dcf$dcf
  check_encoding(self, private, NULL)
}

idesc_create_package <- function(self, private, package) {
  assert_that(is_string(package))
  path <- system.file(package = package, "DESCRIPTION")
  if (path == "") {
    stop("Cannot find DESCRIPTION for installed package ", package)
  }
  idesc_create_file(self, private, path)
}

#' @importFrom crayon strip_style

idesc_write <- function(self, private, file) {
  if (is.null(file)) file <- private$path
  if (is.null(file)) {
    stop("Cannot write back DESCRIPTION. Note that it is not possible
          to update DESCRIPTION files within package archives")
  }

  mat <- idesc_as_matrix(private$data)
  if ("Encoding" %in% colnames(mat)) {
    encoding <- mat[, "Encoding"]
    mat[] <- iconv(mat[], from = "UTF-8", to = encoding)
    Encoding(mat) <- encoding
  } else {
    encoding <- ""
  }

  ## Need to write to a temp file first, to preserve absense of trailing ws
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  write.dcf(mat, file = tmp, keep.white = names(private$data))

  removed <- ! names(private$notws) %in% colnames(mat)
  if (any(removed)) private$notws <- private$notws[! removed]

  changed <- mat[, names(private$notws)] != private$notws
  if (any(changed)) private$notws <- private$notws[! changed]

  postprocess_trailing_ws(tmp, names(private$notws))
  if (file.exists(file) && is_dir(file)) file <- find_description(file)

  ofile <- file(file, encoding = encoding, open = "w+")
  on.exit(close(ofile), add = TRUE)
  writeLines(readLines(tmp), ofile)

  invisible(self)
}

idesc_fields <- function(self, private) {
  names(private$data)
}

idesc_has_fields <- function(self, private, keys) {
  assert_that(is.character(keys), has_no_na(keys))
  keys %in% self$fields()
}

idesc_as_matrix <- function(data) {
  matrix(
    vapply(data, "[[", "", "value"),
    nrow = 1,
    dimnames = list(NULL, names(data))
  )
}

idesc_get <- function(self, private, keys) {
  assert_that(is.character(keys), has_no_na(keys))
  res <- lapply(private$data[keys], "[[", "value")
  res[vapply(res, is.null, logical(1))] <- NA_character_
  res <- unlist(res)
  names(res) <- keys
  res
}

idesc_get_or_fail <- function(self, private, keys) {
  assert_that(is.character(keys), has_no_na(keys))
  res <- self$get(keys)
  if (any(is.na(res))) {
    w <- is.na(res)
    msg <- paste0(
      "Could not find DESCRIPTION ",
      if (sum(w) == 1) "field: " else "fields: ",
      paste(sQuote(keys[w]), collapse = ", "),
      "."
    )
    stop(msg, call. = FALSE)
  }
  res
}

## ... are either
## - two unnamed arguments, key and value, or
## - an arbitrary number of named arguments, the names are the keys,
##   the values are the values

idesc_set <- function(self, private, ...) {
  args <- list(...)

  if (is.null(names(args)) && length(args) == 2) {
    keys <- as_string(args[[1]])
    values <- as_string(args[[2]])

  } else if (!is.null(names(args)) && all(names(args) != "")) {
    keys <- names(args)
    values <- unlist(args)

  } else {
    stop("$set needs two unnamed args, or all named args, see docs")
  }

  fields <- create_fields(keys, enc2utf8(values))
  lapply(fields, check_field, warn = TRUE)
  check_encoding(self, private, lapply(fields, "[[", "value"))
  private$data[keys] <- fields

  invisible(self)
}


idesc_del <- function(self, private, keys) {
  assert_that(is.character(keys), has_no_na(keys))
  private$data <- private$data[setdiff(names(private$data), keys)]
  invisible(self)
}
