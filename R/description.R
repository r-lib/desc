
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
#' \preformatted{description$new(cmd = NULL, file = NULL, text = NULL)}
#' \describe{
#'   \item{cmd:}{A command to create a description from scratch.
#'     Currently only \code{"!new"} is implemented. If it does not start
#'     with an exclamation mark, it will be interpreted as a \sQuote{file}
#'     argument.}
#'   \item{file:}{Name of the \code{DESCRIPTION} file to load. If all of
#'     \sQuote{cmd}, \sQuote{file} and \sQuote{text} are \code{NULL} (the
#'     default), then the \code{DESCRIPTION} file in the current working
#'     directory is used.}
#'   \item{text:}{A character scalar containing the full DESCRIPTION.
#'     Character vectors are collapsed into a character scalar, with
#'     newline as the separator.}
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
#' The complete API reference:
#' \preformatted{  description$get(keys)
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
#' it changes the object).
#'
#' \preformatted{  description$str(by_field = FALSE)
#'   description$normalize()
#'   description$print()
#' }
#' \describe{
#'   \item{by_field:}{Whether to return the normalized format
#'     by field, or collapsed into a character scalar.}
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
#' The \code{$del_deps} method removes all declared dependencies.
#'
#' The \code{$set_dep} method adds or updates a single dependency. By
#' default it adds the package to the \code{Imports} field.
#'
#' The API:
#' \preformatted{  description$set_dep(package, type = dep_types, version = "\*")
#'   description$set_deps(deps)
#'   description$get_deps()
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
#' }
#' \describe{
#'    \item{authors:}{A \code{person} object, a list of authors.}
#' }
#' \code{$get_authors} returns a \code{person} object, the parsed
#' authors. See \code{\link[utils]{person}} for details.
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
#'                            package = "description"))
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
    initialize = function(cmd = NULL, file = NULL, text = NULL)
      desc_create(self, private, cmd, file, text),

    write = function(file = NULL, normalize = FALSE)
      desc_write(self, private, file, normalize),

    fields = function()
      desc_fields(self, private),

    has_fields = function(keys)
      desc_has_fields(self, private, keys),

    get = function(keys)
      desc_get(self, private, keys),

    set = function(...)
      desc_set(self, private, ...),

    del = function(keys)
      desc_del(self, private, keys),

    validate = function()
      desc_validate(self, private),

    print = function()
      desc_print(self, private),

    str = function(by_field = FALSE)
      desc_str(self, private, by_field),

    normalize = function()
      desc_normalize(self, private),

    ## -----------------------------------------------------------------
    ## Package dependencies

    set_dep = function(package, type = dep_types, version = "*")
      desc_set_dep(self, private, package, match.arg(type), version),

    set_deps = function(deps)
      desc_set_deps(self, private, deps),

    get_deps = function()
      desc_get_deps(self, private),

    del_dep = function(package, type = c("all", dep_types))
      desc_del_dep(self, private, package, match.arg(type)),

    del_deps = function()
      desc_del_deps(self, private),

    ## -----------------------------------------------------------------
    ## Collate fields

    set_collate = function(files, which = c("main", "windows", "unix"))
      desc_set_collate(self, private, files, match.arg(which)),

    get_collate = function(which = c("main", "windows", "unix"))
      desc_get_collate(self, private, match.arg(which)),

    del_collate = function(which = c("all", "main", "windows", "unix"))
      desc_del_collate(self, private, match.arg(which)),

    add_to_collate = function(files,
      which = c("default", "all", "main", "windows", "unix"))
      desc_add_to_collate(self, private, files, match.arg(which)),

    del_from_collate = function(files,
      which = c("all", "main", "windows", "unix"))
      desc_del_from_collate(self, private, files, match.arg(which)),

    ## -----------------------------------------------------------------
    ## Authors@R

    get_authors = function()
      desc_get_authors(self, private),

    set_authors = function(authors)
      desc_set_authors(self, private, authors),

    add_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL)
      desc_add_author(self, private, given, family, email, role, comment),

    add_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL)
      desc_add_role(self, private, role, given, family, email, comment),

    del_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL)
      desc_del_author(self, private, given, family, email, role, comment),

    del_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL)
      desc_del_role(self, private, role, given, family, email, comment),

    change_maintainer = function(given = NULL, family = NULL, email = NULL,
                                 comment = NULL)
      desc_change_maintainer(self, private, given, family, email, comment),

    add_me = function(role = "ctb", comment = NULL)
      desc_add_me(self, private, role, comment)
  ),

  private = list(
    data = NULL,
    path = "DESCRIPTION"
  )
)


desc_create <- function(self, private, cmd, file, text) {

  if (!is.null(cmd) && substring(cmd, 1, 1) != "!") {
    file <- cmd
    cmd <- NULL
  }

  if (!is.null(cmd)) {
    if (!is.null(file)) warning("file argument ignored")
    if (!is.null(text)) warning("text argument ignored")
    desc_create_cmd(self, private, cmd)

  } else if (is.null(cmd) && is.null(file) && is.null(text)) {
    desc_create_file(self, private, "DESCRIPTION")

  } else if (!is.null(file)) {
    if (!is.null(text)) warning("text argument ignored")
    desc_create_file(self, private, file)

  } else {
    desc_create_text(self, private, text)
  }

  invisible(self)
}

desc_create_cmd <- function(self, private, cmd = c("new")) {
  if (cmd == "!new") {
    desc_create_text(self, private, text =
"Package: {{ Package }}
Title: {{ Title }}
Version: 1.0.0
Author: {{ Author }}
Maintainer: {{ Maintainer }}
Description: {{ Description }}
License: {{ License }}
LazyData: true
URL: {{ URL }}
BugReports: {{ BugReports }}
")
  }

  invisible(self)
}

desc_create_file <- function(self, private, file) {
  private$path <- file
  desc_create_text(self, private, readLines(file))
}

desc_create_text <- function(self, private, text) {
  con <- textConnection(text, local = TRUE)
  on.exit(close(con), add = TRUE)
  dcf <- read_dcf(con)
  private$data <- dcf
}


#' @importFrom crayon strip_style

desc_write <- function(self, private, file, normalize) {
  if (is.null(file)) file <- private$path

  if (normalize) {
    cat(
      strip_style(self$str(by_field = FALSE)),
      "\n",
      sep = "",
      file = file
    )

  } else {
    write.dcf(
      desc_as_matrix(private$data),
      file = file,
      keep.white = names(private$data)
    )
  }

  invisible(self)
}

desc_fields <- function(self, private) {
  names(private$data)
}

desc_has_fields <- function(self, private, keys) {
  keys <- as.character(keys)
  keys %in% self$fields()
}

desc_as_matrix <- function(data) {
  matrix(
    vapply(data, "[[", "", "value"),
    nrow = 1,
    dimnames = list(NULL, names(data))
  )
}

desc_get <- function(self, private, keys) {
  res <- lapply(private$data[keys], "[[", "value")
  res[vapply(res, is.null, logical(1))] <- NA_character_
  res <- unlist(res)
  names(res) <- keys
  res
}


## ... are either
## - two unnamed arguments, key and value, or
## - an arbitrary number of named arguments, the names are the keys,
##   the values are the values

desc_set <- function(self, private, ...) {
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

  fields <- create_fields(keys, values)
  lapply(fields, check_field, warn = TRUE)
  private$data[keys] <- fields

  invisible(self)
}


desc_del <- function(self, private, keys) {
  private$data <- private$data[setdiff(names(private$data), keys)]
  invisible(self)
}
