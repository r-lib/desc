
#' Read, write, update DESCRIPTION files
#'
#' @section Constructor:
#' TODO
#' \preformatted{x <- description$new()
#' x <- description$new("!new")}
#'
#' @section Methods:
#' TODO
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
#' ## Remove some field
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

    write = function(file = NULL)
      desc_write(self, private, file),

    str = function(by_lines = FALSE)
      desc_str(self, private, by_lines),

    get = function(keys)
      desc_get(self, private, keys),

    set = function(...)
      desc_set(self, private, ...),

    del = function(keys = ? is_keys)
      desc_del(self, private, keys),

    validate = function()
      desc_validate(self, private),

    print = function()
      desc_print(self, private),

    set_dep = function(package, type = dep_types, version = "*")
      desc_set_dep(self, private, package, match.arg(type), version),

    set_deps = function(package, deps)
      desc_set_deps(self, private, deps),

    get_deps = function()
      desc_get_deps(self, private),

    del_dep = function(package, type = c("all", dep_types))
      desc_del_dep(self, private, package, match.arg(type)),

    del_deps = function()
      desc_del_deps(self, private)

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


desc_write <- function(self, private, file) {
  if (is.null(file)) file <- private$path

  data <- matrix(
    nrow = 1,
    private$data,
    dimnames = list(NULL, names(private$data))
  )

  write.dcf(data, file = file)

  invisible(self)
}

desc_get <- function(self, private, keys) {
  res <- private$data[keys]
  res[is.na(res)] <- NA_character_
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
    key <- as_string(args[[1]])
    value <- as_string(args[[2]])
    private$data[key] <- value

  } else if (!is.null(names(args)) && all(names(args) != "")) {
    keys <- names(args)
    values <- unlist(unname(args))
    private$data[keys] <- values

  } else {
    stop("$set needs two unnamed args, or all named args, see docs")
  }

  invisible(self)
}


desc_del <- function(self, private, keys) {
  private$data <- private$data[setdiff(names(private$data), keys)]
  invisible(self)
}
