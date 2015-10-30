
chk <- function(msg, check) {
  if (check) TRUE else msg
}

chks <- function(..., x, warn) {
  results <- list(...)
  results <- unlist(setdiff(results, TRUE))
  results <- if (length(results) == 0) TRUE else results

  if (! identical(results, TRUE) && warn) {
    warning(
      call. = FALSE,
      "'", x$key, "'",
      paste0(
        if (length(results) == 1) " " else  "\n    * ",
        strwrap(results, indent = 0, exdent = 6)
      )
    )
  }

  results
}


#' Syntactical check of a DESCRIPTION field
#'
#' @param x The field.
#' @param warn Whether to generate a warning if the syntax check fails.
#' @param ... Additional arguments, they might be used in the future.
#' @return \code{TRUE} if the field is syntactically correct,
#'   otherwise a character vector, containing one or multiple
#'   error messages.
#'
#' @export

check_field <- function(x, warn = FALSE, ...)
  UseMethod("check_field")

#' @export
#' @method check_field DescriptionField

check_field.DescriptionField <- function(x, warn = FALSE, ...) TRUE

## * Only contains ASCII letters, numbers, dots.
## * At least two characters long.
## * Starts with a letter.
## * Does not end with a dot.
##
##' @export
##' @method check_field DescriptionPackage

check_field.DescriptionPackage <- function(x, warn = FALSE, ...) {

  chks(
    x = x,
    warn = warn,
    chk("must only contain ASCII letters, numbers, dots",
        grepl("^[a-zA-Z0-9\\.]*$", x$value)),
    chk("must be at least two characters long",
        nchar(x$value) >= 2),
    chk("must start with a letter",
        grepl("^[a-zA-Z]", x$value)),
    chk("must not end with a dot",
        !grepl("\\.$", x$value))
  )
}

## sequence of at least two (and usually three) non-negative integers
## separated by single ‘.’ or ‘-’ characters
##
##' @export
##' @method check_field DescriptionVersion

check_field.DescriptionVersion <- function(x, warn = FALSE, ...) {

  chks(
    x = x,
    warn = warn,
    chk(paste("must be a sequence of at least two (usually three)",
              " non-negative integers separated by a single dot or dash",
              " character"),
        grepl("^[0-9]+[-\\.][0-9]+([-\\.][0-9]+)*$", x$value))
  )
}

## TODO
##' @export
##' @method check_field DescriptionLicense

check_field.DescriptionLicense <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionDescription

check_field.DescriptionDescription <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionTitle

check_field.DescriptionTitle <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionMaintainer

check_field.DescriptionMaintainer <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionAuthorsAtR

check_field.DescriptionAuthorsAtR <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionDependencyList

check_field.DescriptionDependencyList <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionRepoList

check_field.DescriptionRepoList <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionURL

check_field.DescriptionURL <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionURLList

check_field.DescriptionURLList <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionPriority

check_field.DescriptionPriority <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionCollate

check_field.DescriptionCollate <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionLogical

check_field.DescriptionLogical <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionPackageList

check_field.DescriptionPackageList <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionEncoding

check_field.DescriptionEncoding <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionOSType

check_field.DescriptionOSType <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionType

check_field.DescriptionType <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionClassification

check_field.DescriptionClassification <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionLanguage

check_field.DescriptionLanguage <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionDate

check_field.DescriptionDate <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionCompression

check_field.DescriptionCompression <- function(x, warn = FALSE, ...) {
  TRUE
}

# TODO
##' @export
##' @method check_field DescriptionRepository

check_field.DescriptionRepository <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionFreeForm

check_field.DescriptionFreeForm <- function(x, warn = FALSE, ...) {
  TRUE
}

## TODO
##' @export
##' @method check_field DescriptionAddedByRCMD

check_field.DescriptionAddedByRCMD <- function(x, warn = FALSE, ...) {
  TRUE
}
