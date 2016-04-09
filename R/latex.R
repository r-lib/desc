idesc_to_latex <- function(self, private) {
  cols <- field_order(names(private$data))
  col_str <- unlist(lapply(
    cols,
    FUN = function(col) {
      to_latex(private$data[[col]])
    }))

  paste(
    "\\begin{description}",
    "  \\raggedright{}",
    paste0("  ", col_str, collapse = "\n"),
    "\\end{description}",
    "",
    sep = "\n"
  )
}


to_latex <- function(x, ...) UseMethod("to_latex", x)


#' @export
#' @method to_latex character

to_latex.character <- function(x, ...) {
  x <- gsub("'([^ ']*)'", "`\\1'", x, useBytes = TRUE)
  x <- gsub("\"([^\"]*)\"", "``\\1''", x, useBytes = TRUE)
  x <- gsub("\\", "\\textbackslash ", x, fixed = TRUE,
               useBytes = TRUE)
  x <- gsub("([{}$#_^%])", "\\\\\\1", x, useBytes = TRUE)
  x
}

#' @export
#' @method to_latex DescriptionField

to_latex.DescriptionField <- function(x, ...) {
  paste0("\\item[", x$key, "] ", to_latex(x$value))
}

#' @export
#' @method to_latex DescriptionCollate

to_latex.DescriptionCollate <- function(x, ...) {
  invisible(NULL)
}

#' @export
#' @method to_latex DescriptionURLList

to_latex.DescriptionURLList <- function(x, ...) {
  paste0("\\item[", x$key, "] ", format_url(parse_url_list(x$value)))
}

#' @export
#' @method to_latex DescriptionURL

to_latex.DescriptionURL <- function(x, ...) {
  paste0("\\item[", x$key, "] ", format_url(x$value))
}


#' @export
#' @method to_latex DescriptionAuthorsAtR

to_latex.DescriptionAuthorsAtR <- function(x, ...) {
  xx <- parse_authors_at_r(x$value)
  c(
    "\\item[Authors@R] \\\\",
    "  \\begin{description}",
    paste0("    ", vapply(xx, to_latex, character(1L)), collapse = "\n"),
    "  \\end{description}"
  )
}

#' @export
to_latex.person <- function(x, ...) {
  paste0(
    "\\item",
    format(x, include = c("role")),
    " ",
    format(x, include = c("given", "family")),
    " ",
    if (!is.null(x$email))
      paste0("<\\href{mailto:", x$email, "}{", x$email, "}>"),
    format(x, include = c("comment"))
  )
}

format_url <- function(x) {
  paste0("\\url{", x, "}", collapse = ", ")
}
