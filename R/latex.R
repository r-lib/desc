idesc_to_latex <- function(self, private) {
  cols <- field_order(names(private$data))
  col_str <- unlist(lapply(
    cols,
    FUN = function(col) {
      toLatex(private$data[[col]])
    }))

  structure(
    c(
      "\\begin{description}",
      "  \\raggedright{}",
      paste0("  ", col_str, collapse = "\n"),
      "\\end{description}"
    ),
    class = "Latex"
  )
}


#' @importFrom utils toLatex
NULL

#' @export
toLatex.character <- function(x, ...) {
  x <- gsub("'([^ ']*)'", "`\\1'", x, useBytes = TRUE)
  x <- gsub("\"([^\"]*)\"", "``\\1''", x, useBytes = TRUE)
  x <- gsub("\\", "\\textbackslash ", x, fixed = TRUE,
               useBytes = TRUE)
  x <- gsub("([{}$#_^%])", "\\\\\\1", x, useBytes = TRUE)
  x
}

#' @export
toLatex.DescriptionField <- function(x, ...) {
  paste0("\\item[", x$key, "] ", toLatex(x$value))
}

#' @export
toLatex.DescriptionCollate <- function(x, ...) {
  invisible(NULL)
}

#' @export
toLatex.DescriptionURLList <- function(x, ...) {
  paste0("\\item[", x$key, "] ", format_url(parse_url_list(x$value)))
}

#' @export
toLatex.DescriptionURL <- function(x, ...) {
  paste0("\\item[", x$key, "] ", format_url(x$value))
}


#' @export
toLatex.DescriptionAuthorsAtR <- function(x, ...) {
  xx <- parse_authors_at_r(x$value)
  c(
    "\\item[Authors@R] ~\\\\",
    "  \\begin{description}",
    paste0("    ", vapply(xx, toLatex, character(1L)), collapse = "\n"),
    "  \\end{description}"
  )
}

#' @export
toLatex.person <- function(x, ...) {
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
