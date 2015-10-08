
## TODO: continuation lines

desc_str <- function(self, private, by_lines) {
  cols <- field_order(names(private$data))
  col_str <- vapply(
    cols, FUN.VALUE = "",
    FUN = function(col) {
      format(private$data[[col]])
    })

  if (by_lines) col_str else paste(col_str, collapse = "\n")
}

field_order <- function(fields) {
  first <- c(
    "Type", "Package", "Title", "Version",
    "Authors@R", "Author", "Maintainer",
    "Description", "License", "URL", "BugReports",
    dep_types
  )

  last <- collate_fields

  c(
    intersect(first, fields),
    sort(setdiff(fields, c(first, last))),
    intersect(last, fields)
  )
}

#' @export
#' @method format DescriptionField

format.DescriptionField <- function(x) {
  paste(
    strwrap(paste0(x$key, ": ", x$value), exdent = 4),
    collapse = "\n"
  )
}

#' @export
#' @method format DescriptionDependencyList

format.DescriptionDependencyList <- function(x) {
  paste0(
    x$key, ":\n",
    paste0(
      "    ",
      str_trim(strsplit(x$value, ",", fixed = TRUE)[[1]]),
      collapse = ",\n"
    )
  )
}

#' @export
#' @method format DescriptionCollate

format.DescriptionCollate <- function(x) {
  paste0(
    x$key, ":\n",
    deparse_collate(parse_collate(x$value)),
    "\n"
  )
}

desc_print <- function(self, private) {
  cat(desc_str(self, private, by_lines = FALSE), sep = "", "\n")
  invisible(self)
}
