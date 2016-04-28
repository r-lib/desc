
## TODO: continuation lines

idesc_str <- function(self, private, by_field,
                     mode = c("file", "screen")) {
  mode <- match.arg(mode)
  cols <- field_order(names(private$data))
  col_str <- vapply(
    cols, FUN.VALUE = "",
    FUN = function(col) {
      format(private$data[[col]], mode = mode)
    })

  if (by_field) col_str else paste(col_str, collapse = "\n")
}

field_order <- function(fields) {
  first <- c(
    "Type", "Package", "Title", "Version", "Date",
    "Authors@R", "Author", "Maintainer",
    "Description", "License", "URL", "BugReports",
    "Depends", setdiff(dep_types, "Depends"), "VignetteBuilder"
  )

  last <- collate_fields

  c(
    intersect(first, fields),
    sort(setdiff(fields, c(first, last))),
    intersect(last, fields)
  )
}

#' @importFrom crayon red

color_bad <- function(x) {
  if (identical(check_field(x), TRUE)) x$value else red(x$value)
}

#' @export
#' @importFrom crayon blue
#' @method format DescriptionField

format.DescriptionField <- function(x, ..., width = 75) {
  paste(
    strwrap(paste0(blue(x$key), ": ", color_bad(x)), exdent = 4, width = width),
    collapse = "\n"
  )
}

#' @export
#' @importFrom crayon blue
#' @method format DescriptionDependencyList

format.DescriptionDependencyList <- function(x, ...) {
  paste0(
    blue(x$key), ":\n",
    paste0(
      "    ",
      str_trim(strsplit(color_bad(x), ",", fixed = TRUE)[[1]]),
      collapse = ",\n"
    )
  )
}

#' @export
#' @importFrom crayon blue
#' @method format DescriptionCollate

format.DescriptionCollate <- function(x, ...) {
  paste0(
    blue(x$key), ":\n",
    deparse_collate(parse_collate(color_bad(x)))
  )
}

#' @export
#' @importFrom crayon blue red
#' @method format DescriptionAuthorsAtR

format.DescriptionAuthorsAtR <- function(x, mode = c("file", "screen"),
                                         ...) {
  xx <- parse_authors_at_r(x$value)

  if (mode == "screen") {
    good <- check_field(x)
    xxx <- if (good) xx else red(xx)
    paste0(
      blue(x$key), " (parsed):\n",
      paste0("    * ", format(xxx), collapse = "\n")
    )

  } else {
    paste0(
      blue(x$key), ":\n",
      sub("\n$", "", deparse_authors_at_r(xx))
    )
  }
}


idesc_print <- function(self, private) {
  cat(
    idesc_str(self, private, by_field = FALSE, mode = "screen"),
    sep = "",
    "\n"
  )
  invisible(self)
}


idesc_normalize <- function(self, private) {
  self$reformat_fields()
  self$reorder_fields()
  invisible(self)
}

#' @importFrom crayon strip_style

idesc_reformat_fields <- function(self, private) {
  norm_fields <- strip_style(idesc_str(self, private, by_field = TRUE))
  for (f in names(norm_fields)) {
    private$data[[f]]$value <-
      sub(paste0(f, ":[ ]?"), "", norm_fields[[f]])
  }
  invisible(self)
}

idesc_reorder_fields <- function(self, private) {
  private$data <- private$data[field_order(names(private$data))]
  invisible(self)
}
