
## TODO: continuation lines

desc_str <- function(self, private, by_lines) {
  cols <- field_order(names(private$data))
  col_str <- vapply(cols, FUN.VALUE = "", FUN = function(col) {
    format_field(col, private$data[col])
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

format_field <- function(key, value) {
  if (key %in% dep_types) {
    paste0(
      key, ":\n",
      paste0(
        "    ",
        str_trim(strsplit(value, ",", fixed = TRUE)[[1]]),
        collapse = ",\n"
      )
    )

  } else if (key %in% collate_fields) {
    paste0(
      key, ":\n",
      deparse_collate(parse_collate(value)),
      "\n"
    )

  } else {
    paste(
      strwrap(paste0(key, ": ", value), exdent = 4),
      collapse = "\n"
    )
  }
}


desc_print <- function(self, private) {
  cat(desc_str(self, private, by_lines = FALSE), sep = "", "\n")
  invisible(self)
}
