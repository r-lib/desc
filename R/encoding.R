
#' Check encoding of new or existing fields
#'
#' If \code{new_fields} is \code{NULL}, then the existing
#' fields are checked. Otherwised \code{new_fields} are checked.
#'
#' Warnings are given for non-ascii fields, if the \code{Encoding}
#' field is not set.
#'
#' @param self Object.
#' @param private Private env.
#' @param new_fields New fields, or \code{NULL} to check existing fields.
#' @return Object, invisibly.
#'
#' @keywords internal

check_encoding <- function(self, private, new_fields) {
  if (!is.na(self$get('Encoding'))) return(invisible(self))

  fields <- if (is.null(new_fields)) {
    as.list(self$get(self$fields()))
  } else {
    new_fields
  }

  nonascii <- !vapply(fields, is_ascii, TRUE)

  if (any(nonascii)) {
    warning(
      "Consider adding an Encoding field to DESCRIPTION,\n",
      "Non-ASCII character(s) in ",
      paste(names(fields)[nonascii], collapse = ", ")
    )
  }

  invisible(self)
}

fix_dcf_encoding_read <- function(dcf) {
  fix_dcf_encoding(dcf, direction = "to")
}

fix_dcf_encoding_write <- function(dcf) {
  fix_dcf_encoding(dcf, direction = "from")
}

fix_dcf_encoding <- function(dcf, direction) {
  if (! "Encoding" %in% colnames(dcf) ||
      (encoding <- dcf[, "Encoding"]) == "UTF-8") return(dcf)

  if (! encoding %in% portable_dcf_encodings()) {
    warning("Encoding `", encoding, "` is not portable, consider using ",
            "`UTF-8` instead")
  }

  if (direction == "to") {
    dcf[] <- iconv(dcf[], from = encoding, to = "UTF-8")
  } else if (direction == "from") {
    dcf[] <- vapply(dcf[], FUN.VALUE = character(1), function(x) {
      if (Encoding(x) != "unknown") {
        x <- iconv(x, from = Encoding(x), to = encoding)
        Encoding(x) <- "unknown"
        x
      } else {
        x
      }
    })
  }

  dcf
}
