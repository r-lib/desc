
str_trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}


is_ascii <- function(x) {
  vapply(
    as.character(x),
    function(txt) all(charToRaw(txt) <= as.raw(127)),
    NA
  )
}


## This is from tools/R/QC.R

RFC_2822_email_regexp <- function() {

  ## Local part consists of ASCII letters and digits, the characters
  ##   ! # $ % * / ? | ^ { } ` ~ & ' + = _ -
  ## and . provided it is not leading or trailing or repeated, or must
  ## be a quoted string.
  ## Domain part consists of dot-separated elements consisting of
  ## ASCII letters, digits and hyphen.
  ## We could also check that the local and domain parts are no longer
  ## than 64 and 255 characters, respectively.
  ## See http://en.wikipedia.org/wiki/Email_address.

  ASCII_letters_and_digits <-
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  l <- sprintf("[%s%s]", ASCII_letters_and_digits, "!#$%*/?|^{}`~&'+=_-")
  d <- sprintf("[%s%s]", ASCII_letters_and_digits, "-")
  ## Be careful to arrange the hyphens to come last in the range spec.
  sprintf("(\\\".+\\\"|(%s+\\.)*%s+)@(%s+\\.)*%s+", l, l, d, d)
}()


is_url <- function(x) {
  grep("^(https?|ftp)://\\s*$", str_trim(x))
}


is_url_list <- function(x) {
  xx <- strsplit(x, ",", fixed = TRUE)[[1]]
  all(vapply(xx, is_url, TRUE))
}


all_true <- function(x) {
  all(vapply(x, identical, TRUE, TRUE))
}


flatten <- function(x) {
  if (is.list(x)) {
    x <- lapply(
      x,
      function(e) if (is.null(e)) "" else paste(e, collapse = ",")
    )
    x <- unlist(x)
  }
  x
}

ngrepl <- function(pattern, x, ...) {
  if (is.null(pattern)) pattern <- ""
  x <- flatten(x)
  grepl(pattern, x, ...)
}
