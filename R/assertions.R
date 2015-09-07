
is_string <- function(x, allow_na = FALSE) {
  is.character(x) && length(x) == 1 && (allow_na || !is.na(x))
}


is_key <- function(x) {
  is_string(x)
}


is_keys <- function(x) {
  is.character(x)
}


as_string <- function(x) {
  x <- as.character(x)
  if (length(x) != 1) stop("Value must be a scalar")
  x
}


is_filename_or_null <- function(x) {
  is.null(x) || is_string(x)
}
