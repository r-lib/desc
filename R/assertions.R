
as_string <- function(x) {
  x <- as.character(x)
  if (length(x) != 1) stop("Value must be a scalar")
  x
}
