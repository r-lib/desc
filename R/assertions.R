
as_string <- function(x) {
  x <- as.character(x)
  if (length(x) != 1) stop("Value must be a scalar")
  x
}

#' @importFrom assertthat assert_that on_failure<-

is_string <- function(x) {
  is.character(x) && length(x) == 1 && ! is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string")
}

is_constructor_cmd <- function(x) {
  is_string(x) && substring(x, 1, 1) == "!"
}

on_failure(is_constructor_cmd) <- function(call, env) {
  paste0(deparse(call$x), " is not a string that starts with '!'")
}

is_path <- function(x) {
  is_string(x)
}

on_failure(is_path) <- function(call, env) {
  paste0(deparse(call$x), " is not a path")
}

is_existing_file <- function(x) {
  is_path(x) && file.exists(x)
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File ", deparse(call$x), " does not exist")
}

has_no_na <- function(x) {
  !any(is.na(x))
}

on_failure(has_no_na) <- function(call, env) {
  paste0(deparse(call$x), " must not contain NAs")
}
