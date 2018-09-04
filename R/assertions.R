
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

# Comment can be either a single string
# or a named vector of strings
is_character <- function(x) {
  is.character(x) && all(! is.na(x))
}

is_named_character <- function(x){
  if(length(x) == 1){
    is_character(x)
  }else{
    is_character(x) && length(names(x)) == length(x)
  }
}

on_failure(is_character) <- function(call, env) {
  paste0(deparse(call$x), " is not a named character vector")
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
  paste0(eval(call$x, env), " is not a path")
}

is_existing_file <- function(x) {
  is_path(x) && file.exists(x)
}

on_failure(is_existing_file) <- function(call, env) {
  paste0("File `", eval(call$x, env), "' does not exist")
}

has_no_na <- function(x) {
  !any(is.na(x))
}

on_failure(has_no_na) <- function(call, env) {
  paste0(deparse(call$x), " must not contain NAs")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_authors <- function(x) {
  inherits(x, "person")
}

on_failure(is_authors) <- function(call, env) {
  paste0(deparse(call$x), " must be a person object")
}

is_string_or_null <- function(x) {
  is_string(x) || is.null(x)
}

is_named_character_or_null <- function(x) {
  is_named_character(x) || is.null(x)
}

on_failure(is_string_or_null) <- function(call, env) {
  paste0(deparse(call$x), " must be a string or NULL")
}

is_collate_field <- function(x) {
  is_string(x) && x %in% names(collate_fields)
}

on_failure(is_collate_field) <- function(call, env) {
  fields <- paste(sQuote(names(collate_fields)), collapse = ", ")
  paste0(deparse(call$x), " must be one of ", fields)
}

is_collate_field_or_all <- function(x) {
  is_string(x) && (x %in% names(collate_fields) || x == "all")
}

on_failure(is_collate_field) <- function(call, env) {
  fields <- paste(sQuote(c(names(collate_fields), "all")), collapse = ", ")
  paste0(deparse(call$x), " must be one of ", fields)
}

is_collate_field_or_all_or_default <- function(x) {
  is_string(x) && (x %in% names(collate_fields) || x == "all" || x == "default")
}

on_failure(is_collate_field) <- function(call, env) {
  fields <- paste(
    sQuote(c(names(collate_fields), "all", "default")),
    collapse = ", "
  )
  paste0(deparse(call$x), " must be one of ", fields)
}

is_deps_df <- function(x) {
  is.data.frame(x) &&
    identical(sort(names(x)), sort(c("type", "package", "version"))) &&
    all(sapply(x, is.character) | sapply(x, is.factor))
}

on_failure(is_deps_df) <- function(call, env) {
  cols <- paste(sQuote(c("type", "package", "version")), collapse = ", ")
  paste0(
    deparse(call$x),
    " must be a data frame with character columns ",
    cols
  )
}

is_package_version <- function(x) {
  tryCatch(
    {
      length(package_version(x)) == 1
    },
    error = function(e) FALSE
  )
}

on_failure(is_package_version) <- function(call, env) {
  paste0(deparse(call$x), " is an invalid version number")
}

is_version_component <- function(x) {
  (is_string(x) && x %in% c("major", "minor", "patch", "dev")) ||
    (is_count(x) && x <= 5)
}

on_failure(is_version_component) <- function(call, env) {
  paste0(deparse(call$x), " is not a version number component (see docs)")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (length 1 integer)")
}
