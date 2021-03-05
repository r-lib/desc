
temp_desc <- function(file = "D2") {
  tmp <- tempfile()
  file.copy(file, tmp)
  tmp
}

private <- function(x) {
  x$.__enclos_env__$private
}
