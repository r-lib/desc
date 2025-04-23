temp_desc <- function(file = "D2") {
  tmp <- tempfile()
  file.copy(test_path(file), tmp)
  tmp
}

private <- function(x) {
  x$.__enclos_env__$private
}
