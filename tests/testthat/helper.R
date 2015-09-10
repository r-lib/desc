
test_file <- function(path) {
  system.file(file.path("tests", "testthat", path), package = "description")
}
