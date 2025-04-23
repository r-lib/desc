if (Sys.getenv("NOT_CRAN") == "true") {
  library(testthat)
  library(desc)
  test_check("desc", reporter = "summary")
}
