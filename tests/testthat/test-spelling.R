
test_package_root <- function() {
  x <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  pkg <- testthat::testing_package()
  x <- tryCatch(
    rprojroot::find_package_root_file(
      path = file.path("..", "..", "00_pkg_src", pkg)),
    error = function(e) NULL)

  if (!is.null(x)) return(x)

  stop("Cannot find package root")
}

test_that("spelling", {
  skip_on_cran()
  skip_on_covr()
  pkgroot <- test_package_root()
  err <- spelling::spell_check_package(pkgroot)
  num_spelling_errors <- nrow(err)
  expect_true(
    num_spelling_errors == 0,
    info = paste(
      c("\nSpelling errors:", capture.output(err)),
      collapse = "\n"
    )
  )
})
