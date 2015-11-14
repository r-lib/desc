
context("Utility functions")

test_that("check_for_package works", {

  expect_true(check_for_package("codetools"))

  expect_error(
    check_for_package("foobarfoobarfoobar"),
    "Package 'foobarfoobarfoobar' is needed"
  )
  
})
