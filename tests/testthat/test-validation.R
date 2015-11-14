
context("Validation")

test_that("validation is not implemented", {

  desc <- description$new("!new")
  expect_warning(
    desc$validate(),
    "not implemented"
  )
})
