
context("desc")

test_that("desc wrapper works", {
  expect_equal(
    desc("D2"),
    description$new("D2")
  )
})
