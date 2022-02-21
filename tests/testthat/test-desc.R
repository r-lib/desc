
test_that("desc wrapper works", {
  expect_equal(
    desc(test_path("D2")),
    description$new(test_path("D2"))
  )
})
