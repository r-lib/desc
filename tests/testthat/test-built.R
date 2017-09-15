
context("BUILT")

test_that("get built", {
  desc <- description$new("D4")
  expect_identical(
    desc$get_built(),
    list(R = R_system_version("3.4.1"),
      Platform = "x86_64-apple-darwin15.6.0",
      Date = "2017-09-14 20:30:19 UTC",
      OStype = "unix")
  )
})
