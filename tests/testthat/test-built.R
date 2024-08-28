
test_that("get built", {
  desc <- description$new(test_path("D4"))
  expect_identical(
    desc$get_built(),
    list(R = R_system_version("3.4.1"),
      Platform = "x86_64-apple-darwin15.6.0",
      Date = "2017-09-14 20:30:19 UTC",
      OStype = "unix")
  )
})

test_that("corrupted Build field", {
  desc <- description$new("!new")
  desc$set(Built = "foobar")
  expect_error(desc$get_built(), "corrupted")
})

test_that("get built works with multiples lines", {
  expect_identical(
    desc_get_built(test_path("D16")),
    list(
      R = R_system_version("3.4.1"),
      Platform = "x86_64-apple-darwin15.6.0",
      Date = "2017-09-14 20:30:19 UTC",
      OStype = "unix"
    )
  )
})
