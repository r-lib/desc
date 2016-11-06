
context("Version")

test_that("get_version", {
  desc <- description$new("D1")
  v <- desc$get_version()
  expect_true(inherits(v, "package_version"))
  expect_equal(as.character(v), "1.0.0")

  desc$del("Version")
  expect_error(desc$get_version())
})

test_that("set_version", {
  desc <- description$new("D1")

  desc$set_version("2.1.3")$set_version("2.1.4")
  expect_equal(desc$get_version(), package_version("2.1.4"))

  desc$set_version(package_version("1.9.10.100"))
  expect_equal(desc$get_version(), package_version("1.9.10.100"))

  expect_error(desc$set_version("1"))
  expect_error(desc$set_version("1.0.0-dev"))
})

test_that("bump_version", {
  desc <- description$new("D1")

  cases <- list(
    c("1.2.3", "major", "2.0.0"),
    c("1.2.3", "minor", "1.3.0"),
    c("1.2.3", "patch", "1.2.4"),
    c("1.2.3", "dev",   "1.2.3.9000"),

    c("1.5",   "major", "2.0"),
    c("1.5",   "minor", "1.6"),
    c("1.5",   "patch", "1.5.1"),
    c("1.5",   "dev",   "1.5.0.9000"),

    c("1.2.3.9000", "major", "2.0.0"),
    c("1.2.3.9000", "minor", "1.3.0"),
    c("1.2.3.9000", "patch", "1.2.4"),
    c("1.2.3.9000", "dev",   "1.2.3.9001"),

    list("1.2.3", 1, "2.0.0"),
    list("1.2.3", 2, "1.3.0"),
    list("1.2.3", 3, "1.2.4"),
    list("1.2.3", 4, "1.2.3.9000"),

    list("1.5",   1, "2.0"),
    list("1.5",   2, "1.6"),
    list("1.5",   3, "1.5.1"),
    list("1.5",   4, "1.5.0.9000"),

    list("1.2.3.9000", 1, "2.0.0"),
    list("1.2.3.9000", 2, "1.3.0"),
    list("1.2.3.9000", 3, "1.2.4"),
    list("1.2.3.9000", 4, "1.2.3.9001")
  )

  for (c in cases) {
    expect_equal(
      desc$set_version(c[[1]])$bump_version(c[[2]])$get_version(),
      package_version(c[[3]])
    )
  }
})
