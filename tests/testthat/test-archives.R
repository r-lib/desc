
context("Package archives")

test_that("is_zip_file", {
  expect_true(is_zip_file(file.path("fixtures", "xxx.zip")))
  expect_false(is_zip_file(file.path("fixtures", "xxx.gz")))
  expect_false(is_zip_file(file.path("fixtures", "xxx.tar.gz")))
})

test_that("is_gz_file", {
  expect_false(is_gz_file(file.path("fixtures", "xxx.zip")))
  expect_true(is_gz_file(file.path("fixtures", "xxx.gz")))
  expect_true(is_gz_file(file.path("fixtures", "xxx.tar.gz")))
})

test_that("is_tar_gz_file", {
  expect_false(is_tar_gz_file(file.path("fixtures", "xxx.zip")))
  expect_false(is_tar_gz_file(file.path("fixtures", "xxx.gz")))
  expect_true(is_tar_gz_file(file.path("fixtures", "xxx.tar.gz")))
})

test_that("is_valid_package_file_name", {
  pos <- c(
    "foo_1.0.tar.gz",
    "a1_0.2.tar.gz",
    "A0_1.2-4.tar.gz",
    "foo_1.0.tgz",
    "a1_0.2.tgz",
    "A0_1.2-4.tgz",
    "foo_1.0.zip",
    "a1_0.2.zip",
    "A0_1.2-4.zip",
    "R6_2.2.0_R_x86_64-pc-linux-gnu.tar.gz"
  )

  neg <- c(
    "1foo_1.0.tar.gz",                  # cannot start with number
    "x_1.5.tar.gz",                     # must be at least two characters
    "xx-1.5.tar.gz",                    # dash separator is invalid
    "foo_1.tar.gz",                     # version number must have 2 comps
    "foo_1.0.tar.gzx",                  # invalid file extension
    "foo_1.0.zipfile"                   # invalid file extension
  )

  for (x in pos) expect_true(is_valid_package_file_name(x), info = x)
  for (x in neg) expect_false(is_valid_package_file_name(x), info = x)
})

test_that("is_package_archive", {
  pos <- file.path("fixtures", c(
    "pkg_1.0.0.tar.gz",
    "pkg_1.0.0.tgz",
    "pkg_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  ))
  neg <- file.path("fixtures", c("xxx.zip", "xxx.gz", "xxx.tar.gz"))

  for (x in pos) expect_true(is_package_archive(x), info = x)
  for (x in neg) expect_false(is_package_archive(x), info = x)
})

test_that("get_description_from_package", {
  d1 <- description$new(file.path("fixtures", "pkg_1.0.0.tar.gz"))
  d2 <- description$new(file.path("fixtures", "pkg_1.0.0.tgz"))
  d3 <- description$new(file.path(
    "fixtures",
    "pkg_1.0.0_R_x86_64-pc-linux-gnu.tar.gz"
  ))
  d4 <- description$new(file.path("fixtures", "pkg_1.0.0.zip"))

  expect_equal(d1$get("Package"), c(Package = "pkg"))
  expect_equal(d2$get("Package"), c(Package = "pkg"))
  expect_equal(d3$get("Package"), c(Package = "pkg"))
  expect_equal(d4$get("Package"), c(Package = "pkg"))

  expect_error(
    d4 <- description$new(file.path("fixtures", "notpkg_1.0.tar.gz")),
    "Cannot extract DESCRIPTION"
  )
})

test_that("write errors if from archive", {
  d <- description$new(file.path("fixtures", "pkg_1.0.0.tar.gz"))
  expect_error(d$write(), "Cannot write back DESCRIPTION")
})
