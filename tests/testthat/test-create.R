
context("Constructors")

test_that("can create new object", {
  desc <- description$new("!new")

  expect_true(!is.na(desc$get("Package")))
  expect_true(!is.na(desc$get("Title")))
  expect_true(!is.na(desc$get("Version")))
  expect_true(!is.na(desc$get("Authors@R")))
  expect_true(!is.na(desc$get("Maintainer")))
  expect_true(!is.na(desc$get("Description")))
  expect_true(!is.na(desc$get("License")))
  expect_true(!is.na(desc$get("LazyData")))
  expect_true(!is.na(desc$get("URL")))
  expect_true(!is.na(desc$get("BugReports")))
})

test_that("can read object from file", {
  desc <- description$new("D1")

  expect_true(!is.na(desc$get("Package")))
  expect_true(!is.na(desc$get("Title")))
  expect_true(!is.na(desc$get("Version")))
  expect_true(!is.na(desc$get("Author")))
  expect_true(!is.na(desc$get("Maintainer")))
  expect_true(!is.na(desc$get("Description")))
  expect_true(!is.na(desc$get("License")))
  expect_true(!is.na(desc$get("URL")))
  expect_true(!is.na(desc$get("BugReports")))

})

test_that("can read object from character vector", {
  lines <- readLines("D1")
  desc <- description$new(text = lines)

  expect_true(!is.na(desc$get("Package")))
  expect_true(!is.na(desc$get("Title")))
  expect_true(!is.na(desc$get("Version")))
  expect_true(!is.na(desc$get("Author")))
  expect_true(!is.na(desc$get("Maintainer")))
  expect_true(!is.na(desc$get("Description")))
  expect_true(!is.na(desc$get("License")))
  expect_true(!is.na(desc$get("URL")))
  expect_true(!is.na(desc$get("BugReports")))

})

test_that("DESCRPTION is read by default", {

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd("files")

  d1 <- description$new()
  d2 <- description$new("DESCRIPTION")

  expect_equal(d1, d2)
})

test_that("From installed package", {

  desc <- description$new(package = "utils")
  expect_match(desc$get("Author"), "Core Team")

  expect_error(
    description$new(package = "fgsdgsdhldsknfglkedsfgsdf"),
    "Cannot find DESCRIPTION for installed package"
  )
})

test_that("Package root is found", {

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd("files")

  d1 <- description$new()

  dir.create("subdir", showWarnings = FALSE)
  setwd("subdir")
  d2 <- description$new()

  expect_equal(d1, d2)
})
