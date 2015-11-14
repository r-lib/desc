
context("Queries")

test_that("get works", {
  desc <- description$new("D1")

  expect_equal(desc$get("Package"), c(Package = "description"))
  expect_equal(desc$get("Version"), c(Version = "1.0.0"))
  expect_equal(desc$get("Author"), c(Author = "Gabor Csardi"))
  expect_equal(desc$get("Imports"), c(Imports = "\n    R6"))
})

test_that("set works", {
  desc <- description$new("D1")
  expect_equal(desc$get("Package"), c(Package = "description"))

  desc$set(Package = "foobar")
  expect_equal(desc$get("Package"), c(Package = "foobar"))

  desc$set("Package", "foo")
  expect_equal(desc$get("Package"), c(Package = "foo"))

  desc$set(Version = "100.0", Author = "Bugs Bunny")
  expect_equal(desc$get("Version"), c(Version = "100.0"))
  expect_equal(desc$get("Author"), c(Author = "Bugs Bunny"))

  desc$set("Description", "Foo\n  foobar\n  foobar2.")
  expect_equal(desc$get("Description"),
               c(Description = "Foo\n  foobar\n  foobar2."))
})

test_that("get with non-exixting fields", {
  desc <- description$new("D1")
  expect_equal(desc$get("foobar"), c(foobar = NA_character_))
})

test_that("del works", {
  desc <- description$new("D1")
  desc$del("Package")
  expect_equal(desc$get("Package"), c(Package = NA_character_))
  expect_false(is.na(desc$get("Title")))
})

test_that("set errors on invalid input", {

  desc <- description$new("D1")
  expect_error(
    desc$set("foobar"),
    "needs two unnamed args"
  )
})
