test_that("get works", {
  desc <- description$new(test_path("D1"))

  expect_equal(desc$get("Package"), c(Package = "desc"))
  expect_equal(desc$get("Version"), c(Version = "1.0.0"))
  expect_equal(desc$get("Author"), c(Author = "G\u00e1bor Cs\u00e1rdi"))
  expect_equal(desc$get("Imports"), c(Imports = "\n    R6"))
})

test_that("get nothing", {
  desc <- description$new(test_path("D1"))
  empty <- structure(character(), names = character())
  expect_identical(desc$get(character()), empty)
})

test_that("get_field works", {
  desc <- description$new(test_path("D1"))

  expect_identical(desc$get_field("Package"), "desc")
  expect_identical(desc$get_field("Version"), "1.0.0")
  expect_identical(desc$get_field("Author"), "G\u00e1bor Cs\u00e1rdi")
  expect_identical(desc$get_field("Imports"), "R6")
  expect_identical(desc$get_field("Imports", trim_ws = FALSE), "\n    R6")
  expect_match(desc$get_field("Description", trim_ws = FALSE), "\\s+")
  expect_match(
    desc$get_field("Description", trim_ws = TRUE, squish_ws = FALSE),
    "\\s+"
  )
  expect_false(grepl("\n", desc$get_field("Description", trim_ws = TRUE)))
  expect_false(grepl(
    "\n",
    desc$get_field("Description", trim_ws = FALSE, squish_ws = TRUE)
  ))

  expect_snapshot(
    error = TRUE,
    desc$get_field("package")
  )
})

test_that("get_or_fail works", {
  desc <- description$new(test_path("D1"))

  expect_equal(desc$get_or_fail("Package"), c(Package = "desc"))
  expect_equal(desc$get_or_fail("Version"), c(Version = "1.0.0"))
  expect_equal(desc$get_or_fail("Author"), c(Author = "G\u00e1bor Cs\u00e1rdi"))
  expect_equal(desc$get_or_fail("Imports"), c(Imports = "\n    R6"))

  expect_equal(
    desc$get_or_fail(c("Package", "Version")),
    c(Package = "desc", Version = "1.0.0")
  )

  expect_snapshot(
    error = TRUE,
    desc$get_or_fail("package")
  )
  expect_snapshot(
    error = TRUE,
    desc$get_or_fail(c("Package", "versionx"))
  )
  expect_snapshot(
    error = TRUE,
    desc$get_or_fail(c("Package", "versionx", "foobar"))
  )
})

test_that("set works", {
  desc <- description$new(test_path("D1"))
  expect_equal(desc$get("Package"), c(Package = "desc"))

  desc$set(Package = "foobar")
  expect_equal(desc$get("Package"), c(Package = "foobar"))

  desc$set("Package", "foo")
  expect_equal(desc$get("Package"), c(Package = "foo"))

  desc$set(Version = "100.0", Author = "Bugs Bunny")
  expect_equal(desc$get("Version"), c(Version = "100.0"))
  expect_equal(desc$get("Author"), c(Author = "Bugs Bunny"))

  desc$set("Description", "Foo\n  foobar\n  foobar2.")
  expect_equal(
    desc$get("Description"),
    c(Description = "Foo\n  foobar\n  foobar2.")
  )
})

test_that("get with non-exixting fields", {
  desc <- description$new(test_path("D1"))
  expect_equal(desc$get("foobar"), c(foobar = NA_character_))
})

test_that("del works", {
  desc <- description$new(test_path("D1"))
  desc$del("Package")
  expect_equal(desc$get("Package"), c(Package = NA_character_))
  expect_false(is.na(desc$get("Title")))
})

test_that("set errors on invalid input", {
  desc <- description$new(test_path("D1"))
  expect_snapshot(
    error = TRUE,
    desc$set("foobar")
  )
})

test_that("get_list, set_list", {
  desc <- description$new(test_path("D1"))
  desc$set(foo = "  this, is   a, \n   list")
  expect_equal(
    desc$get_list("foo"),
    c("this", "is a", "list")
  )
  expect_equal(
    desc$get_list("foo", trim_ws = FALSE, squish_ws = TRUE),
    c(" this", " is a", " list")
  )
  expect_equal(
    desc$get_list("foo", trim_ws = TRUE, squish_ws = FALSE),
    c("this", "is   a", "list")
  )
  expect_equal(
    desc$get_list("foo", trim_ws = FALSE, squish_ws = FALSE),
    c("  this", " is   a", " \n   list")
  )

  desc$set_list("key", c("this", "that"))
  expect_equal(desc$get_list("key"), c("this", "that"))
})
