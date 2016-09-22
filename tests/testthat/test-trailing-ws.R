
context("Absense of trailing WS is kept")

test_that("No WS is kept if field is not modified", {
  d <- description$new("D3")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  d$write(tmp)
  expect_equal(readLines("D3"), readLines(tmp))
})

test_that("WS is present in newly created fields", {
  d <- description$new("D3")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  d$set("Foobar" = "\n    TRAZ")
  d$write(tmp)
  contents <- paste0(paste0(readLines(tmp), collapse = "\n"), "\n")
  expect_match(contents, "\nFoobar: \n    TRAZ\n")
})

test_that("WS is present in newly created files", {
  desc <- description$new("!new")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  desc$write(tmp)
  contents <- paste0(readLines(tmp), collapse = "\n")
  expect_match(contents, "\nAuthors@R: \n")
})

test_that("WS is added if field is changed", {
  d <- description$new("D3")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  d$add_author("Gabor", "Csardi", "foo@bar.com")
  d$write(tmp)
  contents <- paste0(readLines(tmp), collapse = "\n")
  expect_match(contents, "Authors@R: \n")
})

test_that("No WS is added if an other field is changed", {
  d <- description$new("D3")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  d$add_author("Gabor", "Csardi", "foo@bar.com")
  d$write(tmp)
  contents <- paste0(readLines(tmp), collapse = "\n")
  expect_match(contents, "Imports:\n")
})
