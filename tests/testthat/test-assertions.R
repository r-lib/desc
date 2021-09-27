
test_that("is_existing_file", {
  miss <- basename(tempfile())
  withr::with_dir(
    tempdir(),
    expect_error(desc::desc(miss), "is_existing_file")
  )
})

test_that("is_named_character_or_null", {
  x <- c(2, 3)
  expect_false(is_named_character_or_null(x))

  x <- "comment"
  expect_true(is_named_character_or_null(x))

  x <- c("comment1", "comment")
  expect_false(is_named_character_or_null(x))

  x <- c(comment1 = "comment1",
         comment2 = "comment")
  expect_true(is_named_character_or_null(x))

  x <- c(comment1 = "comment1",
         comment2 = NA)
  expect_false(is_named_character_or_null(x))


  x <- NULL
  expect_true(is_named_character_or_null(x))
})

