
context("Assertions")

test_that("is_existing_file", {
  miss <- basename(tempfile())
  withr::with_dir(
    tempdir(),
    expect_error(desc::desc(miss), paste0(miss, ".*does not exist"))
  )
})
