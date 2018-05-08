
context("Write")

test_that("can write to file", {

  desc <- description$new("!new")
  tmp <- tempfile()
  desc$write(tmp)

  desc2 <- description$new(tmp)
  expect_equal(desc$str(), desc2$str())

})

test_that("normalization while writing to file", {

  desc <- description$new("!new")
  desc$set("Imports", "foo, bar, foobar")

  tmp <- tempfile()
  desc$write(tmp)

  desc$normalize()

  desc2 <- description$new(tmp)
  expect_equal(desc$str(), desc2$str())
})

test_that("whitespace after : if field was updated", {

  on.exit(unlink(c(t1, t2), recursive = TRUE), add = TRUE)

  ## Space is kept if it was there
  desc <- description$new(text = "Imports: \n    one,\n    two\n")
  desc$set_dep("doh")
  desc$write(t1 <- tempfile())

  expect_equal(
    readLines(t1),
    c("Imports: ", "    one,", "    two,", "    doh")
  )

  ## Space is not added if it was not there
  desc <- description$new(text = "Imports:\n    one,\n    two\n")
  desc$set_dep("doh")
  desc$write(t2  <- tempfile())

  expect_equal(
    readLines(t2),
    c("Imports:", "    one,", "    two,", "    doh")
  )
})
