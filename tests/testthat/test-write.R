
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
