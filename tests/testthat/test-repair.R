context("repair")

test_that("normalization", {

  desc <- description$new("!new")
  desc$set("Imports", "foo, bar, foobar")

  before_fields <- desc$fields()
  before_data <- desc$get(before_fields)
  desc$normalize()
  after_fields <- desc$fields()
  after_data <- desc$get(after_fields)

  expect_equal(sort(before_fields), sort(after_fields))
  expect_lt(match("Imports", after_fields), match("Imports", before_fields))
  expect_gt(match("LazyData", after_fields), match("LazyData", before_fields))
  expect_equal(after_data[["Imports"]], "\n    foo,\n    bar,\n    foobar")

})

test_that("reformatting", {

  desc <- description$new("!new")
  desc$set("Imports", "foo, bar, foobar")

  before_fields <- desc$fields()
  before_data <- desc$get(before_fields)
  desc$reformat_fields()
  after_fields <- desc$fields()
  after_data <- desc$get(after_fields)

  expect_equal(before_fields, after_fields)
  expect_equal(after_data[["Imports"]], "\n    foo,\n    bar,\n    foobar")

})

test_that("reordering", {

  desc <- description$new("!new")
  desc$set("Imports", "foo, bar, foobar")

  before_fields <- desc$fields()
  before_data <- desc$get(before_fields)
  desc$reorder_fields()
  after_fields <- desc$fields()
  after_data <- desc$get(after_fields)

  expect_equal(sort(before_fields), sort(after_fields))
  expect_lt(match("Imports", after_fields), match("Imports", before_fields))
  expect_gt(match("LazyData", after_fields), match("LazyData", before_fields))
  expect_identical(before_data[after_fields], after_data)

})
