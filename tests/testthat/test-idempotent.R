
context("Nice behavior")

test_that("dependencies are not reformatted if new value is the same", {

  desc <- description$new("D1")
  desc$set("Imports", "R6")
  before <- desc$get("Imports")
  desc$set_dep("R6")
  after <- desc$get("Imports")
  expect_equal(before, after)
  
  desc <- description$new("D1")
  desc$set("Imports", "R6")
  before <- desc$get("Imports")
  desc$set_deps(data.frame(package = "R6", type = "Imports", version = "*"))
  after <- desc$get("Imports")
  expect_equal(before, after)
})
