
context("Dependencies")

test_that("get_deps", {
  desc <- description$new(test_file("D1"))

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Suggests"),
    package = c("R6", "testthat"),
    version = c("*", "*")
  )

  expect_equal(desc$get_deps(), res)
})


test_that("set_dep", {
  desc <- description$new(test_file("D1"))

  desc$set_dep("igraph")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Imports", "Suggests"),
    package = c("R6", "igraph", "testthat"),
    version = c("*", "*", "*")
  )
  expect_equal(desc$get_deps(), res)

  desc$set_dep("igraph", version = ">= 1.0.0")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Imports", "Suggests"),
    package = c("R6", "igraph", "testthat"),
    version = c("*", ">= 1.0.0", "*")
  )

  expect_equal(desc$get_deps(), res)

  desc$set_dep("igraph", type = "Depends", version = ">= 1.0.0")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Imports", "Suggests", "Depends"),
    package = c("R6", "igraph", "testthat", "igraph"),
    version = c("*", ">= 1.0.0", "*", ">= 1.0.0")
  )

  expect_equal(desc$get_deps(), res)

})

test_that("del_dep", {
  desc <- description$new(test_file("D1"))

  desc$set_dep("igraph")
  desc$set_dep("igraph", type = "Depends", version = ">= 1.0.0")
  desc$del_dep("igraph")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Suggests"),
    package = c("R6", "testthat"),
    version = c("*", "*")
  )

  expect_equal(desc$get_deps(), res)

  desc <- description$new(test_file("D1"))

  desc$set_dep("igraph")
  desc$set_dep("igraph", type = "Depends", version = ">= 1.0.0")
  desc$del_dep("igraph", type = "Imports")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Suggests", "Depends"),
    package = c("R6", "testthat", "igraph"),
    version = c("*", "*", ">= 1.0.0")
  )

  expect_equal(desc$get_deps(), res)

})
