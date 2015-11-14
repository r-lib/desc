
context("Dependencies")

test_that("get_deps", {
  desc <- description$new("D1")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports", "Suggests"),
    package = c("R6", "testthat"),
    version = c("*", "*")
  )

  expect_equal(desc$get_deps(), res)
})


test_that("set_dep", {
  desc <- description$new("D1")

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
  desc <- description$new("D1")

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

  desc <- description$new("D1")

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

test_that("deleting all dependencies", {

  desc <- description$new("D1")
  desc$del_deps()
  expect_equal(desc$get("Imports"), c(Imports = NA_character_))
  expect_equal(desc$get("Suggests"), c(Suggests = NA_character_))

})

test_that("deleting a non-dependency is OK", {

  desc <- description$new("D1")
  before <- desc$get("Imports")
  desc$del_dep("foobar", "Imports")
  after <- desc$get("Imports")
  expect_equal(before, after)
})