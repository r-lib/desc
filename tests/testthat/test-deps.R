
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

test_that("set_dep for the first dependency", {

  desc <- description$new("!new")

  expect_equal(eval(formals(desc$set_dep)[["type"]])[[1L]], "Imports")

  desc$set_dep("igraph")

  res <- data.frame(
    stringsAsFactors = FALSE,
    type = c("Imports"),
    package = c("igraph"),
    version = c("*")
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

test_that("has_dep", {

  desc <- description$new("D1")
  expect_true(desc$has_dep("R6"))
  expect_true(desc$has_dep("testthat"))

  expect_true(desc$has_dep("R6", "Imports"))
  expect_true(desc$has_dep("testthat", "Suggests"))

  expect_false(desc$has_dep("foobar"))

  expect_false(desc$has_dep("testthat", "Imports"))

  expect_error(desc$has_dep(123))
  expect_error(desc$has_dep("testthat", "xxx"))
})

test_that("issue #34 is fine (empty dep fields)", {

  empty_deps <- data.frame(
    stringsAsFactors = FALSE,
    type = character(),
    package = character(),
    version = character()
  )

  desc <- description$new("D4")
  expect_silent(deps <- desc$get_deps())
  expect_equal(deps, empty_deps)

  desc$set(Imports = "")
  expect_silent(deps <- desc$get_deps())
  expect_equal(deps, empty_deps)

  expect_silent(desc$set_deps(deps))
  expect_silent(deps <- desc$get_deps())
  expect_equal(deps, empty_deps)
})

test_that("no dependencies at all", {

  empty_deps <- data.frame(
    stringsAsFactors = FALSE,
    type = character(),
    package = character(),
    version = character()
  )

  desc <- description$new("D6")
  expect_silent(deps <- desc$get_deps())
  expect_equal(deps, empty_deps)

  expect_silent(desc$set_deps(deps))
  expect_silent(deps <- desc$get_deps())
  expect_equal(deps, empty_deps)
})
