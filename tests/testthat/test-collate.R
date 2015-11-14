
context("Collate API")

test_that("set_collate and get_collate work", {
  desc <- description$new("D1")

  files <- c("foo.R", "bar.R", "foobar.R")
  desc$set_collate(files)

  expect_equal(desc$get_collate(), files)
  expect_equal(desc$get_collate(which = "windows"), character())
  expect_equal(desc$get_collate(which = "unix"), character())

  files2 <- c(files, "foo-win.R")
  desc$set_collate(files2, which = "windows")

  expect_equal(desc$get_collate("windows"), files2)
  expect_equal(desc$get_collate(which = "main"), files)
  expect_equal(desc$get_collate(which = "unix"), character())

  files3 <- c(files, "foo-unix.R")
  desc$set_collate(files3, which = "unix")

  expect_equal(desc$get_collate(which = "unix"), files3)
  expect_equal(desc$get_collate(which = "windows"), files2)
  expect_equal(desc$get_collate(which = "main"), files)

})

test_that("del_collate works", {
  desc <- description$new("D1")

  files <- c("foo.R", "bar.R", "foobar.R")
  files2 <- c(files, "foo-win.R")
  files3 <- c(files, "foo-unix.R")

  desc$set_collate(files, which = "main")
  desc$set_collate(files2, which = "windows")
  desc$set_collate(files3, which = "unix")

  desc$del_collate(which = "windows")

  expect_equal(desc$get_collate(which = "windows"), character())
  expect_equal(desc$get_collate(which = "main"), files)
  expect_equal(desc$get_collate(which = "unix"), files3)

  desc$del_collate()

  expect_equal(desc$get_collate(which = "windows"), character())
  expect_equal(desc$get_collate(which = "main"), character())
  expect_equal(desc$get_collate(which = "unix"), character())

})

test_that("add_to_collate works", {
  desc <- description$new("D1")

  desc$add_to_collate("bar.R")
  expect_equal(desc$get_collate(), "bar.R")

  desc$add_to_collate("foo.R")
  expect_equal(desc$get_collate(), c("bar.R", "foo.R"))

  desc$add_to_collate("foobar.R", which = "windows")
  expect_equal(desc$get_collate(), c("bar.R", "foo.R"))
  expect_equal(desc$get_collate(which = "windows"), "foobar.R")

  desc$add_to_collate("foobar2.R")
  expect_equal(desc$get_collate(), c("bar.R", "foo.R", "foobar2.R"))
  expect_equal(desc$get_collate(which = "windows"),
               c("foobar.R", "foobar2.R"))

})

test_that("del_from_collate works", {
  desc <- description$new("D1")

  files <- c("foo.R", "bar.R", "foobar.R")
  files2 <- c(files, "foo-win.R")
  files3 <- c(files, "foo-unix.R")

  desc$set_collate(files, which = "main")
  desc$set_collate(files2, which = "windows")
  desc$set_collate(files3, which = "unix")

  desc$del_from_collate("foo.R")

  expect_equal(desc$get_collate(which = "main"), c("bar.R", "foobar.R"))
  expect_equal(desc$get_collate(which = "windows"),
               c("bar.R", "foobar.R", "foo-win.R"))
  expect_equal(desc$get_collate(which = "unix"),
               c("bar.R", "foobar.R", "foo-unix.R"))

  desc$del_from_collate("bar.R", which = "windows")

  expect_equal(desc$get_collate(which = "main"), c("bar.R", "foobar.R"))
  expect_equal(desc$get_collate(which = "windows"),
               c("foobar.R", "foo-win.R"))
  expect_equal(desc$get_collate(which = "unix"),
               c("bar.R", "foobar.R", "foo-unix.R"))

})

test_that("add to all collate fields", {

  desc <- description$new("D1")
  desc$set_collate(c("f1.R", "f2.R"), which = "main")
  desc$set_collate(c("f3.R", "f4.R"), which = "win")
  desc$add_to_collate("f5.R", which = "all")

  expect_equal(
    desc$get_collate(which = "main"),
    c("f1.R", "f2.R", "f5.R")
  )
  expect_equal(
    desc$get_collate(which = "win"),
    c("f3.R", "f4.R", "f5.R")
  )
})

test_that("deleting from non-existing collate does nothing", {

  desc <- description$new("D1")
  expect_silent(desc$del_from_collate('foo.R'))
})
