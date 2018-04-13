
context("REMOTE")

test_that("get, set, etc. remotes", {
  desc <- description$new("D2")
  expect_identical(
    desc$get_remotes(),
    c("foo/digest",
      "svn::https://github.com/hadley/stringr",
      "local::/pkgs/testthat"
      )
  )

  desc$set_remotes(c("bar/knitr", "local::/pkgs/Rcpp"))
  expect_identical(desc$get_remotes(), c("bar/knitr", "local::/pkgs/Rcpp"))

  desc$add_remotes("github::brewer/brew")
  expect_identical(
    desc$get_remotes(),
    c("bar/knitr", "local::/pkgs/Rcpp", "github::brewer/brew")
  )

  desc$del_remotes("^local::")
  expect_identical(
    desc$get_remotes(),
    c("bar/knitr", "github::brewer/brew")
  )

  desc$clear_remotes()
  expect_identical(desc$get_remotes(), character())

  desc$add_remotes("hadley/stringr")
  expect_identical(desc$get_remotes(), "hadley/stringr")

  desc$del_remotes("stringr")
  expect_identical(desc$get_remotes(), character())
  expect_identical(desc$get("Remotes"), c(Remotes = NA_character_))

  expect_warning(
    desc$set("Remotes", "my remote"),
    "must be a comma separated list of remotes"
  )
})
