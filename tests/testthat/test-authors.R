
context("Authors")

test_that("we can get the authors", {
  desc <- description$new("D2")

  ans <- c(
    person("Hadley", "Wickham", email = "h.wickham@gmail.com",
           role = c("aut", "cre", "cph")),
    person("Peter", "Danenberg", email = "pcd@roxygen.org",
           role = c("aut", "cph")),
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)
})

test_that("we can set the authors", {

  desc1 <- description$new("D1")
  desc2 <- description$new("D2")

  desc1$set_authors(desc2$get_authors())

  expect_identical(desc1$get_authors(), desc2$get_authors())
})

test_that("we can add an author", {
  desc <- description$new("D2")

  desc$add_author("Gabor", "Csardi", email = "csardi.gabor@gmail.com",
                  role = "ctb", comment = "Really?")

  expect_identical(
    format(desc$get_authors()[5]),
    "Gabor Csardi <csardi.gabor@gmail.com> [ctb] (Really?)"
  )
})

test_that("we can search for authors", {
  desc <- description$new("D2")
  authors <- desc$get_authors()

  expect_equal(
    search_for_author(authors, given = "Hadley")$index,
    1L
  )

})

test_that("we can add a role to an author", {
  desc <- description$new("D2")

  desc$add_author("Gabor", "Csardi", email = "csardi.gabor@gmail.com",
                  role = "ctb", comment = "Really?")
  desc$add_role(given = "Gabor", role = "cph")

  expect_identical(
    format(desc$get_authors()[5]),
    "Gabor Csardi <csardi.gabor@gmail.com> [ctb, cph] (Really?)"
  )
})

test_that("we can delete an author", {
  desc <- description$new("D2")

  desc$del_author(given = "Hadley")
  desc$del_author(family = "Danenberg")

  ans <- c(
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)
})

test_that("we can delete a role", {

})

test_that("we can change the maintainer", {

})

test_that("add_me works", {

})

test_that("we can convert to Authors@R", {

})
