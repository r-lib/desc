
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

  desc$add_author("Gábor", "Csárdi", email = "csardi.gabor@gmail.com",
                  role = "ctb", comment = "Really?")

  expect_identical(
    format(desc$get_authors()[5]),
    "Gábor Csárdi <csardi.gabor@gmail.com> [ctb] (Really?)"
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

  desc$add_author("Gábor", "Csárdi", email = "csardi.gabor@gmail.com",
                  role = "ctb", comment = "Really?")
  desc$add_role(given = "Gábor", role = "cph")

  expect_identical(
    format(desc$get_authors()[5]),
    "Gábor Csárdi <csardi.gabor@gmail.com> [ctb, cph] (Really?)"
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
  desc <- description$new("D2")

  desc$add_author("Gábor", "Csárdi", email = "csardi.gabor@gmail.com",
                  role = "ctb", comment = "Really?")
  desc$add_role(given = "Gábor", role = "cph")
  desc$del_role(family = "Csárdi", role = "ctb")

  expect_identical(
    format(desc$get_authors()[5]),
    "Gábor Csárdi <csardi.gabor@gmail.com> [cph] (Really?)"
  )
})

test_that("we can change the maintainer", {
  desc <- description$new("D2")

  desc$change_maintainer(given = "Peter")

  ans <- c(
    person("Hadley", "Wickham", email = "h.wickham@gmail.com",
           role = c("aut", "cph")),
    person("Peter", "Danenberg", email = "pcd@roxygen.org",
           role = c("aut", "cph", "cre")),
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)

})

test_that("add_me works", {
  desc <- description$new("D2")
  with_mock(
    `desc:::check_for_package` = function(...) TRUE,
    `whoami::fullname` = function() "Bugs Bunny",
    `whoami::email_address` = function() "bugs.bunny@acme.com",
    desc$add_me(comment = "Yikes!")
  )

  expect_identical(
    format(desc$get_authors()[5]),
    "Bugs Bunny <bugs.bunny@acme.com> [ctb] (Yikes!)"
  )
})

test_that("error if not Authors@R field", {

  desc <- description$new("D1")
  expect_error(
    desc$get_authors(),
    "No 'Authors@R' field"
  )
})

test_that("message if not author to delete does not exist", {

  desc <- description$new("D2")
  expect_message(
    desc$del_author(given = "Gábor"),
    "Could not find author to remove"
  )
})

test_that("get_author is OK", {

  D2 <- description$new("D2")

  expect_equal(
    D2$get_author(role = "cre"),
    person(
      given = "Hadley",
      family = "Wickham",
      email = "h.wickham@gmail.com",
      role = c("aut", "cre", "cph"),
      comment = NULL
    )
  )

  expect_equal(
    D2$get_author(role = "aut"),
    D2$get_authors()[1:3]
  )

  D1 <- description$new("D1")
  expect_null(D1$get_author(role = "cre"))
})

test_that("get_maintainer is OK, too", {

  D1 <- description$new("D1")
  expect_equal(
    D1$get_maintainer(),
    "G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com>"
  )

  D2 <- description$new("D2")
  expect_equal(
    D2$get_maintainer(),
    "Hadley Wickham <h.wickham@gmail.com>"
  )

  D1$del("Maintainer")
  expect_equal(
    D1$get_maintainer(),
    NA_character_
  )
})

test_that("add_author if there is no Authors@R field", {
  D1 <- description$new("D1")
  D1$add_author("Gabor", "Csardi", "csardi.gabor@gmail.com", role = "ctb")
  expect_identical(
    format(D1$get_authors()[1]),
    "Gabor Csardi <csardi.gabor@gmail.com> [ctb]"
  )
})

test_that("add myself if there is no Authors@R field", {
  D1 <- description$new("D1")
  with_mock(
    `desc:::check_for_package` = function(...) TRUE,
    `whoami::fullname` = function() "Bugs Bunny",
    `whoami::email_address` = function() "bugs.bunny@acme.com",
    D1$add_me(comment = "Yikes!")
  )

  expect_identical(
    format(D1$get_authors()[1]),
    "Bugs Bunny <bugs.bunny@acme.com> [ctb] (Yikes!)"
  )
})
