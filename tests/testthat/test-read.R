
context("DCF reader")

test_that("DCF reader works", {
  desc <- description$new("D1")

  expect_equal(desc$get("Package"), c(Package = "description"))
  expect_equal(desc$get("Version"), c(Version = "1.0.0"))
  expect_equal(desc$get("Author"), c(Author = "Gabor Csardi"))
  expect_equal(desc$get("Imports"), c(Imports = "\n    R6"))
})

test_that("DCF reader keeps whitespace", {
  desc <- description$new("D1")

  expect_equal(desc$get("Suggests"), c(Suggests = "\n    testthat"))
  expect_equal(desc$get("Description"), c(
    Description = paste0(
      "Tools to read, write, create, and manipulate DESCRIPTION\n",
      "    files. It is intented for packages that create or manipulate other\n",
      "    packages."
    )
  ))

})
