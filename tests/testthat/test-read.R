test_that("DCF reader works", {
  desc <- description$new(test_path("D1"))

  expect_equal(desc$get("Package"), c(Package = "desc"))
  expect_equal(desc$get("Version"), c(Version = "1.0.0"))
  expect_equal(desc$get("Author"), c(Author = "G\u00e1bor Cs\u00e1rdi"))
  expect_equal(desc$get("Imports"), c(Imports = "\n    R6"))
})

test_that("DCF reader keeps whitespace", {
  desc <- description$new(test_path("D1"))

  expect_equal(desc$get("Suggests"), c(Suggests = "\n    testthat"))
  expect_equal(
    desc$get("Description"),
    c(
      Description = paste0(
        "Tools to read, write, create, and manipulate DESCRIPTION\n",
        "    files. It is intented for packages that create or manipulate other\n",
        "    packages."
      )
    )
  )
})

test_that("duplicate fields, #43", {
  expect_snapshot(
    error = TRUE,
    description$new(test_path("D5"))
  )
})

test_that("empty lines error", {
  expect_snapshot(
    error = TRUE,
    description$new(test_path("D12"))
  )
})

test_that("Empty DESCRIPTION", {
  expect_error(description$new(text = ""), NA)
  expect_error(description$new(text = character()), NA)
})
