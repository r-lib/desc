
context("Formatting")

test_that("str orders fields", {
  desc <- description$new("!new")

  desc$del("Package")
  desc$set("Package", "foobar")

  expect_match(crayon::strip_style(desc$str()), "^Package:")
})

test_that("str formats some fields specially", {
  desc <- description$new("!new")

  desc$set("Imports", "pkg1, pkg2, \n pkg3, pkg4")
  expect_match(
    crayon::strip_style(desc$str()),
    "Imports:\n    pkg1,\n    pkg2,\n    pkg3,\n    pkg4"
  )

  desc$set("Collate", "file1.R 'file2.R' 'file with spaces.R' file4.R")
  expect_match(
    crayon::strip_style(desc$str()),
    "Collate:\n    'file1.R'\n    'file2.R'\n    'file with spaces.R'\n    'file4.R'"
  )
})

test_that("str formats authors properly", {

  desc <- description$new("D2")

  expect_equal(
    crayon::strip_style(desc$str(by_field = TRUE)[["Authors@R"]]),
    paste0(
      "Authors@R:\n    ",
      "c(person(given = \"Hadley\",\n             ",
      "family = \"Wickham\",\n             ",
      "role = c(\"aut\", \"cre\", \"cph\"),\n             ",
      "email = \"h.wickham@gmail.com\"),\n      ",
      "person(given = \"Peter\",\n             ",
      "family = \"Danenberg\",\n             ",
      "role = c(\"aut\", \"cph\"),\n             ",
      "email = \"pcd@roxygen.org\"),\n      ",
      "person(given = \"Manuel\",\n             ",
      "family = \"Eugster\",\n             ",
      "role = c(\"aut\", \"cph\")),\n      ",
      "person(given = \"RStudio\",\n             ",
      "role = \"cph\"))"
    )
  )
})

test_that("authors are printed to the screen properly", {

  desc <- description$new("D2")

  expect_output(
    print(desc),
    "(parsed):
    * Hadley Wickham <h.wickham@gmail.com> [aut, cre, cph]
    * Peter Danenberg <pcd@roxygen.org> [aut, cph]
    * Manuel Eugster [aut, cph]
    * RStudio [cph]",
    fixed = TRUE
  )

})

test_that("continuation lines", {
  desc <- description$new("D7")
  t1 <- desc$str(normalize = TRUE)
  t2 <- desc$str(normalize = FALSE)
  expect_false(grepl("\n[ \t]*\n", t1))
  expect_false(grepl("\n[ \t]*\n", t2))
})
