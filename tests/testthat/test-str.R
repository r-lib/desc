
context("Formatting")

test_that("str orders fields", {
  desc <- description$new("!new")

  desc$del("Package")
  desc$set("Package", "foobar")

  expect_match(desc$str(), "^Package:")
})

test_that("str formats some fields specially", {
  desc <- description$new("!new")

  desc$set("Imports", "pkg1, pkg2, \n pkg3, pkg4")
  expect_match(
    desc$str(),
    "Imports:\n    pkg1,\n    pkg2,\n    pkg3,\n    pkg4"
  )

  desc$set("Collate", "file1.R 'file2.R' 'file with spaces.R' file4.R")
  expect_match(
    desc$str(),
    "Collate:\n    'file1.R'\n    'file2.R'\n    'file with spaces.R'\n    'file4.R'"
  )
})
