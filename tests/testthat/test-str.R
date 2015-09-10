
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

  desc$set("Collate", "file1 'file2' 'file with spaces' file4")
  expect_match(
    desc$str(),
    "Collate:\n    'file1'\n    'file2'\n    'file with spaces'\n    'file4'"
  )
})
