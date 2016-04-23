
context("Encoding")

test_that("Encoding field is observed", {

  expect_silent(
    desc <- description$new(
      text = paste0(
        "Package: foo\n",
        "Title: Foo Package\n",
        "Author: Package Author\n",
        "Maintainer: Package Author <author@here.net>\n",
        "Description: The great foo package.\n",
        "License: GPL\n",
        "Encoding: UTF-8\n"
      )
    )
  )

  expect_silent(desc$set(Author = "Gábor Csárdi"))
})

test_that("Unset Encoding results warning", {

  expect_warning(
    desc <- description$new(
      text = paste0(
        "Package: foo\n",
        "Title: Foo Package\n",
        "Author: Package Author\n",
        "Maintainer: Gábor Csárdi <author@here.net>\n",
        "Description: The great foo package.\n",
        "License: GPL\n"
      )
    ),
    "Encoding"
  )

  expect_warning(desc$set(Author = "Gábor Csárdi"), "Encoding")

})
