
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

test_that("different encoding is converted to UTF-8 when reading", {

  expect_silent(
    desc <- description$new(
      text = paste0(
        "Package: foo\n",
        "Title: Foo Package\n",
        "Author: Package Author\n",
        "Maintainer: G\xE1bor Cs\xE1rdi <author@here.net>\n",
        "Description: The great foo package.\n",
        "License: GPL\n",
        "Encoding: latin1\n"
      )
    )
  )

  expect_equal(Encoding(desc$get("Maintainer")), "UTF-8")
  expect_silent(desc$set(Author = "G\u00e1bor Cs\u00e1rdi"))
})

test_that("encoding is converted to specified when writing", {

  desc <- description$new(
    text = paste0(
      "Package: foo\n",
      "Title: Foo Package\n",
      "Author: Package Author\n",
      "Maintainer: G\xE1bor Cs\xE1rdi <author@here.net>\n",
      "Description: The great foo package.\n",
      "License: GPL\n",
      "Encoding: latin1\n"
    )
  )

  desc$write(file = tmp <- tempfile())
  on.exit(unlink(tmp))
  tmpf <- file(tmp, encoding = "", open = "r")
  lines <- readLines(tmpf, encoding = "latin1")
  close(tmpf)
  maint <- grep("^Maintainer", lines, value = TRUE)

  exp <- "Maintainer: G\xE1bor Cs\xE1rdi <author@here.net>"
  Encoding(exp) <- "latin1"
  expect_equal(charToRaw(maint), charToRaw(exp))
})
