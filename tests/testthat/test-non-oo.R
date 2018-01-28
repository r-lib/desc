
context("Non OO API")

test_that("desc_add_author", {
  d <- temp_desc()
  on.exit(unlink(d))
  x <- desc_add_author(file = d, "Bunny", "Bugs", "bugs.bunny@acme.com")
  expect_match(desc_get(file = d, "Authors@R"), "Bunny")
})

test_that("desc_add_me", {
  d <- temp_desc()
  on.exit(unlink(d))
  with_mock(
    `whoami::fullname` = function() "First Last",
    `whoami::email_address` = function() "first.last@dom.com",
    x <- desc_add_me(file = d)
  )
  expect_match(desc_get(file = d, "Authors@R"), "First")
  expect_match(desc_get(file = d, "Authors@R"), "first.last@dom.com")
})

test_that("desc_add_role", {
  d <- temp_desc()
  on.exit(unlink(d))
  x <- desc_add_role(file = d, "ctb", given = "Manuel")
  expect_match(
    as.character(desc_get_author(file = d, role = "ctb")),
    "Manuel"
  )
})

test_that("desc_add_to_collate et al.", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_true("roxygenize.R" %in% desc_get_collate(file = d))
  x <- desc_del_from_collate(file = d, "roxygenize.R")
  expect_false("roxygenize.R" %in% desc_get_collate(file = d))
  x <- desc_add_to_collate(file = d, "roxygenize.R")
  expect_true("roxygenize.R" %in% desc_get_collate(file = d))
})

test_that("desc_get_maintainer, desc_change_maintainer", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_match(
    as.character(desc_get_maintainer(file = d)),
    "Hadley"
  )
  x <- desc_change_maintainer(file = d, given = "Peter")
  expect_match(
    as.character(desc_get_maintainer(file = d)),
    "Peter"
  )
})

test_that("desc_set, desc_get, desc_del", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_equal(
    desc_get(file = d, c("Package", "Version")),
    c(Package = "roxygen2", Version = "4.1.1.9000")
  )
  desc_set(file = d, Package = "foobar")
  expect_equal(
    desc_get(file = d, c("Package", "Version")),
    c(Package = "foobar", Version = "4.1.1.9000")
  )
})

test_that("desc_del_author", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_del_author(file = d, given = "Peter")
  expect_false(
    any(grepl("Peter", as.character(desc_get_authors(file = d))))
  )
})

test_that("desc_del_collate", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_del_collate(file = d)
  expect_equal(
    desc_get(file = d, "Collate"),
    c(Collate = NA_character_)
  )
})

test_that("desc_del_dep", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_true(
    "testthat" %in% desc_get_deps(file = d)$package
  )
  desc_del_dep(file = d, "testthat")
  expect_false(
    "testthat" %in% desc_get_deps(file = d)$package
  )
})

test_that("desc_del_deps", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_del_deps(file = d)
  expect_equal(nrow(desc_get_deps(file = d)), 0)
})

test_that("desc_del_role", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_del_role(file = d, "aut", given = "Manuel")
  expect_false(
    any(grepl("Manuel", desc_get_author(file = d, role = "aut")))
  )
})

test_that("desc_fields", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_true(
    all(c("Package", "Title", "Version", "URL", "Collate") %in%
        desc_fields(file = d))
  )
})

test_that("desc_has_fields", {
  d <- temp_desc()
  on.exit(unlink(d))
  expect_equal(
    desc_has_fields(file = d, c("Package", "foobar", "Collate")),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("desc_normalize", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc <- description$new(d)
  desc_normalize(file = d)
  desc$normalize()

  d2 <- tempfile()
  on.exit(unlink(d2), add = TRUE)
  desc$write(d2)
  expect_equal(readLines(d), readLines(d2))
})

test_that("desc_print", {
  d <- temp_desc()
  on.exit(unlink(d))
  capt <- capture.output(description$new("D2")$print())
  expect_output(
    desc_print(file = d),
    paste(capt, collapse = "\n"),
    fixed = TRUE
  )
})

test_that("desc_reformat_fields", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_reformat_fields(file = d)
  expect_equal(
    desc_get(file = d, "Description"),
    description$new("D2")$reformat_fields()$get("Description")
  )
})

test_that("desc_reorder_fields", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_reorder_fields(file = d)
  expect_equal(
    desc_fields(file = d),
    c("Package", "Title", "Version", "Authors@R", "Description",
      "License", "URL", "Depends", "Imports", "Suggests", "LinkingTo",
      "VignetteBuilder", "Remotes", "Encoding", "RoxygenNote", "Collate")
  )
})

test_that("desc_set_authors", {
  d <- temp_desc("D1")
  on.exit(unlink(d))
  desc_set_authors(file = d, desc_get_authors(file = "D2"))
  expect_equal(
    desc_get_authors(file = d),
    desc_get_authors("D2")
  )
})

test_that("desc_set_collate", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_set_collate(file = d, c("foo.R", "bar.R"))
  expect_equal(
    desc_get_collate(file = d),
    c("foo.R", "bar.R")
  )
})

test_that("desc_set_dep", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_set_dep(file = d, "foobar", type = "Suggests")
  expect_true(
    "foobar" %in% desc_get_deps(file = d)$package
  )
})

test_that("desc_set_deps", {
  d <- temp_desc()
  on.exit(unlink(d))
  desc_set_deps(file = d, desc_get_deps(file = "D1"))
  expect_equal(
    desc_get_deps(file = d),
    desc_get_deps(file = "D1")
  )
})

test_that("desc_to_latex", {
  expect_equal(
    desc_to_latex(file = "D2"),
    description$new("D2")$to_latex()
  )
})

test_that("desc_validate", {
  expect_warning(
    desc_validate(file = "D1"),
    "not implemented"
  )
})

test_that("can write back automatically found DESCRIPTION file", {
  dir.create(tmp <- tempfile())
  file.copy(file.path("files", "DESCRIPTION"), tmp)
  withr::with_dir(
    tmp,
    desc_set_dep("somepackage", "Suggests")
  )
  expect_true("somepackage" %in% desc_get_deps(file = tmp)$package)
})
