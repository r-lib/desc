
context("Syntax checks")


test_that("checking other fields is always ok", {

  desc <- description$new("D1")
  desc$set("Note", "Blah-blah")

  expect_equal(desc$get("Note"), c(Note = "Blah-blah"))
})


test_that("we catch syntax errors", {

  desc <- description$new("D1")

  expect_warning(
    desc$set("Package", "444!!! not a valid package name"),
    "must only contain ASCII letters.*must start with a letter"
  )

  expect_warning(
    desc$set("Package", "4aaaa"),
    "must start with a letter"
  )
})


test_that("generic field check is always true", {
  field <- list(key = "XY", value = "foobar")
  class(field) <- "DescriptionField"
  expect_silent(check_field(field, warn = TRUE))
  expect_true(check_field(field))
})


test_chk <- function(field, value, warn_msg, warn = TRUE, class = field) {
  rec <- list(key = field, value = value)
  class(rec) <- c(paste0("Description", class), "DescriptionField")

  test_that(paste(field, "syntax"), {
    if (warn) {
      expect_warning(check_field(rec, warn = TRUE), warn_msg)
    } else {
      expect_silent(check_field(rec, warn = TRUE))
    }
  })
}

test_chk("License", "GPL", warn = FALSE)
test_chk("License", "", "must not be empty")
test_chk("License", "almost good but \x80", "only ASCII characters")

test_chk("Title", "This is a Good Title", warn = FALSE)
test_chk("Title", "", "must not be empty")
test_chk("Title", "Not a good title.", "must not end with a period")
test_chk("Title", "Good title by Bugs Bunny et al.", warn = FALSE)

test_chk("Maintainer", "Bugs Bunny <bb@acme.com>", warn = FALSE)
test_chk("Maintainer", "ORPHANED", warn = FALSE)
test_chk("Maintainer", "", "must not be empty")
test_chk("Maintainer", "foobar", "must contain an email address")

test_chk("RepoList", "", warn = FALSE)
test_chk("RepoList", "http://cran.rstudio.com", warn = FALSE)
test_chk("RepoList", "http://cran.rstudio.com, https://cran.r-project.org",
         warn = FALSE)
test_chk("RepoList", "foobar", "must be a comma .* repository URLs")
test_chk("RepoList", "http://foo.bar http://fobar", "must be a comma")

test_chk("URL", "http://acme.com", warn = FALSE)
test_chk("URL", "not this one", "must be a http, https or ftp URL")

test_chk("URLList", "", warn = FALSE)
test_chk("URLList", "http://cran.rstudio.com", warn = FALSE)
test_chk("URLList", "http://cran.rstudio.com, https://cran.r-project.org",
         warn = FALSE)
test_chk("URLList", "foobar", "must be a comma .* URLs")
test_chk("URLList", "http://foo.bar http://fobar", "must be a comma")

test_chk("Priority", "recommended", warn = FALSE)
test_chk("Priority", "foobar", "must be one of")

test_chk("LazyLoad", "yes", warn = FALSE, class = "Logical")
test_chk("LazyLoad", "foobar", "must be one of", class = "Logical")

test_chk("VignetteBuilder", "devtools, knitr", warn = FALSE,
         class = "PackageList")
test_chk("VignetteBuilder", "this is not", "must be a comma",
         class = "PackageList")

test_chk("Encoding", "UTF-8", warn = FALSE)
test_chk("Encoding", "foobar", "must be one of")

test_chk("OSType", "unix", warn = FALSE)
test_chk("OSType", "foobar", "must be one of")

test_chk("Type", "Translation", warn = FALSE)
test_chk("Type", "foobar", "must be either")

test_chk("Classification", "", warn = FALSE)

test_chk("Language", "hun, deu", warn = FALSE)
test_chk("Language", "this is not", "must be a list of IETF language")

test_chk("Date", "2015-01-21", warn = FALSE)
test_chk("Date", "foobar", "must be an ISO date")

test_chk("Compression", "gzip", warn = FALSE)
test_chk("Compression", "foobar", "must be one of")

test_chk("Repository", "", warn = FALSE)

test_chk("AddedByRCMD", "", warn = FALSE)
