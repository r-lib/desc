
context("Utility functions")

test_that("check_for_package works", {

  expect_true(check_for_package("utils"))

  expect_error(
    check_for_package("foobarfoobarfoobar"),
    "Package 'foobarfoobarfoobar' is needed"
  )
  
})

test_that("is_ascii", {

  expect_true(is_ascii(""))
  expect_true(is_ascii("a"))
  expect_true(is_ascii(rawToChar(as.raw(127))))

  expect_equal(
    is_ascii(character()),
    logical()
  )

  expect_equal(
    is_ascii(c("a", "b")),
    c(TRUE, TRUE)
  )

  expect_false(is_ascii(rawToChar(as.raw(128))))
})

test_that("is_url", {

  expect_true(is_url("http://acme.com"))
  expect_true(is_url("https://acme.com"))
  expect_true(is_url("ftp://this.is.it"))

  expect_equal(is_url(character()), logical())

  expect_false(is_url(""))
  expect_false(is_url("this.is.not"))
  expect_false(is_url("http://"))
  expect_false(is_url("http:/this.is.no"))

  expect_equal(
    is_url(c("http://acme.com", "http://index.me")),
    c(TRUE, TRUE)
  )

  expect_equal(
    is_url(c("foo", "https://foo.bar")),
    c(FALSE, TRUE)
  )
})

test_that("is_url_list", {

  expect_true(is_url_list(""))
  expect_true(is_url_list("http://foo.bar"))

  expect_false(is_url_list("this is not it"))
  expect_false(is_url_list("http://so.far.so.good, oh, no!"))
})
