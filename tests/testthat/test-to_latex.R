
test_that("Test expected LaTeX output", {
  desc <- description$new("!new")

  desc$set_authors(person("Kirill", "M\u{00fc}ller", email = "aaa@bbb.xx", role = "cre"))
  desc$set(Package = "Test.Package",
           Title = "Must be in Title Case",
           Description = "Words not in the dictionary must be 'quoted'. \"Double quotes\" can also be used, as well as \\_{}[]^$#%. Must end with a full stop.",
           License = "CC-BY-SA",
           URL = "http://somewhere.io, http://somewhereel.se",
           BugReports = "http://somewhere.io/trac")

  new <- capture.output(print(desc$to_latex()))
  old <- readLines(test_path("output/to_latex.tex"), encoding = "UTF-8")

  new1 <- enc2native(new)
  old1 <- enc2native(old)

  expect_equal(new1, old1)
})
