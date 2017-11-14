context("to_latex")

test_that("Test expected LaTeX output", {
  desc <- description$new("!new")

  desc$set_authors(person("Kirill", "Müller", email = "aaa@bbb.xx", role = "cre"))
  desc$set(Package = "Test.Package",
           Title = "Must be in Title Case",
           Description = "Words not in the dictionary must be 'quoted'. \"Double quotes\" can also be used, as well as \\_{}[]^$#%. Must end with a full stop.",
           License = "CC-BY-SA",
           URL = "http://somewhere.io, http://somewhereel.se",
           BugReports = "http://somewhere.io/trac")

  expect_output_file(print(desc$to_latex()), "output/to_latex.tex", update = FALSE)
})
