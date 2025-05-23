test_that("we can get the authors", {
  desc <- description$new(test_path("D2"))

  ans <- c(
    person(
      "Hadley",
      "Wickham",
      email = "h.wickham@gmail.com",
      role = c("aut", "cre", "cph")
    ),
    person(
      "Peter",
      "Danenberg",
      email = "pcd@roxygen.org",
      role = c("aut", "cph")
    ),
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)
})

test_that("we can set the authors", {
  desc1 <- description$new(test_path("D1"))
  desc2 <- description$new(test_path("D2"))

  desc1$set_authors(desc2$get_authors())

  expect_identical(desc1$get_authors(), desc2$get_authors())
})

test_that("we can add an author", {
  desc <- description$new(test_path(test_path("D2")))

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = "Really?"
  )

  expect_identical(
    format(desc$get_authors()[5]),
    "G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com> [ctb] (Really?)"
  )
})

test_that("we can add an author with ORCID via comment", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  desc <- description$new(cmd = "!new")

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = c(ORCID = "0000-0001-7098-9676", what = "he did it")
  )

  expect_snapshot(
    format(desc$get_authors()[2]),
    variant = if (getRversion() >= "4.5.0") "R45" else "R44"
  )
})

test_that("we can add an author with ORCID", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  desc <- description$new(cmd = "!new")

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = c(what = "he did it"),
    orcid = "0000-0001-7098-9676"
  )

  expect_snapshot(
    format(desc$get_authors()[2]),
    variant = if (getRversion() >= "4.5.0") "R45" else "R44"
  )
})

test_that("we cannot add an author with malformatted comment", {
  desc <- description$new(cmd = "!new")

  expect_snapshot(
    error = TRUE,
    desc$add_author(
      "G\u00e1bor",
      "Cs\u00e1rdi",
      email = "csardi.gabor@gmail.com",
      role = "ctb",
      comment = c(ORCID = "orcid_number", what = NA)
    )
  )
})

test_that("we can search for authors", {
  desc <- description$new(test_path("D9"))
  authors <- desc$get_authors()

  expect_equal(
    search_for_author(authors, given = "Hadley")$index,
    1L
  )

  expect_equal(
    search_for_author(authors, orcid = "0000-0003-4757-117X")$index,
    1L
  )

  expect_equal(
    search_for_author(authors, orcid = "117X")$index,
    1L
  )
})

test_that("we can add a role to an author", {
  desc <- description$new(test_path("D2"))

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = "Really?"
  )
  desc$add_role(given = "G\u00e1bor", role = "cph")

  expect_identical(
    format(desc$get_authors()[5]),
    "G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com> [ctb, cph] (Really?)"
  )
})

test_that("we can add an ORCID to an author", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  desc <- description$new(test_path("D2"))

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = "Really?"
  )
  desc$add_orcid(given = "G\u00e1bor", orcid = "0000-0001-7098-9676")

  expect_snapshot(
    format(desc$get_authors()[5]),
    variant = if (getRversion() >= "4.5.0") "R45" else "R44"
  )
})

test_that("we can replace the ORCID of an author", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  desc <- description$new(test_path("D9"))

  desc$add_orcid(given = "Hadley", orcid = "1000-0003-4757-117X")

  expect_snapshot(
    format(desc$get_authors()[1]),
    variant = if (getRversion() >= "4.5.0") "R45" else "R44"
  )
})

test_that("we cannot add the same ORCID to more than one author", {
  desc <- description$new(test_path("D10"))

  expect_snapshot(
    error = TRUE,
    desc$add_orcid(given = "Peter", orcid = "orcidid")
  )

  expect_snapshot(
    error = TRUE,
    desc$add_orcid(given = "Manuel", orcid = "0000-0003-4757-117X")
  )
})

test_that("we can add an ROR to an author", {
  desc <- description$new(test_path("D2"))

  desc$add_author(
    "Posit Software, PBC",
    role = c("cph", "fnd"),
    comment = "Really?"
  )
  desc$add_ror(given = "Posit", ror = "03wc8by49")

  expect_match(
    format(desc$get_authors()[5]),
    "03wc8by49"
  )
})

test_that("we can replace the ROR of an author", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  desc <- description$new(test_path("D9"))

  expect_match(
    format(desc$get_authors()[4]),
    "03wc8by49"
  )

  desc$add_ror(given = "Posit Software, PBC", ror = "012345678")

  expect_match(
    format(desc$get_authors()[4]),
    "012345678"
  )
})

test_that("we cannot add the same ROR to more than one author", {
  desc <- description$new(test_path("D10"))

  expect_snapshot(
    error = TRUE,
    desc$add_ror(given = "Peter", ror = "bla")
  )

  expect_snapshot(
    error = TRUE,
    desc$add_ror(given = "Manuel", ror = "012345678")
  )
})


test_that("we can delete an author", {
  desc <- description$new(test_path("D2"))

  expect_message(desc$del_author(given = "Hadley"), "removed:")
  expect_message(desc$del_author(family = "Danenberg"), "removed:")

  ans <- c(
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)
})

test_that("we can delete a role", {
  desc <- description$new(test_path("D2"))

  desc$add_author(
    "G\u00e1bor",
    "Cs\u00e1rdi",
    email = "csardi.gabor@gmail.com",
    role = "ctb",
    comment = "Really?"
  )
  desc$add_role(given = "G\u00e1bor", role = "cph")
  desc$del_role(family = "Cs\u00e1rdi", role = "ctb")

  expect_identical(
    format(desc$get_authors()[5]),
    "G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com> [cph] (Really?)"
  )
})

test_that("we can change the maintainer", {
  desc <- description$new(test_path("D2"))

  desc$change_maintainer(given = "Peter")

  ans <- c(
    person(
      "Hadley",
      "Wickham",
      email = "h.wickham@gmail.com",
      role = c("aut", "cph")
    ),
    person(
      "Peter",
      "Danenberg",
      email = "pcd@roxygen.org",
      role = c("aut", "cph", "cre")
    ),
    person("Manuel", "Eugster", role = c("aut", "cph")),
    person("RStudio", role = "cph")
  )

  expect_identical(desc$get_authors(), ans)
})

test_that("add_me works", {
  withr::local_envvar(
    FULLNAME = "Bugs Bunny",
    EMAIL = "bugs.bunny@acme.com",
    ORCID_ID = ""
  )

  desc <- description$new(test_path("D2"))
  desc$add_me(comment = "Yikes!")

  expect_identical(
    format(desc$get_authors()[5]),
    "Bugs Bunny <bugs.bunny@acme.com> [ctb] (Yikes!)"
  )
})

test_that("add_me can use ORCID_ID", {
  R_version <- paste(R.version$major, R.version$minor, sep = ".")

  skip_if_not(R_version >= "3.5.0")

  withr::local_envvar(
    FULLNAME = "Bugs Bunny",
    EMAIL = "bugs.bunny@acme.com",
    ORCID_ID = "0000-0002-0775-162X"
  )

  desc <- description$new(test_path("D2"))
  desc$add_me()

  expect_snapshot(
    format(desc$get_authors()[5]),
    variant = if (getRversion() >= "4.5.0") "R45" else "R44"
  )
})

test_that("add_author_gh works", {
  withr::local_options(
    desc.gh_user = list(name = "Jeroen Ooms", email = "notanemail")
  )

  desc <- description$new(test_path("D2"))
  desc$add_author_gh(username = "jeroen")

  expect_identical(
    format(desc$get_authors()[5]),
    "Jeroen Ooms <notanemail> [ctb]"
  )
})

test_that("error if not Authors@R field", {
  desc <- description$new(test_path("D1"))
  expect_snapshot(
    error = TRUE,
    desc$get_authors()
  )
})

test_that("message if not author to delete does not exist", {
  desc <- description$new(test_path("D2"))
  expect_message(
    desc$del_author(given = "G\u00e1bor"),
    "Could not find author to remove"
  )
})

test_that("get_author is OK", {
  D2 <- description$new(test_path("D2"))

  expect_equal(
    D2$get_author(role = "cre"),
    person(
      given = "Hadley",
      family = "Wickham",
      email = "h.wickham@gmail.com",
      role = c("aut", "cre", "cph"),
      comment = NULL
    )
  )

  expect_equal(
    D2$get_author(role = "aut"),
    D2$get_authors()[1:3]
  )

  D1 <- description$new(test_path("D1"))
  expect_null(D1$get_author(role = "cre"))
})

test_that("get_maintainer is OK, too", {
  D1 <- description$new(test_path("D1"))
  expect_equal(
    D1$get_maintainer(),
    "G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com>"
  )

  D2 <- description$new(test_path("D2"))
  expect_equal(
    D2$get_maintainer(),
    "Hadley Wickham <h.wickham@gmail.com>"
  )

  D1$del("Maintainer")
  expect_equal(
    D1$get_maintainer(),
    NA_character_
  )
})

test_that("coerce_authors_at_r if there is no Authors@R field", {
  D1 <- description$new(test_path("D1"))
  expect_snapshot(error = TRUE, D1$get_authors())
  expect_silent(D1$coerce_authors_at_r())
  expect_silent(auth <- D1$get_authors())
  expect_equal(
    auth,
    as.person("G\u00e1bor Cs\u00e1rdi <csardi.gabor@gmail.com> [aut, cre]")
  )
  # author and maintainer are the same in this case
  expect_equal(D1$get_author("cre"), D1$get_author("aut"))
})

test_that("coerce_authors_at_r does nothing if there IS an Authors@R field", {
  D2 <- description$new(test_path("D2"))
  expect_null(D2$coerce_authors_at_r())
})

test_that("coerce_authors_at_r errors if no authors fields at all", {
  D1 <- description$new(test_path("D1"))
  D1$del("Author")
  expect_snapshot(
    error = TRUE,
    D1$coerce_authors_at_r()
  )
})

test_that("coerce_authors_at_r with multiple authors in Author: field", {
  D6 <- description$new(test_path("D6"))
  expect_silent(D6$coerce_authors_at_r())
  expect_equal(
    D6$get_author("cre"),
    as.person("Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]")
  )
  a <- D6$get_author("aut")
  expect_equal(
    a[1],
    as.person("Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]")
  )
  expect_equal(a[2], as.person("John Muschelli [aut]"))
})

test_that("coerce_authors_at_r handles role tags, #114", {
  D13 <- description$new(test_path("D13"))
  expect_silent(D13$coerce_authors_at_r())
  # Joe Developer is a maintainer
  expect_equal(
    D13$get_author("cre"),
    as.person("Joe Developer <Joe.Developer@some.domain.net> [aut, cre]")
  )
  # Joe and Pat Developer are authors
  a <- D13$get_author("aut")
  expect_length(a, 2)
  expect_equal(
    a[1],
    as.person("Joe Developer <Joe.Developer@some.domain.net> [aut, cre]")
  )
  expect_equal(a[2], as.person("Pat Developer [aut]"))
  # "A. User" is a contributor
  expect_equal(D13$get_author("ctb"), as.person("A. User [ctb]"))
})

test_that("coerce_authors_at_r ignores reference to AUTHOR files, #114", {
  D14 <- description$new("D14")
  expect_message(D14$coerce_authors_at_r(), "AUTHOR file")
  expect_equal(
    D14$get_authors(),
    as.person("Gábor Csárdi <csardi.gabor@gmail.com> [aut, cre]")
  )
})

test_that("coerce_authors_at_r handles maintainer not being author", {
  D15 <- description$new(test_path("D15"))
  expect_silent(D15$coerce_authors_at_r())
  expect_snapshot({
    D15$get_author("cre")
    D15$get_author("aut")
  })
})

test_that("coerce_authors_at_r handles maintainer not being author", {
  D17 <- description$new(test_path("D17"))
  expect_silent(D17$coerce_authors_at_r())
  expect_snapshot({
    D17$get_author("cre")
    D17$get_author("aut")
  })
})

test_that("add_author if there is no Authors@R field", {
  D1 <- description$new(test_path("D1"))
  D1$add_author("Gabor", "Csardi", "csardi.gabor@gmail.com", role = "ctb")
  expect_identical(
    format(D1$get_authors()[1]),
    "Gabor Csardi <csardi.gabor@gmail.com> [ctb]"
  )
})

test_that("add myself if there is no Authors@R field", {
  withr::local_envvar(
    FULLNAME = "Bugs Bunny",
    EMAIL = "bugs.bunny@acme.com",
    ORCID_ID = ""
  )

  D1 <- description$new(test_path("D1"))
  D1$add_me(comment = "Yikes!")

  expect_identical(
    format(D1$get_authors()[1]),
    "Bugs Bunny <bugs.bunny@acme.com> [ctb] (Yikes!)"
  )
})

test_that("normalization keeps authors in UTF-8", {
  D11 <- description$new(test_path("D11"))
  expect_equal(
    Encoding(private(D11)$data$`Authors@R`$value),
    "UTF-8"
  )
  D11$normalize()
  expect_equal(
    Encoding(private(D11)$data$`Authors@R`$value),
    "UTF-8"
  )
})

test_that("long comments are deparsed properly", {
  authors <- c(
    person(
      given = "Josiah",
      family = "Carberry",
      role = c("aut", "cre"),
      email = "flast@email.org",
      comment = c(
        ORCID = "0000-0002-1825-0097",
        affiliation = "Brown University"
      )
    ),
    person(
      given = "Second",
      family = "Last",
      role = "aut",
      email = "slast@email.org",
      comment = c(
        ORCID = "0000-0002-1825-0097",
        affiliation = c("University One", "University Two")
      )
    )
  )

  desc <- desc::description$new("!new")
  desc$set_authors(authors)

  expect_equal(
    desc$get_authors()[[1]]$comment,
    authors[[1]]$comment
  )
  expect_equal(
    desc$get_authors()[[2]]$comment,
    authors[[2]]$comment
  )
})

test_that("deparse_authors_at_r", {
  # old R deparses named vectors differently
  if (getRversion() < "3.5") skip("Needs newer R")

  ppl <- c(
    person(
      "Hadley",
      "Wickham",
      ,
      "hadley@rstudio.com",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0003-4757-117X")
    ),
    person(
      "Jennifer",
      "Bryan",
      ,
      "jenny@rstudio.com",
      role = "aut",
      comment = c(ORCID = "0000-0002-6983-2759")
    ),
    person("RStudio", role = c("cph", "fnd"))
  )

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  desc <- description$new(text = "")

  desc$set_authors(ppl[[1]])
  desc$write(tmp)
  expect_snapshot(readLines(tmp))

  desc$set_authors(ppl)
  desc$write(tmp)
  expect_snapshot(readLines(tmp))
})
