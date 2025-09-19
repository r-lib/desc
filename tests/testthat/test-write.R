test_that("can write to file", {
  desc <- description$new("!new")
  tmp <- tempfile()
  desc$write(tmp)

  desc2 <- description$new(tmp)
  expect_equal(desc$str(), desc2$str())
})

test_that("normalization while writing to file", {
  desc <- description$new("!new")
  desc$set("Imports", "foo, bar, foobar")

  tmp <- tempfile()
  desc$write(tmp)

  desc$normalize()

  desc2 <- description$new(tmp)
  expect_equal(desc$str(), desc2$str())
})

test_that("whitespace after : if field was updated", {
  on.exit(unlink(c(t1, t2), recursive = TRUE), add = TRUE)

  ## Space is kept if it was there
  desc <- description$new(text = "Imports: \n    one,\n    two\n")
  desc$set_dep("ugh")
  desc$write(t1 <- tempfile())

  expect_equal(
    readLines(t1),
    c("Imports: ", "    one,", "    two,", "    ugh")
  )

  ## Space is not added if it was not there
  desc <- description$new(text = "Imports:\n    one,\n    two\n")
  desc$set_dep("ugh")
  desc$write(t2 <- tempfile())

  expect_equal(
    readLines(t2),
    c("Imports:", "    one,", "    two,", "    ugh")
  )
})

test_that("auto-tidy when Config/desc/tidy is true", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a desc with auto-tidy enabled and messy formatting
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "true")

  # Will be reordered by normalize
  desc$set("Imports", "zzz, aaa, mmm")
  desc$set("Title", "A Package")

  # Write should auto-tidy (normalize)
  desc$write(tmp)

  # Read back and check if it's normalized
  desc2 <- description$new(tmp)
  desc_normalized <- description$new("!new")
  desc_normalized$set("Config/desc/tidy", "true")
  desc_normalized$set("Imports", "zzz, aaa, mmm")
  desc_normalized$set("Title", "A Package")
  desc_normalized$normalize()

  expect_equal(desc2$str(), desc_normalized$str())
})

test_that("no auto-tidy when Config/desc/tidy is false", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a desc with auto-tidy disabled
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "false")
  desc$set("Imports", "zzz, aaa, mmm")
  desc$set("Title", "A Package")

  # Store original state before writing
  original_str <- desc$str()

  # Write should NOT auto-tidy
  desc$write(tmp)

  # Read back and check it matches original (not normalized)
  desc2 <- description$new(tmp)
  expect_equal(desc2$str(), original_str)
})

test_that("no auto-tidy when Config/desc/tidy is missing", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a desc without the config field
  desc <- description$new("!new")
  desc$set("Imports", "zzz, aaa, mmm")
  desc$set("Title", "A Package")

  # Store original state
  original_str <- desc$str()

  # Write should NOT auto-tidy
  desc$write(tmp)

  # Read back and check it matches original
  desc2 <- description$new(tmp)
  expect_equal(desc2$str(), original_str)
})

test_that("Config/desc/tidy boolean parsing is case-insensitive", {
  test_cases <- list(
    list("true", TRUE),
    list("TRUE", TRUE),
    list("yes", TRUE),
    list("1", TRUE),
    list("false", FALSE),
    list("FALSE", FALSE),
    list("no", FALSE),
    list("0", FALSE),
    list("invalid", FALSE),
    list("", FALSE)
  )

  for (case in test_cases) {
    desc <- description$new("!new")
    desc$set("Config/desc/tidy", case[[1]])
    expect_equal(
      desc$get_config("Config/desc/tidy"),
      case[[2]],
      info = paste("Failed for value:", case[[1]])
    )
  }
})

test_that("get_config method works correctly", {
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "true")
  desc$set("Config/other/setting", "false")

  expect_true(desc$get_config("Config/desc/tidy"))
  expect_false(desc$get_config("Config/other/setting"))
  expect_false(desc$get_config("Config/nonexistent"))
})

test_that("non-OO API respects auto-tidy config", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a DESCRIPTION with auto-tidy enabled
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "true")
  desc$set("Imports", "zzz, aaa, mmm")
  desc$write(tmp)

  # Use non-OO API to modify it
  desc_set("Title", "Modified Title", file = tmp)

  # Read back and verify it's still normalized
  desc2 <- description$new(tmp)
  desc_expected <- description$new("!new")
  desc_expected$set("Config/desc/tidy", "true")
  desc_expected$set("Imports", "zzz, aaa, mmm")
  desc_expected$set("Title", "Modified Title")
  desc_expected$normalize()

  expect_equal(desc2$str(), desc_expected$str())
})

test_that("no double normalization with normalize=TRUE and auto-tidy", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a DESCRIPTION with auto-tidy enabled
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "true")
  desc$set("Imports", "zzz, aaa, mmm")
  desc$write(tmp)

  # Use non-OO API with normalize=TRUE (should not double-normalize)
  desc_set("Title", "New Title", file = tmp, normalize = TRUE)

  # should be normalized once, not corrupted by double normalization
  desc2 <- description$new(tmp)
  desc_expected <- description$new("!new")
  desc_expected$set("Config/desc/tidy", "true")
  desc_expected$set("Imports", "zzz, aaa, mmm")
  desc_expected$set("Title", "New Title")
  desc_expected$normalize()

  expect_equal(desc2$str(), desc_expected$str())
})

test_that("manual normalize works when auto-tidy disabled", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  # Create a DESCRIPTION with auto-tidy disabled
  desc <- description$new("!new")
  desc$set("Config/desc/tidy", "false")
  desc$set("Imports", "zzz, aaa, mmm")
  desc$write(tmp)

  # Use non-OO API with normalize=TRUE (should manually normalize)
  desc_set("Title", "New Title", file = tmp, normalize = TRUE)

  # should be normalized due to explicit normalize=TRUE
  desc2 <- description$new(tmp)
  desc_expected <- description$new("!new")
  desc_expected$set("Config/desc/tidy", "false")
  desc_expected$set("Imports", "zzz, aaa, mmm")
  desc_expected$set("Title", "New Title")
  desc_expected$normalize()

  expect_equal(desc2$str(), desc_expected$str())
})
