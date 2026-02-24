# jarl-ignore-file internal_function: tests intentionally exercise voucher internal functions.
vouch_default_database <- c(
  "# Copied from vouch repo:",
  "# Vouched contributors for this project.",
  "#",
  "# See https://github.com/mitchellh/vouch for details.",
  "#",
  "# Syntax:",
  "#   - One handle per line (without @), sorted alphabetically.",
  "#   - Optional platform prefix: platform:username (e.g., github:user).",
  "#   - Denounce with minus prefix: -username or -platform:username.",
  "#   - Optional details after a space following the handle."
)

test_that("use_vouch creates database and .Rbuildignore when missing", {
  vouch_with_temp_project({
    expect_snapshot(expect_invisible(voucher:::use_vouch()))

    expect_true(file.exists(".github/VOUCHED.td"))
    expect_equal(
      readLines(".github/VOUCHED.td", warn = FALSE),
      vouch_default_database
    )
    expect_equal(readLines(".Rbuildignore", warn = FALSE), "^\\.github$")
  })
})

test_that("use_vouch appends .github rule to existing .Rbuildignore", {
  vouch_with_temp_project({
    writeLines(c("^data$", "^README\\.Rmd$"), ".Rbuildignore")

    expect_snapshot(expect_invisible(voucher:::use_vouch()))

    expect_equal(
      readLines(".Rbuildignore", warn = FALSE),
      c("^data$", "^README\\.Rmd$", "^\\.github$")
    )
  })
})

test_that("use_vouch does not duplicate .github rule in .Rbuildignore", {
  vouch_with_temp_project({
    writeLines(c("^data$", "^\\.github$"), ".Rbuildignore")

    expect_snapshot(expect_invisible(voucher:::use_vouch()))

    expect_equal(
      readLines(".Rbuildignore", warn = FALSE),
      c("^data$", "^\\.github$")
    )
  })
})

test_that("use_vouch exits without changes when database already exists", {
  vouch_with_temp_project({
    writeLines("existing", "VOUCHED.td")

    expect_snapshot(expect_invisible(voucher:::use_vouch()))

    expect_equal(readLines("VOUCHED.td", warn = FALSE), "existing")
    expect_false(file.exists(".github/VOUCHED.td"))
    expect_false(file.exists(".Rbuildignore"))
  })
})
