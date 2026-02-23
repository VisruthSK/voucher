test_that("use_vouch creates database and .Rbuildignore when missing", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  expect_snapshot(expect_invisible(voucher:::use_vouch()))

  expect_true(file.exists(".github/VOUCHED.td"))
  expect_equal(
    readLines(".github/VOUCHED.td", warn = FALSE),
    c(
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
  )
  expect_equal(readLines(".Rbuildignore", warn = FALSE), "^\\.github$")
})

test_that("use_vouch appends .github rule to existing .Rbuildignore", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("^data$", "^README\\.Rmd$"), ".Rbuildignore")

  expect_snapshot(expect_invisible(voucher:::use_vouch()))

  expect_equal(
    readLines(".Rbuildignore", warn = FALSE),
    c("^data$", "^README\\.Rmd$", "^\\.github$")
  )
})

test_that("use_vouch does not duplicate .github rule in .Rbuildignore", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("^data$", "^\\.github$"), ".Rbuildignore")

  expect_snapshot(expect_invisible(voucher:::use_vouch()))

  expect_equal(
    readLines(".Rbuildignore", warn = FALSE),
    c("^data$", "^\\.github$")
  )
})

test_that("use_vouch exits without changes when database already exists", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines("existing", "VOUCHED.td")

  expect_snapshot(expect_invisible(voucher:::use_vouch()))

  expect_equal(readLines("VOUCHED.td", warn = FALSE), "existing")
  expect_false(file.exists(".github/VOUCHED.td"))
  expect_false(file.exists(".Rbuildignore"))
})
