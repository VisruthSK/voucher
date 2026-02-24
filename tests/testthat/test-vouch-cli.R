test_that("add preview prints updated trustdown and does not write", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(".github")
  initial <- c("# header", "alice", "-bob spam")
  writeLines(initial, ".github/VOUCHED.td")

  expect_snapshot(
    result <- withVisible(voucher:::add("bob")),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(result$value, "# header\nalice\nbob\n")
  expect_equal(readLines(".github/VOUCHED.td", warn = FALSE), initial)
})

test_that("add write updates file and emits cli success", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(
    c("# header", "github:carol", "carol", "-github:carol stale", "alice"),
    "VOUCHED.td"
  )

  expect_snapshot(
    result <- withVisible(voucher:::add(
      "carol",
      write = TRUE,
      default_platform = "github"
    )),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(result$value, "# header\nalice\ncarol\n")
  expect_equal(
    readLines("VOUCHED.td", warn = FALSE),
    c("# header", "alice", "carol")
  )
})

test_that("denounce preview includes reason and does not write", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(".github")
  initial <- c("# header", "alice", "bob")
  writeLines(initial, ".github/VOUCHED.td")

  expect_snapshot(
    result <- withVisible(voucher:::denounce("bob", reason = "bad actor")),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(result$value, "# header\nalice\n-bob bad actor\n")
  expect_equal(readLines(".github/VOUCHED.td", warn = FALSE), initial)
})

test_that("denounce write updates file and emits cli success", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("alice", "github:bob"), "VOUCHED.td")

  expect_snapshot(
    result <- withVisible(voucher:::denounce("github:bob", write = TRUE)),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(result$value, "alice\n-github:bob\n")
  expect_equal(readLines("VOUCHED.td", warn = FALSE), c("alice", "-github:bob"))
})

test_that("check reports statuses via cli and returns invisibly", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("alice", "-github:bob reason"), "VOUCHED.td")

  expect_snapshot(
    {
      vouched <- withVisible(voucher:::check("alice"))
      denounced <- withVisible(voucher:::check("github:bob"))
      unknown <- withVisible(voucher:::check("charlie"))
    },
    cran = FALSE
  )

  expect_false(vouched$visible)
  expect_false(denounced$visible)
  expect_false(unknown$visible)
  expect_equal(vouched$value, "vouched")
  expect_equal(denounced$value, "denounced")
  expect_equal(unknown$value, "unknown")
})

test_that("check default_platform changes matching for unqualified handles", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("-github:bob reason", "gitlab:bob", "bob"), "VOUCHED.td")

  expect_snapshot(
    {
      github <- withVisible(voucher:::check("bob", default_platform = "github"))
      gitlab <- withVisible(voucher:::check("bob", default_platform = "gitlab"))
      none <- withVisible(voucher:::check("bob"))
    },
    cran = FALSE
  )

  expect_equal(github$value, "denounced")
  expect_equal(gitlab$value, "vouched")
  expect_equal(none$value, "denounced")
})

test_that("check errors when file is missing and handles empty file", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(voucher:::check("nobody"), "no VOUCHED file found")

  writeLines(character(0), "VOUCHED.td")
  expect_snapshot(
    result <- withVisible(voucher:::check("nobody")),
    cran = FALSE
  )
  expect_false(result$visible)
  expect_equal(result$value, "unknown")
})

test_that("line and handle parsers cover comment and platform branches", {
  expect_null(voucher:::vouch_parse_line("# comment"))
  expect_null(voucher:::vouch_parse_line("   "))

  parsed_denounce <- voucher:::vouch_parse_line(
    "-github:alice reason",
    default_platform = "gitlab"
  )
  expect_equal(parsed_denounce$type, "denounce")
  expect_equal(parsed_denounce$username, "alice")
  expect_equal(parsed_denounce$platform, "github")

  parsed_vouch <- voucher:::vouch_parse_line(
    "alice trailing details",
    default_platform = "GitLab"
  )
  expect_equal(parsed_vouch$type, "vouch")
  expect_equal(parsed_vouch$username, "alice")
  expect_equal(parsed_vouch$platform, "gitlab")

  split_explicit <- voucher:::vouch_split_handle("github:alice:team", "gitlab")
  expect_equal(split_explicit$raw_platform, "github")
  expect_equal(split_explicit$username, "alice:team")
  expect_equal(split_explicit$platform, "github")

  split_default <- voucher:::vouch_split_handle("alice", "GitLab")
  expect_equal(split_default$raw_platform, "")
  expect_equal(split_default$username, "alice")
  expect_equal(split_default$platform, "gitlab")
})

test_that("file resolution helper branches", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(voucher:::vouch_resolve_existing_file(), "no VOUCHED file found")
  expect_equal(voucher:::vouch_resolve_existing_file("custom.td"), "custom.td")
})
