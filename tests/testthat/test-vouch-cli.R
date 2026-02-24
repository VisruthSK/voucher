test_that("add previews updated trustdown without writing", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(".github")
  writeLines(c("# header", "alice", "-bob spam"), ".github/VOUCHED.td")

  output <- capture.output(
    result <- withVisible(voucher:::add("bob"))
  )

  expect_false(result$visible)
  expect_equal(
    output,
    c("# header", "alice", "bob")
  )
  expect_equal(
    readLines(".github/VOUCHED.td", warn = FALSE),
    c("# header", "alice", "-bob spam")
  )
})

test_that("add writes in-place when write is TRUE", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("# header", "alice"), "VOUCHED.td")

  expect_snapshot(
    result <- withVisible(voucher:::add("github:carol", write = TRUE)),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(
    readLines("VOUCHED.td", warn = FALSE),
    c("# header", "alice", "github:carol")
  )
})

test_that("check returns vouched denounced and unknown", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("alice", "-github:bob reason"), "VOUCHED.td")

  expect_snapshot(
    status_vouched <- voucher:::check("alice"),
    cran = FALSE
  )
  expect_snapshot(
    status_denounced <- voucher:::check("github:bob"),
    cran = FALSE
  )
  expect_snapshot(
    status_unknown <- voucher:::check("charlie"),
    cran = FALSE
  )

  expect_equal(status_vouched, "vouched")
  expect_equal(status_denounced, "denounced")
  expect_equal(status_unknown, "unknown")
})

test_that("denounce previews and writes denounced entry", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  dir.create(".github")
  writeLines(c("# header", "alice", "bob"), ".github/VOUCHED.td")

  preview <- capture.output(
    result_preview <- withVisible(voucher:::denounce(
      "bob",
      reason = "bad actor"
    ))
  )
  expect_false(result_preview$visible)
  expect_equal(
    preview,
    c("# header", "alice", "-bob bad actor")
  )
  expect_equal(
    readLines(".github/VOUCHED.td", warn = FALSE),
    c("# header", "alice", "bob")
  )

  expect_snapshot(
    result_write <- withVisible(voucher:::denounce(
      "bob",
      reason = "bad actor",
      write = TRUE
    )),
    cran = FALSE
  )
  expect_false(result_write$visible)
  expect_equal(
    readLines(".github/VOUCHED.td", warn = FALSE),
    c("# header", "alice", "-bob bad actor")
  )
})

test_that("helpers cover no file and empty text edge cases", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  expect_error(
    voucher:::vouch_resolve_existing_file(),
    "no VOUCHED file found"
  )
})

test_that("helpers cover vector and parsing edge branches", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("# comment", "", "alice"), "VOUCHED.td")

  expect_equal(voucher:::vouch_resolve_existing_file("custom.td"), "custom.td")

  parsed <- voucher:::vouch_parse_entry("alice ")
  expect_identical(parsed$details, NULL)

  expect_snapshot(
    status <- voucher:::check("alice"),
    cran = FALSE
  )
  expect_equal(status, "vouched")
})

test_that("default_platform paths are exercised in check and add", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines(c("github:alice", "bob"), "VOUCHED.td")

  status <- suppressMessages(
    voucher:::check("bob", default_platform = "github")
  )
  expect_equal(status, "vouched")

  invisible(capture.output(
    voucher:::add(
      "carol",
      default_platform = "github",
      vouched_file = "VOUCHED.td"
    )
  ))

  expect_equal(
    readLines("VOUCHED.td", warn = FALSE),
    c("github:alice", "bob")
  )
})
