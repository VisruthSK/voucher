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

  output <- capture.output(
    result <- withVisible(voucher:::add("github:carol", write = TRUE))
  )

  expect_false(result$visible)
  expect_equal(output, "Added (github:carol) to vouched contributors")
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

  out_vouched <- capture.output(status_vouched <- voucher:::check("alice"))
  out_denounced <- capture.output(
    status_denounced <- voucher:::check("github:bob")
  )
  out_unknown <- capture.output(status_unknown <- voucher:::check("charlie"))

  expect_equal(status_vouched, "vouched")
  expect_equal(status_denounced, "denounced")
  expect_equal(status_unknown, "unknown")
  expect_equal(out_vouched, "alice is vouched")
  expect_equal(out_denounced, "github:bob is denounced")
  expect_equal(out_unknown, "charlie is unknown")
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

  write_out <- capture.output(
    result_write <- withVisible(voucher:::denounce(
      "bob",
      reason = "bad actor",
      write = TRUE
    ))
  )
  expect_false(result_write$visible)
  expect_equal(write_out, "Denounced (bob)")
  expect_equal(
    readLines(".github/VOUCHED.td", warn = FALSE),
    c("# header", "alice", "-bob bad actor")
  )
})
