test_that("check parity matches explicit vouched-file usage", {
  vouch_parity_skip()

  temp_proj <- tempfile("voucher-vouch-check-")
  dir.create(temp_proj)
  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  lines <- c(
    "# header",
    "",
    "-github:bob reason",
    "github:bob",
    "gitlab:bob",
    "alice"
  )
  writeLines(lines, file_r)
  writeLines(lines, file_v)

  cases <- list(
    list(username = "alice", default_platform = "", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "bob", default_platform = "", code = 1L),
    list(username = "bob", default_platform = "github", code = 1L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "GiTlAb:BoB", default_platform = "", code = 0L),
    list(username = "nobody", default_platform = "", code = 2L)
  )

  for (case in cases) {
    voucher_result <- vouch_run_check(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = file_r
    )
    vouch_result <- vouch_cli(vouch_check_args(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = file_v
    ))

    expect_false(voucher_result$visible, info = case$username)
    expect_equal(
      voucher_result$value,
      vouch_status_from_output(vouch_result$output),
      info = case$username
    )
    expect_equal(vouch_result$status, case$code, info = case$username)
  }
})

test_that("check parity matches additional ordering and platform tie cases", {
  vouch_parity_skip()

  temp_proj <- tempfile("voucher-vouch-check-extra-")
  dir.create(temp_proj)
  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  lines <- c(
    "# header",
    "gitlab:bob",
    "-github:bob blocked",
    "bob",
    "github:bob",
    "-bob denied",
    "alice"
  )
  writeLines(lines, file_r)
  writeLines(lines, file_v)

  cases <- list(
    list(username = "bob", default_platform = "", code = 0L),
    list(username = "bob", default_platform = "github", code = 0L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "gitlab:bob", default_platform = "", code = 0L)
  )

  for (case in cases) {
    voucher_result <- vouch_run_check(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = file_r
    )
    vouch_result <- vouch_cli(vouch_check_args(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = file_v
    ))

    expect_false(voucher_result$visible, info = case$username)
    expect_equal(
      voucher_result$value,
      vouch_status_from_output(vouch_result$output),
      info = case$username
    )
    expect_equal(vouch_result$status, case$code, info = case$username)
  }
})

test_that("check parity matches default-path resolution", {
  vouch_parity_skip()

  cases <- list(
    list(
      name = "prefers VOUCHED.td when both exist",
      root_lines = c("# root", "alice"),
      github_lines = c("# gh", "bob"),
      username = "alice",
      code = 0L
    ),
    list(
      name = "falls back to .github/VOUCHED.td",
      root_lines = NULL,
      github_lines = c("# gh", "bob"),
      username = "bob",
      code = 0L
    )
  )

  for (case in cases) {
    dir_r <- tempfile("voucher-check-default-r-")
    dir_v <- tempfile("voucher-check-default-v-")
    vouch_new_project(
      dir_r,
      root_lines = case$root_lines,
      github_lines = case$github_lines
    )
    vouch_new_project(
      dir_v,
      root_lines = case$root_lines,
      github_lines = case$github_lines
    )

    voucher_result <- vouch_with_dir(dir_r, vouch_run_check(case$username))
    vouch_result <- vouch_with_dir(
      dir_v,
      vouch_cli(vouch_check_args(case$username))
    )

    expect_false(voucher_result$visible, info = case$name)
    expect_equal(
      voucher_result$value,
      vouch_status_from_output(vouch_result$output),
      info = case$name
    )
    expect_equal(vouch_result$status, case$code, info = case$name)
  }
})

test_that("check parity matches explicit vouched-file override", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-check-override-r-")
  dir_v <- tempfile("voucher-check-override-v-")
  vouch_new_project(
    dir_r,
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob")
  )
  vouch_new_project(
    dir_v,
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob")
  )

  override_r <- file.path(dir_r, "override.td")
  override_v <- file.path(dir_v, "override.td")
  writeLines(c("# override", "-bob blocked"), override_r)
  writeLines(c("# override", "-bob blocked"), override_v)

  voucher_result <- vouch_with_dir(
    dir_r,
    vouch_run_check("bob", vouched_file = override_r)
  )
  vouch_result <- vouch_with_dir(
    dir_v,
    vouch_cli(vouch_check_args("bob", vouched_file = override_v))
  )

  expect_false(voucher_result$visible)
  expect_equal(
    voucher_result$value,
    vouch_status_from_output(vouch_result$output)
  )
  expect_equal(vouch_result$status, 1L)
  expect_equal(
    readLines(file.path(dir_r, "VOUCHED.td"), warn = FALSE),
    c("# root", "alice")
  )
  expect_equal(
    readLines(file.path(dir_r, ".github", "VOUCHED.td"), warn = FALSE),
    c("# gh", "bob")
  )
})

test_that("check missing-file errors match vouch CLI", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-check-missing-r-")
  dir_v <- tempfile("voucher-check-missing-v-")
  dir.create(dir_r)
  dir.create(dir_v)

  voucher_error <- vouch_with_dir(
    dir_r,
    tryCatch(
      {
        vouch_run_check("nobody")
        ""
      },
      error = function(e) conditionMessage(e)
    )
  )
  vouch_error <- vouch_with_dir(dir_v, vouch_cli(vouch_check_args("nobody")))

  expect_match(voucher_error, "no VOUCHED file found")
  expect_true(vouch_error$status != 0L)
  expect_match(vouch_error$output, "no VOUCHED file found")
})
