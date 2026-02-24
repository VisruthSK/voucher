test_that("check parity matches explicit vouched-file usage", {
  vouch_parity_skip()

  files <- vouch_new_pair_files(
    c(
      "# header",
      "",
      "-github:bob reason",
      "github:bob",
      "gitlab:bob",
      "alice"
    ),
    prefix = "voucher-vouch-check-"
  )

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
      vouched_file = files$voucher_file
    )
    vouch_result <- vouch_cli(vouch_check_args(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = files$vouch_file
    ))

    info <- paste(case$username, case$default_platform, sep = " | ")
    vouch_expect_check_parity(
      voucher_result = voucher_result,
      vouch_result = vouch_result,
      expected_code = case$code,
      info = info
    )
  }
})

test_that("check parity matches additional ordering and platform tie cases", {
  vouch_parity_skip()

  files <- vouch_new_pair_files(
    c(
      "# header",
      "gitlab:bob",
      "-github:bob blocked",
      "bob",
      "github:bob",
      "-bob denied",
      "alice"
    ),
    prefix = "voucher-vouch-check-extra-"
  )

  cases <- list(
    list(username = "bob", default_platform = "", code = 0L),
    list(username = "bob", default_platform = "github", code = 1L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "gitlab:bob", default_platform = "", code = 0L)
  )

  for (case in cases) {
    voucher_result <- vouch_run_check(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = files$voucher_file
    )
    vouch_result <- vouch_cli(vouch_check_args(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = files$vouch_file
    ))

    info <- paste(case$username, case$default_platform, sep = " | ")
    vouch_expect_check_parity(
      voucher_result = voucher_result,
      vouch_result = vouch_result,
      expected_code = case$code,
      info = info
    )
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
    projects <- vouch_new_pair_projects(
      root_lines = case$root_lines,
      github_lines = case$github_lines,
      prefix = "voucher-check-default-"
    )

    voucher_result <- vouch_with_dir(
      projects$voucher_dir,
      vouch_run_check(case$username)
    )
    vouch_result <- vouch_with_dir(
      projects$vouch_dir,
      vouch_cli(vouch_check_args(case$username))
    )

    vouch_expect_check_parity(
      voucher_result = voucher_result,
      vouch_result = vouch_result,
      expected_code = case$code,
      info = case$name
    )
  }
})

test_that("check parity matches explicit vouched-file override", {
  vouch_parity_skip()

  projects <- vouch_new_pair_projects(
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    prefix = "voucher-check-override-"
  )

  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(c("# override", "-bob blocked"), override_r)
  writeLines(c("# override", "-bob blocked"), override_v)

  voucher_result <- vouch_with_dir(
    projects$voucher_dir,
    vouch_run_check("bob", vouched_file = override_r)
  )
  vouch_result <- vouch_with_dir(
    projects$vouch_dir,
    vouch_cli(vouch_check_args("bob", vouched_file = override_v))
  )

  vouch_expect_check_parity(
    voucher_result = voucher_result,
    vouch_result = vouch_result,
    expected_code = 1L
  )
  vouch_expect_project_files(
    projects$voucher_dir,
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob")
  )
})

test_that("check missing-file errors match vouch CLI", {
  vouch_parity_skip()

  vouch_expect_missing_file_parity(
    run_voucher = function() vouch_run_check("nobody"),
    run_vouch = function() vouch_cli(vouch_check_args("nobody")),
    prefix = "voucher-check-missing-"
  )
})
