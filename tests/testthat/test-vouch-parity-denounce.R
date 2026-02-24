test_that("denounce parity matches explicit vouched-file usage", {
  vouch_parity_skip()

  cases <- list(
    list(
      name = "preview with reason",
      username = "bob",
      write = FALSE,
      reason = "AI slop",
      default_platform = "",
      initial = c("# header", "zoe", "alice", "bob")
    ),
    list(
      name = "write with default platform",
      username = "bob",
      write = TRUE,
      reason = "spam",
      default_platform = "github",
      initial = c("# header", "gitlab:bob", "github:bob", "bob", "zoe")
    ),
    list(
      name = "write explicit platform",
      username = "github:bob",
      write = TRUE,
      reason = "",
      default_platform = "",
      initial = c("# header", "gitlab:bob", "github:bob", "bob", "zoe")
    ),
    list(
      name = "write without reason",
      username = "bob",
      write = TRUE,
      reason = "",
      default_platform = "",
      initial = c("# header", "bob")
    )
  )

  for (case in cases) {
    files <- vouch_new_pair_files(
      case$initial,
      prefix = "voucher-vouch-denounce-"
    )

    voucher_result <- vouch_run_denounce(
      username = case$username,
      write = case$write,
      reason = case$reason,
      default_platform = case$default_platform,
      vouched_file = files$voucher_file
    )
    vouch_result <- vouch_cli(
      vouch_denounce_args(
        username = case$username,
        write = case$write,
        reason = case$reason,
        default_platform = case$default_platform,
        vouched_file = files$vouch_file
      )
    )

    vouch_expect_update_parity(
      voucher_result = voucher_result,
      vouch_result = vouch_result,
      write = case$write,
      voucher_file = files$voucher_file,
      vouch_file = files$vouch_file,
      initial_lines = case$initial,
      info = case$name
    )
  }
})

test_that("denounce parity matches default-path resolution", {
  vouch_parity_skip()

  cases <- list(
    list(
      name = "prefers VOUCHED.td when both exist",
      root_lines = c("# root", "bob"),
      github_lines = c("# gh", "bob"),
      target = "VOUCHED.td"
    ),
    list(
      name = "falls back to .github/VOUCHED.td",
      root_lines = NULL,
      github_lines = c("# gh", "bob"),
      target = ".github/VOUCHED.td"
    )
  )

  for (case in cases) {
    projects <- vouch_new_pair_projects(
      root_lines = case$root_lines,
      github_lines = case$github_lines,
      prefix = "voucher-denounce-default-"
    )

    voucher_result <- vouch_with_dir(
      projects$voucher_dir,
      vouch_run_denounce("bob", write = TRUE, reason = "spam")
    )
    vouch_result <- vouch_with_dir(
      projects$vouch_dir,
      vouch_cli(vouch_denounce_args("bob", write = TRUE, reason = "spam"))
    )

    vouch_expect_update_parity(
      voucher_result = voucher_result,
      vouch_result = vouch_result,
      write = TRUE,
      voucher_file = file.path(projects$voucher_dir, case$target),
      vouch_file = file.path(projects$vouch_dir, case$target),
      info = case$name
    )
  }
})

test_that("denounce parity matches explicit vouched-file override", {
  vouch_parity_skip()

  projects <- vouch_new_pair_projects(
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    prefix = "voucher-denounce-override-"
  )

  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(c("# override", "zoe", "bob"), override_r)
  writeLines(c("# override", "zoe", "bob"), override_v)

  voucher_result <- vouch_with_dir(
    projects$voucher_dir,
    vouch_run_denounce(
      "bob",
      write = TRUE,
      reason = "spam",
      vouched_file = override_r
    )
  )
  vouch_result <- vouch_with_dir(
    projects$vouch_dir,
    vouch_cli(vouch_denounce_args(
      "bob",
      write = TRUE,
      reason = "spam",
      vouched_file = override_v
    ))
  )

  vouch_expect_update_parity(
    voucher_result = voucher_result,
    vouch_result = vouch_result,
    write = TRUE,
    voucher_file = override_r,
    vouch_file = override_v
  )
  vouch_expect_project_files(
    projects$voucher_dir,
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob")
  )
})

test_that("denounce missing-file errors match vouch CLI", {
  vouch_parity_skip()

  vouch_expect_missing_file_parity(
    run_voucher = function() vouch_run_denounce("nobody"),
    run_vouch = function() vouch_cli(vouch_denounce_args("nobody")),
    prefix = "voucher-denounce-missing-"
  )
})
