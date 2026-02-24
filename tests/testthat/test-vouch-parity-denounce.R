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
    temp_proj <- tempfile("voucher-vouch-denounce-")
    dir.create(temp_proj)
    file_r <- file.path(temp_proj, "voucher.td")
    file_v <- file.path(temp_proj, "vouch.td")
    writeLines(case$initial, file_r)
    writeLines(case$initial, file_v)

    voucher_result <- vouch_run_denounce(
      username = case$username,
      write = case$write,
      reason = case$reason,
      default_platform = case$default_platform,
      vouched_file = file_r
    )
    vouch_result <- vouch_cli(
      vouch_denounce_args(
        username = case$username,
        write = case$write,
        reason = case$reason,
        default_platform = case$default_platform,
        vouched_file = file_v
      )
    )

    expect_false(voucher_result$visible, info = case$name)
    expect_equal(vouch_result$status, 0L, info = case$name)

    if (isTRUE(case$write)) {
      expect_equal(vouch_td(file_r), vouch_td(file_v), info = case$name)
      expect_equal(voucher_result$value, vouch_td(file_r), info = case$name)
    } else {
      expect_equal(voucher_result$value, vouch_result$output, info = case$name)
      expect_equal(
        readLines(file_r, warn = FALSE),
        case$initial,
        info = case$name
      )
      expect_equal(
        readLines(file_v, warn = FALSE),
        case$initial,
        info = case$name
      )
    }
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
    dir_r <- tempfile("voucher-denounce-default-r-")
    dir_v <- tempfile("voucher-denounce-default-v-")
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

    voucher_result <- vouch_with_dir(
      dir_r,
      vouch_run_denounce("bob", write = TRUE, reason = "spam")
    )
    vouch_result <- vouch_with_dir(
      dir_v,
      vouch_cli(vouch_denounce_args("bob", write = TRUE, reason = "spam"))
    )

    expect_false(voucher_result$visible, info = case$name)
    expect_equal(vouch_result$status, 0L, info = case$name)
    expect_equal(
      vouch_td(file.path(dir_r, case$target)),
      vouch_td(file.path(dir_v, case$target)),
      info = case$name
    )
  }
})

test_that("denounce parity matches explicit vouched-file override", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-denounce-override-r-")
  dir_v <- tempfile("voucher-denounce-override-v-")
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
  writeLines(c("# override", "zoe", "bob"), override_r)
  writeLines(c("# override", "zoe", "bob"), override_v)

  voucher_result <- vouch_with_dir(
    dir_r,
    vouch_run_denounce(
      "bob",
      write = TRUE,
      reason = "spam",
      vouched_file = override_r
    )
  )
  vouch_result <- vouch_with_dir(
    dir_v,
    vouch_cli(vouch_denounce_args(
      "bob",
      write = TRUE,
      reason = "spam",
      vouched_file = override_v
    ))
  )

  expect_false(voucher_result$visible)
  expect_equal(vouch_result$status, 0L)
  expect_equal(vouch_td(override_r), vouch_td(override_v))
  expect_equal(
    readLines(file.path(dir_r, "VOUCHED.td"), warn = FALSE),
    c("# root", "alice")
  )
  expect_equal(
    readLines(file.path(dir_r, ".github", "VOUCHED.td"), warn = FALSE),
    c("# gh", "bob")
  )
})

test_that("denounce missing-file errors match vouch CLI", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-denounce-missing-r-")
  dir_v <- tempfile("voucher-denounce-missing-v-")
  dir.create(dir_r)
  dir.create(dir_v)

  voucher_error <- vouch_with_dir(
    dir_r,
    tryCatch(
      {
        vouch_run_denounce("nobody")
        ""
      },
      error = function(e) conditionMessage(e)
    )
  )
  vouch_error <- vouch_with_dir(dir_v, vouch_cli(vouch_denounce_args("nobody")))

  expect_match(voucher_error, "no VOUCHED file found")
  expect_true(vouch_error$status != 0L)
  expect_match(vouch_error$output, "no VOUCHED file found")
})
