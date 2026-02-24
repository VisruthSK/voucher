test_that("add parity matches explicit vouched-file usage", {
  vouch_parity_skip()

  cases <- list(
    list(
      name = "preview explicit platform",
      username = "github:bob",
      write = FALSE,
      default_platform = "",
      initial = c("# header", "zoe", "-github:bob stale", "alice")
    ),
    list(
      name = "write with default platform keeps other platforms",
      username = "ana",
      write = TRUE,
      default_platform = "gitlab",
      initial = c("# header", "github:ana", "ana", "-gitlab:ana stale", "zoe")
    ),
    list(
      name = "write no default removes all platform variants",
      username = "alex",
      write = TRUE,
      default_platform = "",
      initial = c("# header", "github:alex", "gitlab:alex", "alex", "zoe")
    ),
    list(
      name = "write normalizes case and keeps colon suffix",
      username = "GitHub:Team:Bob",
      write = TRUE,
      default_platform = "",
      initial = c("# header", "zoe")
    ),
    list(
      name = "write with no existing contributors",
      username = "newuser",
      write = TRUE,
      default_platform = "",
      initial = c("# header", "")
    )
  )

  for (case in cases) {
    files <- vouch_new_pair_files(case$initial, prefix = "voucher-vouch-add-")

    voucher_result <- vouch_run(
      command = "add",
      username = case$username,
      write = case$write,
      default_platform = case$default_platform,
      vouched_file = files$voucher_file
    )
    vouch_result <- vouch_cli(vouch_args(
      command = "add",
      username = case$username,
      write = case$write,
      default_platform = case$default_platform,
      vouched_file = files$vouch_file
    ))

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

test_that("add parity matches default-path resolution", {
  vouch_parity_skip()

  cases <- list(
    list(
      name = "prefers VOUCHED.td when both exist",
      root_lines = c("# root", "alice"),
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
      prefix = "voucher-add-default-"
    )

    voucher_result <- vouch_with_dir(
      projects$voucher_dir,
      vouch_run("add", username = "charlie", write = TRUE)
    )
    vouch_result <- vouch_with_dir(
      projects$vouch_dir,
      vouch_cli(vouch_args("add", "charlie", write = TRUE))
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

test_that("add parity matches explicit vouched-file override", {
  vouch_parity_skip()

  projects <- vouch_new_pair_projects(
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    prefix = "voucher-add-override-"
  )

  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(c("# override", "zoe"), override_r)
  writeLines(c("# override", "zoe"), override_v)

  voucher_result <- vouch_with_dir(
    projects$voucher_dir,
    vouch_run("add", username = "bob", write = TRUE, vouched_file = override_r)
  )
  vouch_result <- vouch_with_dir(
    projects$vouch_dir,
    vouch_cli(vouch_args("add", "bob", write = TRUE, vouched_file = override_v))
  )

  vouch_expect_update_parity(
    voucher_result = voucher_result,
    vouch_result = vouch_result,
    write = TRUE,
    voucher_file = override_r,
    vouch_file = override_v
  )
  expect_equal(
    readLines(file.path(projects$voucher_dir, "VOUCHED.td"), warn = FALSE),
    c("# root", "alice")
  )
  expect_equal(
    readLines(
      file.path(projects$voucher_dir, ".github", "VOUCHED.td"),
      warn = FALSE
    ),
    c("# gh", "bob")
  )
})

test_that("add missing-file errors match vouch CLI", {
  vouch_parity_skip()

  voucher_dir <- tempfile("voucher-add-missing-r-")
  vouch_dir <- tempfile("voucher-add-missing-v-")
  dir.create(voucher_dir)
  dir.create(vouch_dir)

  voucher_error <- vouch_with_dir(
    voucher_dir,
    tryCatch(
      {
        vouch_run("add", username = "nobody")
        ""
      },
      error = function(e) conditionMessage(e)
    )
  )
  vouch_error <- vouch_with_dir(
    vouch_dir,
    vouch_cli(vouch_args("add", "nobody"))
  )

  expect_match(voucher_error, "no VOUCHED file found")
  expect_true(vouch_error$status != 0L)
  expect_match(vouch_error$output, "no VOUCHED file found")
})
