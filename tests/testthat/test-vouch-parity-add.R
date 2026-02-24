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
    temp_proj <- tempfile("voucher-vouch-add-")
    dir.create(temp_proj)
    file_r <- file.path(temp_proj, "voucher.td")
    file_v <- file.path(temp_proj, "vouch.td")
    writeLines(case$initial, file_r)
    writeLines(case$initial, file_v)

    voucher_result <- vouch_run_add(
      username = case$username,
      write = case$write,
      default_platform = case$default_platform,
      vouched_file = file_r
    )
    vouch_result <- vouch_cli(vouch_add_args(
      username = case$username,
      write = case$write,
      default_platform = case$default_platform,
      vouched_file = file_v
    ))

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
    dir_r <- tempfile("voucher-add-default-r-")
    dir_v <- tempfile("voucher-add-default-v-")
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
      vouch_run_add("charlie", write = TRUE)
    )
    vouch_result <- vouch_with_dir(
      dir_v,
      vouch_cli(vouch_add_args("charlie", write = TRUE))
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

test_that("add parity matches explicit vouched-file override", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-add-override-r-")
  dir_v <- tempfile("voucher-add-override-v-")
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
  writeLines(c("# override", "zoe"), override_r)
  writeLines(c("# override", "zoe"), override_v)

  voucher_result <- vouch_with_dir(
    dir_r,
    vouch_run_add("bob", write = TRUE, vouched_file = override_r)
  )
  vouch_result <- vouch_with_dir(
    dir_v,
    vouch_cli(vouch_add_args("bob", write = TRUE, vouched_file = override_v))
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

test_that("add missing-file errors match vouch CLI", {
  vouch_parity_skip()

  dir_r <- tempfile("voucher-add-missing-r-")
  dir_v <- tempfile("voucher-add-missing-v-")
  dir.create(dir_r)
  dir.create(dir_v)

  voucher_error <- vouch_with_dir(
    dir_r,
    tryCatch(
      {
        vouch_run_add("nobody")
        ""
      },
      error = function(e) conditionMessage(e)
    )
  )
  vouch_error <- vouch_with_dir(dir_v, vouch_cli(vouch_add_args("nobody")))

  expect_match(voucher_error, "no VOUCHED file found")
  expect_true(vouch_error$status != 0L)
  expect_match(vouch_error$output, "no VOUCHED file found")
})
