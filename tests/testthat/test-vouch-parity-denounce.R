test_that("denounce parity matches vouch CLI", {
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

  vouch_expect_update_file_cases(
    "denounce",
    cases,
    prefix = "voucher-vouch-denounce-"
  )

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

  vouch_expect_update_default_path_cases(
    command = "denounce",
    cases = cases,
    args = list(username = "bob", write = TRUE, reason = "spam"),
    prefix = "voucher-denounce-default-"
  )

  vouch_expect_update_override(
    command = "denounce",
    args = list(username = "bob", write = TRUE, reason = "spam"),
    override_lines = c("# override", "zoe", "bob"),
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    prefix = "voucher-denounce-override-"
  )

  vouch_expect_missing_file(
    command = "denounce",
    args = list(username = "nobody"),
    prefix = "voucher-denounce-missing-"
  )
})
