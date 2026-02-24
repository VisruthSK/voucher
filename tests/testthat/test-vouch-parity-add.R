test_that("add parity matches vouch CLI", {
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

  vouch_expect_update_file_cases("add", cases, prefix = "voucher-vouch-add-")

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

  vouch_expect_update_default_path_cases(
    command = "add",
    cases = cases,
    args = list(username = "charlie", write = TRUE),
    prefix = "voucher-add-default-"
  )

  vouch_expect_update_override(
    command = "add",
    args = list(username = "bob", write = TRUE),
    override_lines = c("# override", "zoe"),
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    prefix = "voucher-add-override-"
  )

  vouch_expect_missing_file(
    command = "add",
    args = list(username = "nobody"),
    prefix = "voucher-add-missing-"
  )
})
