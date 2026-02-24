test_that("vouch parity matches vouch CLI", {
  vouch_parity_skip()

  add_cases <- list(
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
  vouch_expect_update_file_cases(
    "add",
    add_cases,
    prefix = "voucher-vouch-add-"
  )

  add_path_cases <- list(
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
    cases = add_path_cases,
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

  denounce_cases <- list(
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
    denounce_cases,
    prefix = "voucher-vouch-denounce-"
  )

  denounce_path_cases <- list(
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
    cases = denounce_path_cases,
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

  check_lines <- c(
    "# header",
    "",
    "-github:bob reason",
    "github:bob",
    "gitlab:bob",
    "alice"
  )
  check_cases <- list(
    list(username = "alice", default_platform = "", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "bob", default_platform = "", code = 1L),
    list(username = "bob", default_platform = "github", code = 1L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "GiTlAb:BoB", default_platform = "", code = 0L),
    list(username = "nobody", default_platform = "", code = 2L)
  )
  vouch_expect_check_file_cases(
    check_lines,
    check_cases,
    prefix = "voucher-vouch-check-"
  )

  check_lines <- c(
    "# header",
    "gitlab:bob",
    "-github:bob blocked",
    "bob",
    "github:bob",
    "-bob denied",
    "alice"
  )
  check_cases <- list(
    list(username = "bob", default_platform = "", code = 0L),
    list(username = "bob", default_platform = "github", code = 1L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "gitlab:bob", default_platform = "", code = 0L)
  )
  vouch_expect_check_file_cases(
    check_lines,
    check_cases,
    prefix = "voucher-vouch-check-extra-"
  )

  check_path_cases <- list(
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
  vouch_expect_check_default_path_cases(
    check_path_cases,
    prefix = "voucher-check-default-"
  )
  vouch_expect_check_override(
    override_lines = c("# override", "-bob blocked"),
    root_lines = c("# root", "alice"),
    github_lines = c("# gh", "bob"),
    args = list(username = "bob"),
    expected_code = 1L,
    prefix = "voucher-check-override-"
  )
  vouch_expect_missing_file(
    command = "check",
    args = list(username = "nobody"),
    prefix = "voucher-check-missing-"
  )
})
