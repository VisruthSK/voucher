test_that("check parity matches vouch CLI", {
  vouch_parity_skip()

  lines <- c(
    "# header",
    "",
    "-github:bob reason",
    "github:bob",
    "gitlab:bob",
    "alice"
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
  vouch_expect_check_file_cases(lines, cases, prefix = "voucher-vouch-check-")

  lines <- c(
    "# header",
    "gitlab:bob",
    "-github:bob blocked",
    "bob",
    "github:bob",
    "-bob denied",
    "alice"
  )
  cases <- list(
    list(username = "bob", default_platform = "", code = 0L),
    list(username = "bob", default_platform = "github", code = 1L),
    list(username = "bob", default_platform = "gitlab", code = 0L),
    list(username = "github:bob", default_platform = "", code = 1L),
    list(username = "gitlab:bob", default_platform = "", code = 0L)
  )
  vouch_expect_check_file_cases(
    lines,
    cases,
    prefix = "voucher-vouch-check-extra-"
  )

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
  vouch_expect_check_default_path_cases(
    cases,
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
