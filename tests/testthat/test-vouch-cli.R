# jarl-ignore-file internal_function: tests intentionally exercise voucher internal functions.
expect_invisible_value <- function(result, value) {
  expect_false(result$visible)
  expect_equal(result$value, value)
}

test_that("add preview prints updated trustdown and does not write", {
  vouch_with_temp_project({
    dir.create(".github")
    initial <- c("# header", "alice", "-bob spam")
    writeLines(initial, ".github/VOUCHED.td")

    expect_snapshot(
      result <- withVisible(voucher:::add("bob")),
      cran = FALSE
    )

    expect_invisible_value(result, "# header\nalice\nbob\n")
    expect_equal(readLines(".github/VOUCHED.td", warn = FALSE), initial)
  })
})

test_that("add write updates file and emits cli success", {
  vouch_with_temp_project({
    writeLines(
      c("# header", "github:carol", "carol", "-github:carol stale", "alice"),
      "VOUCHED.td"
    )

    expect_snapshot(
      result <- withVisible(voucher:::add(
        "carol",
        write = TRUE,
        default_platform = "github"
      )),
      cran = FALSE
    )

    expect_invisible_value(result, "# header\nalice\ncarol\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("# header", "alice", "carol")
    )
  })
})

test_that("add write preserves non-contributor lines when no contributors exist", {
  vouch_with_temp_project({
    writeLines(c("# header", ""), "VOUCHED.td")

    result <- suppressMessages(withVisible(voucher:::add(
      "newuser",
      write = TRUE
    )))

    expect_invisible_value(result, "# header\n\nnewuser\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("# header", "", "newuser")
    )
  })
})

test_that("add supports vector usernames and writes once", {
  vouch_with_temp_project({
    writeLines(c("# header", "alice", "-bob old", "carol"), "VOUCHED.td")

    result <- suppressMessages(withVisible(voucher:::add(
      c("bob", "dave"),
      write = TRUE
    )))

    expect_invisible_value(result, "# header\nalice\nbob\ncarol\ndave\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("# header", "alice", "bob", "carol", "dave")
    )
  })
})

test_that("denounce preview includes reason and does not write", {
  vouch_with_temp_project({
    dir.create(".github")
    initial <- c("# header", "alice", "bob")
    writeLines(initial, ".github/VOUCHED.td")

    expect_snapshot(
      result <- withVisible(voucher:::denounce("bob", reason = "bad actor")),
      cran = FALSE
    )

    expect_invisible_value(result, "# header\nalice\n-bob bad actor\n")
    expect_equal(readLines(".github/VOUCHED.td", warn = FALSE), initial)
  })
})

test_that("denounce write updates file and emits cli success", {
  vouch_with_temp_project({
    writeLines(c("alice", "github:bob"), "VOUCHED.td")

    expect_snapshot(
      result <- withVisible(voucher:::denounce("github:bob", write = TRUE)),
      cran = FALSE
    )

    expect_invisible_value(result, "alice\n-github:bob\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("alice", "-github:bob")
    )
  })
})

test_that("denounce supports vector usernames with recycled reason", {
  vouch_with_temp_project({
    writeLines(c("alice", "bob", "carol"), "VOUCHED.td")

    result <- suppressMessages(withVisible(voucher:::denounce(
      c("alice", "carol"),
      write = TRUE,
      reason = "bad actor"
    )))

    expect_invisible_value(result, "-alice bad actor\nbob\n-carol bad actor\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("-alice bad actor", "bob", "-carol bad actor")
    )
  })
})

test_that("denounce errors when reason length doesn't match usernames", {
  vouch_with_temp_project({
    writeLines("alice", "VOUCHED.td")

    expect_error(
      voucher:::denounce(c("alice", "bob"), reason = c("x", "y", "z")),
      "`reason` must have length 1 or match `username` length."
    )
  })
})

test_that("check reports statuses via cli and returns invisibly", {
  vouch_with_temp_project({
    writeLines(c("# header", "", "alice", "-github:bob reason"), "VOUCHED.td")

    expect_snapshot(
      {
        vouched <- withVisible(voucher:::check("alice"))
        denounced <- withVisible(voucher:::check("github:bob"))
        unknown <- withVisible(voucher:::check("charlie"))
      },
      cran = FALSE
    )

    expect_invisible_value(vouched, "vouched")
    expect_invisible_value(denounced, "denounced")
    expect_invisible_value(unknown, "unknown")
  })
})

test_that("check supports vector usernames and returns named statuses", {
  vouch_with_temp_project({
    writeLines(c("alice", "-github:bob reason"), "VOUCHED.td")

    result <- suppressMessages(withVisible(voucher:::check(
      c("alice", "github:bob", "charlie")
    )))

    expect_false(result$visible)
    expect_equal(
      result$value,
      c(alice = "vouched", "github:bob" = "denounced", charlie = "unknown")
    )
  })
})

test_that("check default_platform changes matching for unqualified handles", {
  vouch_with_temp_project({
    writeLines(c("-github:bob reason", "gitlab:bob", "bob"), "VOUCHED.td")

    expect_snapshot(
      {
        github <- withVisible(voucher:::check(
          "bob",
          default_platform = "github"
        ))
        gitlab <- withVisible(voucher:::check(
          "bob",
          default_platform = "gitlab"
        ))
        none <- withVisible(voucher:::check("bob"))
      },
      cran = FALSE
    )

    expect_equal(github$value, "denounced")
    expect_equal(gitlab$value, "vouched")
    expect_equal(none$value, "denounced")
  })
})

test_that("check includes git blame author when blame is TRUE", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("git")))

  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  git <- function(...) {
    suppressWarnings(system2(
      "git",
      c(...),
      stdout = TRUE,
      stderr = TRUE
    ))
  }

  init <- git("init")
  if (!is.null(attr(init, "status", exact = TRUE))) {
    skip("git init failed in test environment")
  }

  git("config", "user.name", "VoucherBlameTest")
  git("config", "user.email", "voucher@example.com")

  writeLines("alice", "VOUCHED.td")
  git("add", "VOUCHED.td")
  commit <- git("commit", "-m", "add-alice")
  if (!is.null(attr(commit, "status", exact = TRUE))) {
    skip("git commit failed in test environment")
  }

  expect_snapshot(
    result <- withVisible(voucher:::check("alice", blame = TRUE)),
    cran = FALSE
  )

  expect_false(result$visible)
  expect_equal(result$value, "vouched")
})

test_that("check warns when blame is requested but unavailable", {
  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

  writeLines("alice", "VOUCHED.td")

  expect_warning(
    result <- suppressMessages(withVisible(voucher:::check(
      "alice",
      blame = TRUE
    ))),
    "Unable to resolve git blame author"
  )

  expect_false(result$visible)
  expect_equal(result$value, "vouched")
})

test_that("check with blame handles unknown users", {
  vouch_with_temp_project({
    writeLines("alice", "VOUCHED.td")

    expect_warning(
      result <- suppressMessages(withVisible(voucher:::check(
        "charlie",
        blame = TRUE
      ))),
      NA
    )

    expect_invisible_value(result, "unknown")
  })
})

test_that("check includes mocked git blame author in status message", {
  vouch_with_temp_project({
    writeLines("alice", "VOUCHED.td")
    local_mocked_bindings(
      vouch_git_blame_username = function(filepath, line_number) {
        expect_equal(basename(filepath), "VOUCHED.td")
        expect_equal(line_number, 1L)
        "Mock Author"
      },
      .package = "voucher"
    )

    expect_message(
      result <- withVisible(voucher:::check("alice", blame = TRUE)),
      "alice is vouched \\(Mock Author\\)"
    )
    expect_invisible_value(result, "vouched")
  })
})

test_that("check errors when file is missing and handles empty file", {
  vouch_with_temp_project({
    expect_error(voucher:::check("nobody"), "no VOUCHED file found")

    writeLines(character(0), "VOUCHED.td")
    expect_snapshot(
      result <- withVisible(voucher:::check("nobody")),
      cran = FALSE
    )
    expect_invisible_value(result, "unknown")
  })
})

test_that("empty usernames fail with clear validation errors", {
  vouch_with_temp_project({
    writeLines("alice", "VOUCHED.td")

    expect_error(
      voucher:::add("", write = TRUE),
      "`username` must not contain empty values."
    )
    expect_error(
      voucher:::denounce("", write = TRUE),
      "`username` must not contain empty values."
    )
    expect_error(
      voucher:::check(""),
      "`username` must not contain empty values."
    )
  })
})

test_that("missing usernames are dropped with an informational message", {
  vouch_with_temp_project({
    writeLines(c("alice", "-bob spam"), "VOUCHED.td")

    expect_message(
      add_result <- withVisible(voucher:::add(c("carol", NA), write = TRUE)),
      "Dropped 1 missing `username` value"
    )
    expect_invisible_value(add_result, "alice\n-bob spam\ncarol\n")
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("alice", "-bob spam", "carol")
    )

    expect_message(
      denounce_result <- withVisible(voucher:::denounce(
        c("alice", NA),
        write = TRUE,
        reason = c("bad actor", "unused")
      )),
      "Dropped 1 missing `username` value"
    )
    expect_invisible_value(
      denounce_result,
      "-alice bad actor\n-bob spam\ncarol\n"
    )
    expect_equal(
      readLines("VOUCHED.td", warn = FALSE),
      c("-alice bad actor", "-bob spam", "carol")
    )

    expect_message(
      check_result <- withVisible(voucher:::check(c("carol", NA))),
      "Dropped 1 missing `username` value"
    )
    expect_invisible_value(check_result, "vouched")
  })
})

test_that("all-missing usernames become a no-op after dropping NAs", {
  vouch_with_temp_project({
    writeLines(c("alice", "-bob spam"), "VOUCHED.td")
    initial <- readLines("VOUCHED.td", warn = FALSE)

    expect_message(
      add_result <- withVisible(voucher:::add(NA, write = TRUE)),
      "Dropped 1 missing `username` value"
    )
    expect_invisible_value(add_result, "alice\n-bob spam\n")
    expect_equal(readLines("VOUCHED.td", warn = FALSE), initial)

    expect_message(
      check_result <- withVisible(voucher:::check(NA)),
      "Dropped 1 missing `username` value"
    )
    expect_false(check_result$visible)
    expect_equal(check_result$value, character(0))
  })
})

test_that("line and handle parsers cover comment and platform branches", {
  expect_null(voucher:::vouch_parse_line("# comment"))
  expect_null(voucher:::vouch_parse_line("   "))

  parsed_denounce <- voucher:::vouch_parse_line(
    "-github:alice reason",
    default_platform = "gitlab"
  )
  expect_equal(parsed_denounce$type, "denounce")
  expect_equal(parsed_denounce$username, "alice")
  expect_equal(parsed_denounce$platform, "github")

  parsed_vouch <- voucher:::vouch_parse_line(
    "alice trailing details",
    default_platform = "GitLab"
  )
  expect_equal(parsed_vouch$type, "vouch")
  expect_equal(parsed_vouch$username, "alice")
  expect_equal(parsed_vouch$platform, "gitlab")

  split_explicit <- voucher:::vouch_split_handle("github:alice:team", "gitlab")
  expect_equal(split_explicit$raw_platform, "github")
  expect_equal(split_explicit$username, "alice:team")
  expect_equal(split_explicit$platform, "github")

  split_default <- voucher:::vouch_split_handle("alice", "GitLab")
  expect_equal(split_default$raw_platform, "")
  expect_equal(split_default$username, "alice")
  expect_equal(split_default$platform, "gitlab")
})

test_that("file resolution helper branches", {
  vouch_with_temp_project({
    expect_error(
      voucher:::vouch_resolve_existing_file(),
      "no VOUCHED file found"
    )
    expect_equal(
      voucher:::vouch_resolve_existing_file("custom.td"),
      "custom.td"
    )
  })
})

test_that("vouch_git_blame_username covers guard and porcelain parsing branches", {
  expect_equal(
    voucher:::vouch_git_blame_username("VOUCHED.td", NA_integer_),
    ""
  )

  expect_equal(
    with_mocked_bindings(
      voucher:::vouch_git_blame_username("VOUCHED.td", 1L),
      Sys.which = function(...) "",
      .package = "base"
    ),
    ""
  )

  expect_equal(
    with_mocked_bindings(
      voucher:::vouch_git_blame_username("VOUCHED.td", 1L),
      Sys.which = function(...) "/fake/git",
      system2 = function(...) {
        out <- "fatal: bad revision"
        attr(out, "status") <- 128L
        out
      },
      .package = "base"
    ),
    ""
  )

  expect_equal(
    with_mocked_bindings(
      voucher:::vouch_git_blame_username("VOUCHED.td", 1L),
      Sys.which = function(...) "/fake/git",
      system2 = function(...) stop("boom"),
      .package = "base"
    ),
    ""
  )

  expect_equal(
    with_mocked_bindings(
      voucher:::vouch_git_blame_username("VOUCHED.td", 1L),
      Sys.which = function(...) "/fake/git",
      system2 = function(...) c("committer Someone", "summary line"),
      .package = "base"
    ),
    ""
  )

  expect_equal(
    with_mocked_bindings(
      voucher:::vouch_git_blame_username("VOUCHED.td", 1L),
      Sys.which = function(...) "/fake/git",
      system2 = function(...) {
        c("author Jane Doe", "author-mail <jane@example.com>")
      },
      .package = "base"
    ),
    "Jane Doe"
  )
})
