vouch_parity_skip_if_unavailable <- function() {
  testthat::skip_on_cran()

  if (!identical(tolower(Sys.getenv("CI")), "true")) {
    testthat::skip("vouch parity tests run only in CI")
  }

  if (!nzchar(Sys.which("vouch"))) {
    testthat::skip("vouch CLI is not available on PATH")
  }
}

vouch_parity_run_cli <- function(args) {
  output <- suppressWarnings(
    system2(
      command = "vouch",
      args = args,
      stdout = TRUE,
      stderr = TRUE,
      env = c("NO_COLOR=1")
    )
  )

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  list(
    status = status,
    output = if (length(output)) {
      paste0(paste(output, collapse = "\n"), "\n")
    } else {
      ""
    }
  )
}

vouch_parity_read_td <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) {
    return("")
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}

vouch_parity_extract_status <- function(output) {
  for (status in c("vouched", "denounced", "unknown")) {
    if (grepl(paste0(" is ", status, "\\s*$"), output)) {
      return(status)
    }
  }

  NA_character_
}

test_that("add preview matches vouch CLI", {
  vouch_parity_skip_if_unavailable()

  temp_proj <- tempfile("voucher-vouch-parity-")
  dir.create(temp_proj)

  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  initial <- c("# header", "zoe", "-github:bob stale", "alice")
  writeLines(initial, file_r)
  writeLines(initial, file_v)

  voucher_result <- suppressMessages(withVisible(voucher:::add(
    "github:bob",
    vouched_file = file_r
  )))
  vouch_result <- vouch_parity_run_cli(c(
    "add",
    "github:bob",
    "--vouched-file",
    file_v
  ))

  expect_false(voucher_result$visible)
  expect_equal(vouch_result$status, 0L)
  expect_equal(voucher_result$value, vouch_result$output)
  expect_equal(readLines(file_r, warn = FALSE), initial)
  expect_equal(readLines(file_v, warn = FALSE), initial)
})

test_that("add write matches vouch CLI", {
  vouch_parity_skip_if_unavailable()

  temp_proj <- tempfile("voucher-vouch-parity-")
  dir.create(temp_proj)

  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  initial <- c("# header", "github:zoe", "bob", "-gitlab:ana stale", "ana")
  writeLines(initial, file_r)
  writeLines(initial, file_v)

  voucher_result <- suppressMessages(withVisible(voucher:::add(
    "ana",
    write = TRUE,
    default_platform = "gitlab",
    vouched_file = file_r
  )))
  vouch_result <- vouch_parity_run_cli(c(
    "add",
    "ana",
    "--write",
    "--default-platform",
    "gitlab",
    "--vouched-file",
    file_v
  ))

  expect_false(voucher_result$visible)
  expect_equal(vouch_result$status, 0L)
  expect_equal(vouch_parity_read_td(file_r), vouch_parity_read_td(file_v))
  expect_equal(voucher_result$value, vouch_parity_read_td(file_r))
})

test_that("denounce preview matches vouch CLI", {
  vouch_parity_skip_if_unavailable()

  temp_proj <- tempfile("voucher-vouch-parity-")
  dir.create(temp_proj)

  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  initial <- c("# header", "zoe", "alice", "bob")
  writeLines(initial, file_r)
  writeLines(initial, file_v)

  voucher_result <- suppressMessages(withVisible(voucher:::denounce(
    "bob",
    reason = "AI slop",
    vouched_file = file_r
  )))
  vouch_result <- vouch_parity_run_cli(c(
    "denounce",
    "bob",
    "--reason",
    "AI slop",
    "--vouched-file",
    file_v
  ))

  expect_false(voucher_result$visible)
  expect_equal(vouch_result$status, 0L)
  expect_equal(voucher_result$value, vouch_result$output)
  expect_equal(readLines(file_r, warn = FALSE), initial)
  expect_equal(readLines(file_v, warn = FALSE), initial)
})

test_that("denounce write matches vouch CLI", {
  vouch_parity_skip_if_unavailable()

  temp_proj <- tempfile("voucher-vouch-parity-")
  dir.create(temp_proj)

  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  initial <- c("# header", "zoe", "alice", "bob")
  writeLines(initial, file_r)
  writeLines(initial, file_v)

  reason <- "suspicious behavior"
  voucher_result <- suppressMessages(withVisible(voucher:::denounce(
    "bob",
    write = TRUE,
    reason = reason,
    vouched_file = file_r
  )))
  vouch_result <- vouch_parity_run_cli(c(
    "denounce",
    "bob",
    "--write",
    "--reason",
    reason,
    "--vouched-file",
    file_v
  ))

  expect_false(voucher_result$visible)
  expect_equal(vouch_result$status, 0L)
  expect_true(grepl("Denounced", vouch_result$output, fixed = TRUE))
  expect_equal(vouch_parity_read_td(file_r), vouch_parity_read_td(file_v))
  expect_equal(voucher_result$value, vouch_parity_read_td(file_r))
})

test_that("check matches vouch CLI status and exit codes", {
  vouch_parity_skip_if_unavailable()

  temp_proj <- tempfile("voucher-vouch-parity-")
  dir.create(temp_proj)

  file_r <- file.path(temp_proj, "voucher.td")
  file_v <- file.path(temp_proj, "vouch.td")
  initial <- c("alice", "-github:bob reason", "gitlab:charlie")
  writeLines(initial, file_r)
  writeLines(initial, file_v)

  cases <- list(
    list(
      username = "alice",
      default_platform = "",
      expected = "vouched",
      code = 0L
    ),
    list(
      username = "github:bob",
      default_platform = "",
      expected = "denounced",
      code = 1L
    ),
    list(
      username = "bob",
      default_platform = "github",
      expected = "denounced",
      code = 1L
    ),
    list(
      username = "charlie",
      default_platform = "gitlab",
      expected = "vouched",
      code = 0L
    ),
    list(
      username = "nobody",
      default_platform = "",
      expected = "unknown",
      code = 2L
    )
  )

  for (case in cases) {
    voucher_result <- suppressMessages(withVisible(voucher:::check(
      username = case$username,
      default_platform = case$default_platform,
      vouched_file = file_r
    )))

    vouch_args <- c("check", case$username)
    if (nzchar(case$default_platform)) {
      vouch_args <- c(vouch_args, "--default-platform", case$default_platform)
    }
    vouch_args <- c(vouch_args, "--vouched-file", file_v)
    vouch_result <- vouch_parity_run_cli(vouch_args)

    expect_false(voucher_result$visible, info = case$username)
    expect_equal(voucher_result$value, case$expected, info = case$username)
    expect_equal(
      voucher_result$value,
      vouch_parity_extract_status(vouch_result$output),
      info = case$username
    )
    expect_equal(vouch_result$status, case$code, info = case$username)
  }
})
