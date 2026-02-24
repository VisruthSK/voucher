# jarl-ignore-file internal_function: parity helpers intentionally call voucher internals.
vouch_parity_skip <- function() {
  testthat::skip_on_cran()

  if (!identical(tolower(Sys.getenv("CI")), "true")) {
    testthat::skip("vouch parity tests run only in CI")
  }

  if (!nzchar(Sys.which("vouch"))) {
    testthat::skip("vouch CLI is not available on PATH")
  }
}

vouch_cli <- function(args) {
  output <- suppressWarnings(
    system2(
      command = "vouch",
      args = shQuote(args),
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
    output = paste0(paste(output, collapse = "\n"), "\n")
  )
}

vouch_td <- function(path) {
  paste0(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n")
}

vouch_status_from_output <- function(output) {
  for (status in c("vouched", "denounced", "unknown")) {
    if (grepl(paste0(" is ", status, "\\s*$"), output)) {
      return(status)
    }
  }

  NA_character_
}

vouch_with_dir <- function(path, code) {
  old_wd <- setwd(path)
  on.exit(setwd(old_wd), add = TRUE)
  force(code)
}

vouch_new_project <- function(path, root_lines = NULL, github_lines = NULL) {
  dir.create(path, recursive = TRUE)
  if (!is.null(root_lines)) {
    writeLines(root_lines, file.path(path, "VOUCHED.td"))
  }
  if (!is.null(github_lines)) {
    dir.create(file.path(path, ".github"), recursive = TRUE)
    writeLines(github_lines, file.path(path, ".github", "VOUCHED.td"))
  }
}

vouch_run_add <- function(
  username,
  write = FALSE,
  default_platform = "",
  vouched_file = ""
) {
  suppressMessages(withVisible(voucher:::add(
    username = username,
    write = write,
    default_platform = default_platform,
    vouched_file = vouched_file
  )))
}

vouch_run_denounce <- function(
  username,
  write = FALSE,
  reason = "",
  default_platform = "",
  vouched_file = ""
) {
  suppressMessages(withVisible(voucher:::denounce(
    username = username,
    write = write,
    reason = reason,
    default_platform = default_platform,
    vouched_file = vouched_file
  )))
}

vouch_run_check <- function(
  username,
  default_platform = "",
  vouched_file = ""
) {
  args <- list(username = username, default_platform = default_platform)
  if (nzchar(vouched_file)) {
    args$vouched_file <- vouched_file
  }

  suppressMessages(withVisible(do.call(voucher:::check, args)))
}

vouch_add_args <- function(
  username,
  write = FALSE,
  default_platform = "",
  vouched_file = ""
) {
  args <- c("add", username)
  if (isTRUE(write)) {
    args <- c(args, "--write")
  }
  if (nzchar(default_platform)) {
    args <- c(args, "--default-platform", default_platform)
  }
  if (nzchar(vouched_file)) {
    args <- c(args, "--vouched-file", vouched_file)
  }
  args
}

vouch_denounce_args <- function(
  username,
  write = FALSE,
  reason = "",
  default_platform = "",
  vouched_file = ""
) {
  args <- c("denounce", username)
  if (isTRUE(write)) {
    args <- c(args, "--write")
  }
  if (nzchar(reason)) {
    args <- c(args, "--reason", reason)
  }
  if (nzchar(default_platform)) {
    args <- c(args, "--default-platform", default_platform)
  }
  if (nzchar(vouched_file)) {
    args <- c(args, "--vouched-file", vouched_file)
  }
  args
}

vouch_check_args <- function(
  username,
  default_platform = "",
  vouched_file = ""
) {
  args <- c("check", username)
  if (nzchar(default_platform)) {
    args <- c(args, "--default-platform", default_platform)
  }
  if (nzchar(vouched_file)) {
    args <- c(args, "--vouched-file", vouched_file)
  }
  args
}
