vouch_parity_skip <- function() {
  testthat::skip_on_cran()

  if (!identical(tolower(Sys.getenv("CI")), "true")) {
    testthat::skip("vouch parity tests run only in CI")
  }

  if (!nzchar(Sys.which("vouch"))) {
    testthat::skip("vouch CLI is not available on PATH")
  }
}

vouch_with_dir <- function(path, code) {
  old_wd <- setwd(path)
  on.exit(setwd(old_wd), add = TRUE)
  force(code)
}

vouch_with_temp_project <- function(code, prefix = "voucher-test-") {
  path <- tempfile(prefix)
  dir.create(path, recursive = TRUE)
  vouch_with_dir(path, code)
}

vouch_cli <- function(args) {
  cli_call <- vouch_cli_call(args)

  output <- suppressWarnings(
    system2(
      command = cli_call$command,
      args = shQuote(cli_call$args),
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

vouch_cli_call <- function(args) {
  vouch_bin <- Sys.which("vouch")
  nu_bin <- Sys.which("nu")

  if (identical(basename(dirname(vouch_bin)), ".vouch-bin") && nzchar(nu_bin)) {
    vouch_mod <- file.path(dirname(dirname(vouch_bin)), "vouch")
    if (file.exists(vouch_mod)) {
      return(list(
        command = nu_bin,
        args = c("--no-config-file", "-c", vouch_nu_command(vouch_mod, args))
      ))
    }
  }

  list(command = "vouch", args = args)
}

vouch_nu_command <- function(vouch_mod, args) {
  paste(
    c(
      "use",
      vouch_nu_quote(vouch_mod),
      "*;",
      vapply(args, vouch_nu_quote, character(1))
    ),
    collapse = " "
  )
}

vouch_nu_quote <- function(arg) {
  if (!grepl("[[:space:]\"\\\\]", arg)) {
    return(arg)
  }

  escaped <- gsub("\\\\", "\\\\\\\\", arg)
  escaped <- gsub("\"", "\\\\\"", escaped)
  paste0("\"", escaped, "\"")
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

vouch_new_pair_files <- function(lines, prefix = "voucher-vouch-") {
  path <- tempfile(prefix)
  dir.create(path)

  voucher_file <- file.path(path, "voucher.td")
  vouch_file <- file.path(path, "vouch.td")
  writeLines(lines, voucher_file)
  writeLines(lines, vouch_file)

  list(
    voucher_file = voucher_file,
    vouch_file = vouch_file
  )
}

vouch_new_pair_projects <- function(
  root_lines = NULL,
  github_lines = NULL,
  prefix = "voucher-project-"
) {
  voucher_dir <- tempfile(paste0(prefix, "r-"))
  vouch_dir <- tempfile(paste0(prefix, "v-"))
  vouch_new_project(
    voucher_dir,
    root_lines = root_lines,
    github_lines = github_lines
  )
  vouch_new_project(
    vouch_dir,
    root_lines = root_lines,
    github_lines = github_lines
  )
  list(voucher_dir = voucher_dir, vouch_dir = vouch_dir)
}

vouch_expect_update_parity <- function(
  voucher_result,
  vouch_result,
  write,
  voucher_file,
  vouch_file,
  initial_lines = NULL,
  info = NULL
) {
  testthat::expect_false(voucher_result$visible, info = info)
  testthat::expect_equal(vouch_result$status, 0L, info = info)

  if (isTRUE(write)) {
    testthat::expect_equal(
      vouch_td(voucher_file),
      vouch_td(vouch_file),
      info = info
    )
    testthat::expect_equal(
      voucher_result$value,
      vouch_td(voucher_file),
      info = info
    )
    return(invisible(NULL))
  }

  testthat::expect_equal(voucher_result$value, vouch_result$output, info = info)
  testthat::expect_equal(
    readLines(voucher_file, warn = FALSE),
    initial_lines,
    info = info
  )
  testthat::expect_equal(
    readLines(vouch_file, warn = FALSE),
    initial_lines,
    info = info
  )
}

vouch_expect_check_parity <- function(
  voucher_result,
  vouch_result,
  expected_code,
  info = NULL
) {
  testthat::expect_false(voucher_result$visible, info = info)
  testthat::expect_equal(
    voucher_result$value,
    vouch_status_from_output(vouch_result$output),
    info = info
  )
  testthat::expect_equal(vouch_result$status, expected_code, info = info)
}

vouch_run <- function(command, ...) {
  fn <- get(command, envir = asNamespace("voucher"))
  suppressMessages(withVisible(do.call(fn, list(...))))
}

vouch_args <- function(
  command,
  username,
  write = FALSE,
  reason = "",
  default_platform = "",
  vouched_file = ""
) {
  args <- c(command, username)
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
