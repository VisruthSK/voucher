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
  call <- vouch_cli_call(args)
  output <- suppressWarnings(system2(
    command = call$command,
    args = shQuote(call$args),
    stdout = TRUE,
    stderr = TRUE,
    env = c("NO_COLOR=1")
  ))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  list(status = status, output = paste0(paste(output, collapse = "\n"), "\n"))
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

vouch_status_from_output <- function(output) {
  for (status in c("vouched", "denounced", "unknown")) {
    if (grepl(paste0(" is ", status, "\\s*$"), output)) {
      return(status)
    }
  }
  NA_character_
}

vouch_read_td <- function(path) {
  paste0(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n")
}

vouch_default <- function(x) {
  if (is.null(x)) "" else x
}

vouch_update_args <- function(case) {
  list(
    username = case$username,
    write = isTRUE(case$write),
    reason = vouch_default(case$reason),
    default_platform = vouch_default(case$default_platform)
  )
}

vouch_check_args <- function(case) {
  list(
    username = case$username,
    default_platform = vouch_default(case$default_platform)
  )
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

vouch_new_pair_files <- function(lines, prefix = "voucher-vouch-") {
  path <- tempfile(prefix)
  dir.create(path)
  voucher_file <- file.path(path, "voucher.td")
  vouch_file <- file.path(path, "vouch.td")
  writeLines(lines, voucher_file)
  writeLines(lines, vouch_file)
  list(voucher_file = voucher_file, vouch_file = vouch_file)
}

vouch_pair <- function(command, args, voucher_file, vouch_file) {
  list(
    voucher = do.call(
      vouch_run,
      c(list(command = command), args, list(vouched_file = voucher_file))
    ),
    vouch = vouch_cli(do.call(
      vouch_args,
      c(list(command = command), args, list(vouched_file = vouch_file))
    ))
  )
}

vouch_pair_in_dirs <- function(command, args, voucher_dir, vouch_dir) {
  list(
    voucher = vouch_with_dir(
      voucher_dir,
      do.call(vouch_run, c(list(command = command), args))
    ),
    vouch = vouch_with_dir(
      vouch_dir,
      vouch_cli(do.call(vouch_args, c(list(command = command), args)))
    )
  )
}

vouch_expect_update_pair <- function(
  results,
  write,
  voucher_file,
  vouch_file,
  initial_lines = NULL,
  info = NULL
) {
  testthat::expect_false(results$voucher$visible, info = info)
  testthat::expect_equal(results$vouch$status, 0L, info = info)
  if (isTRUE(write)) {
    testthat::expect_equal(
      vouch_read_td(voucher_file),
      vouch_read_td(vouch_file),
      info = info
    )
    testthat::expect_equal(
      results$voucher$value,
      vouch_read_td(voucher_file),
      info = info
    )
    return(invisible(NULL))
  }
  testthat::expect_equal(
    results$voucher$value,
    results$vouch$output,
    info = info
  )
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

vouch_expect_check_pair <- function(results, code, info = NULL) {
  testthat::expect_false(results$voucher$visible, info = info)
  testthat::expect_equal(
    results$voucher$value,
    vouch_status_from_output(results$vouch$output),
    info = info
  )
  testthat::expect_equal(results$vouch$status, code, info = info)
}

vouch_expect_project_lines <- function(projects, root_lines, github_lines) {
  testthat::expect_equal(
    readLines(file.path(projects$voucher_dir, "VOUCHED.td"), warn = FALSE),
    root_lines
  )
  testthat::expect_equal(
    readLines(
      file.path(projects$voucher_dir, ".github", "VOUCHED.td"),
      warn = FALSE
    ),
    github_lines
  )
}

vouch_expect_update_file_cases <- function(command, cases, prefix) {
  for (case in cases) {
    files <- vouch_new_pair_files(case$initial, prefix = prefix)
    results <- vouch_pair(
      command,
      vouch_update_args(case),
      files$voucher_file,
      files$vouch_file
    )
    vouch_expect_update_pair(
      results,
      isTRUE(case$write),
      files$voucher_file,
      files$vouch_file,
      case$initial,
      case$name
    )
  }
}

vouch_expect_update_default_path_cases <- function(
  command,
  cases,
  args,
  prefix
) {
  for (case in cases) {
    projects <- vouch_new_pair_projects(
      case$root_lines,
      case$github_lines,
      prefix = prefix
    )
    results <- vouch_pair_in_dirs(
      command,
      args,
      projects$voucher_dir,
      projects$vouch_dir
    )
    vouch_expect_update_pair(
      results,
      TRUE,
      file.path(projects$voucher_dir, case$target),
      file.path(projects$vouch_dir, case$target),
      info = case$name
    )
  }
}

vouch_expect_update_override <- function(
  command,
  args,
  override_lines,
  root_lines,
  github_lines,
  prefix
) {
  projects <- vouch_new_pair_projects(root_lines, github_lines, prefix = prefix)
  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(override_lines, override_r)
  writeLines(override_lines, override_v)
  results <- vouch_pair(command, args, override_r, override_v)
  vouch_expect_update_pair(results, TRUE, override_r, override_v)
  vouch_expect_project_lines(projects, root_lines, github_lines)
}

vouch_expect_check_file_cases <- function(lines, cases, prefix) {
  files <- vouch_new_pair_files(lines, prefix = prefix)
  for (case in cases) {
    results <- vouch_pair(
      "check",
      vouch_check_args(case),
      files$voucher_file,
      files$vouch_file
    )
    vouch_expect_check_pair(
      results,
      case$code,
      paste(case$username, case$default_platform, sep = " | ")
    )
  }
}

vouch_expect_check_default_path_cases <- function(cases, prefix) {
  for (case in cases) {
    projects <- vouch_new_pair_projects(
      case$root_lines,
      case$github_lines,
      prefix = prefix
    )
    results <- vouch_pair_in_dirs(
      "check",
      list(username = case$username),
      projects$voucher_dir,
      projects$vouch_dir
    )
    vouch_expect_check_pair(results, case$code, case$name)
  }
}

vouch_expect_check_override <- function(
  override_lines,
  root_lines,
  github_lines,
  args = list(username = "bob"),
  expected_code = 1L,
  prefix = "voucher-check-override-"
) {
  projects <- vouch_new_pair_projects(root_lines, github_lines, prefix = prefix)
  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(override_lines, override_r)
  writeLines(override_lines, override_v)
  results <- vouch_pair("check", args, override_r, override_v)
  vouch_expect_check_pair(results, expected_code)
  vouch_expect_project_lines(projects, root_lines, github_lines)
}

vouch_expect_missing_file <- function(command, args, prefix) {
  voucher_dir <- tempfile(paste0(prefix, "r-"))
  vouch_dir <- tempfile(paste0(prefix, "v-"))
  dir.create(voucher_dir)
  dir.create(vouch_dir)
  voucher_error <- vouch_with_dir(
    voucher_dir,
    tryCatch(
      {
        do.call(vouch_run, c(list(command = command), args))
        ""
      },
      error = function(e) conditionMessage(e)
    )
  )
  vouch_error <- vouch_with_dir(
    vouch_dir,
    vouch_cli(do.call(vouch_args, c(list(command = command), args)))
  )
  testthat::expect_match(voucher_error, "no VOUCHED file found")
  testthat::expect_true(vouch_error$status != 0L)
  testthat::expect_match(vouch_error$output, "no VOUCHED file found")
}
