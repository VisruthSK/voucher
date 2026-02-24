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

vouch_read_td <- function(path) {
  paste0(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n")
}

vouch_default <- function(x, default = "") {
  if (is.null(x)) default else x
}

vouch_update_args <- function(case) {
  list(
    username = case$username,
    write = isTRUE(case$write),
    reason = vouch_default(case$reason),
    default_platform = vouch_default(case$default_platform)
  )
}

vouch_check_args_from_case <- function(case) {
  list(
    username = case$username,
    default_platform = vouch_default(case$default_platform)
  )
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

vouch_expect_update_file_cases <- function(command, cases, prefix) {
  for (case in cases) {
    files <- vouch_new_pair_files(case$initial, prefix = prefix)
    args <- vouch_update_args(case)
    voucher_result <- do.call(
      vouch_run,
      c(
        list(command = command),
        args,
        list(vouched_file = files$voucher_file)
      )
    )
    vouch_result <- vouch_cli(do.call(
      vouch_args,
      c(
        list(command = command),
        args,
        list(vouched_file = files$vouch_file)
      )
    ))

    testthat::expect_false(voucher_result$visible, info = case$name)
    testthat::expect_equal(vouch_result$status, 0L, info = case$name)
    if (isTRUE(case$write)) {
      testthat::expect_equal(
        vouch_read_td(files$voucher_file),
        vouch_read_td(files$vouch_file),
        info = case$name
      )
      testthat::expect_equal(
        voucher_result$value,
        vouch_read_td(files$voucher_file),
        info = case$name
      )
    } else {
      testthat::expect_equal(
        voucher_result$value,
        vouch_result$output,
        info = case$name
      )
      testthat::expect_equal(
        readLines(files$voucher_file, warn = FALSE),
        case$initial,
        info = case$name
      )
      testthat::expect_equal(
        readLines(files$vouch_file, warn = FALSE),
        case$initial,
        info = case$name
      )
    }
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
      root_lines = case$root_lines,
      github_lines = case$github_lines,
      prefix = prefix
    )
    voucher_result <- vouch_with_dir(
      projects$voucher_dir,
      do.call(vouch_run, c(list(command = command), args))
    )
    vouch_result <- vouch_with_dir(
      projects$vouch_dir,
      vouch_cli(do.call(vouch_args, c(list(command = command), args)))
    )
    target_r <- file.path(projects$voucher_dir, case$target)
    target_v <- file.path(projects$vouch_dir, case$target)

    testthat::expect_false(voucher_result$visible, info = case$name)
    testthat::expect_equal(vouch_result$status, 0L, info = case$name)
    testthat::expect_equal(
      vouch_read_td(target_r),
      vouch_read_td(target_v),
      info = case$name
    )
    testthat::expect_equal(
      voucher_result$value,
      vouch_read_td(target_r),
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
  projects <- vouch_new_pair_projects(
    root_lines = root_lines,
    github_lines = github_lines,
    prefix = prefix
  )
  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(override_lines, override_r)
  writeLines(override_lines, override_v)

  voucher_result <- vouch_with_dir(
    projects$voucher_dir,
    do.call(
      vouch_run,
      c(list(command = command), args, list(vouched_file = override_r))
    )
  )
  vouch_result <- vouch_with_dir(
    projects$vouch_dir,
    vouch_cli(do.call(
      vouch_args,
      c(list(command = command), args, list(vouched_file = override_v))
    ))
  )

  testthat::expect_false(voucher_result$visible)
  testthat::expect_equal(vouch_result$status, 0L)
  testthat::expect_equal(vouch_read_td(override_r), vouch_read_td(override_v))
  testthat::expect_equal(voucher_result$value, vouch_read_td(override_r))
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

vouch_expect_check_file_cases <- function(lines, cases, prefix) {
  files <- vouch_new_pair_files(lines, prefix = prefix)

  for (case in cases) {
    args <- vouch_check_args_from_case(case)
    voucher_result <- do.call(
      vouch_run,
      c(
        list(command = "check"),
        args,
        list(vouched_file = files$voucher_file)
      )
    )
    vouch_result <- vouch_cli(do.call(
      vouch_args,
      c(
        list(command = "check"),
        args,
        list(vouched_file = files$vouch_file)
      )
    ))
    info <- paste(case$username, case$default_platform, sep = " | ")

    testthat::expect_false(voucher_result$visible, info = info)
    testthat::expect_equal(
      voucher_result$value,
      vouch_status_from_output(vouch_result$output),
      info = info
    )
    testthat::expect_equal(vouch_result$status, case$code, info = info)
  }
}

vouch_expect_check_default_path_cases <- function(cases, prefix) {
  for (case in cases) {
    projects <- vouch_new_pair_projects(
      root_lines = case$root_lines,
      github_lines = case$github_lines,
      prefix = prefix
    )
    voucher_result <- vouch_with_dir(
      projects$voucher_dir,
      vouch_run("check", username = case$username)
    )
    vouch_result <- vouch_with_dir(
      projects$vouch_dir,
      vouch_cli(vouch_args("check", case$username))
    )

    testthat::expect_false(voucher_result$visible, info = case$name)
    testthat::expect_equal(
      voucher_result$value,
      vouch_status_from_output(vouch_result$output),
      info = case$name
    )
    testthat::expect_equal(vouch_result$status, case$code, info = case$name)
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
  projects <- vouch_new_pair_projects(
    root_lines = root_lines,
    github_lines = github_lines,
    prefix = prefix
  )
  override_r <- file.path(projects$voucher_dir, "override.td")
  override_v <- file.path(projects$vouch_dir, "override.td")
  writeLines(override_lines, override_r)
  writeLines(override_lines, override_v)

  voucher_result <- vouch_with_dir(
    projects$voucher_dir,
    do.call(
      vouch_run,
      c(list(command = "check"), args, list(vouched_file = override_r))
    )
  )
  vouch_result <- vouch_with_dir(
    projects$vouch_dir,
    vouch_cli(do.call(
      vouch_args,
      c(list(command = "check"), args, list(vouched_file = override_v))
    ))
  )

  testthat::expect_false(voucher_result$visible)
  testthat::expect_equal(
    voucher_result$value,
    vouch_status_from_output(vouch_result$output)
  )
  testthat::expect_equal(vouch_result$status, expected_code)
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
