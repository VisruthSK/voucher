vouch_resolve_existing_file <- function(
  vouched_file = "",
  paths = c("VOUCHED.td", ".github/VOUCHED.td")
) {
  if (nzchar(vouched_file)) {
    return(vouched_file)
  }

  found <- paths[fs::file_exists(paths)]
  if (length(found) == 0L) {
    cli::cli_abort("no VOUCHED file found")
  }

  found[[1]]
}

is_vouch_project <- function(
  paths = c("VOUCHED.td", ".github/VOUCHED.td")
) {
  found <- tryCatch(
    vouch_resolve_existing_file(paths = paths),
    error = function(...) ""
  )

  if (nzchar(found)) {
    cli::cli_alert_info(
      "Existing vouch database found at {.path {found}}. Exiting without any changes."
    )
    return(TRUE)
  }
  FALSE
}

write_to_path <- function(text, filepath) {
  fs::dir_create(fs::path_dir(filepath), recurse = TRUE)
  writeLines(text, filepath)
}
