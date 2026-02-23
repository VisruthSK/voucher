use_vouch <- function() {
  paths <- c("VOUCHED.td", ".github/VOUCHED.td")
  found <- paths[fs::file_exists(paths)]
  if (length(found) > 0) {
    cli::cli_alert_info(
      "Existing vouch database found at {.path {found}}. Exiting without any changes."
    )
    return(invisible(NULL))
  }

  vouched_path <- fs::path(".github", "VOUCHED.td")
  fs::dir_create(fs::path_dir(vouched_path), recurse = TRUE)

  default_vouched_text <- c(
    "# Copied from vouch repo:",
    "# Vouched contributors for this project.",
    "#",
    "# See https://github.com/mitchellh/vouch for details.",
    "#",
    "# Syntax:",
    "#   - One handle per line (without @), sorted alphabetically.",
    "#   - Optional platform prefix: platform:username (e.g., github:user).",
    "#   - Denounce with minus prefix: -username or -platform:username.",
    "#   - Optional details after a space following the handle."
  )
  writeLines(default_vouched_text, vouched_path)

  if (
    !fs::file_exists(".Rbuildignore") ||
      !("^\\.github$" %in% readLines(".Rbuildignore", warn = FALSE))
  ) {
    cat("^\\.github$", "\n", file = ".Rbuildignore", append = TRUE, sep = "")
  }
  cli::cli_alert_info(
    "Wrote vouch database to {.path {vouched_path}}."
  )
  invisible(NULL)
}
