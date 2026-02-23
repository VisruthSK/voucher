use_vouch <- function() {
  if (exit_if_vouch_project()) {
    return(invisible(NULL))
  }

  vouched_path <- fs::path(".github", "VOUCHED.td")

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
  write_to_path(default_vouched_text, vouched_path)

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
