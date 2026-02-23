use_vouch <- function() {
  if (fs::file_exists(c("VOUCHED.td", ".github/VOUCHED.td")) |> any()) {
    message("Existing vouch database found. Exiting without any changes.")
    return()
  }

  path <- fs::path(".github", "VOUCHED.td")
  fs::dir_create(fs::path_dir(path), recurse = TRUE)

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
    "#   - Optional details after a space following the handle.",
    ""
  )
  writeLines(default_vouched_text, path)

  if (
    !fs::file_exists(".Rbuildignore") ||
      !("^\\.github$" %in% readLines(".Rbuildignore", warn = FALSE))
  ) {
    cat("^\\.github$", "\n", file = ".Rbuildignore", append = TRUE, sep = "")
  }

  invisible(NULL)
}
