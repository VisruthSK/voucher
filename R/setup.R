#' Initialize vouch in the current project
#'
#' Creates a starter `VOUCHED.td` file at `.github/VOUCHED.td` and ensures
#' `.github` is ignored in package builds by adding `^\\.github$` to
#' `.Rbuildignore` when needed.
#'
#' If a vouch file already exists (`VOUCHED.td` or `.github/VOUCHED.td`), this
#' function exits without making changes.
#'
#' @return Invisibly returns `NULL`. Called for the side effects of writing a
#'   starter `VOUCHED.td` file and updating `.Rbuildignore`.
#'
#' @examples
#' \dontrun{
#' project <- file.path(tempdir(), "voucher-use-vouch-example")
#' dir.create(project, recursive = TRUE)
#' old <- setwd(project)
#' on.exit(setwd(old), add = TRUE)
#' use_vouch()
#' }
#'
#' @export
use_vouch <- function() {
  if (is_vouch_project()) {
    return(invisible(NULL))
  }

  vouched_path <- fs::path(".github", "VOUCHED.td")
  rbuildignore_path <- ".Rbuildignore"

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
    !fs::file_exists(rbuildignore_path) ||
      !("^\\.github$" %in% readLines(rbuildignore_path, warn = FALSE))
  ) {
    cat("^\\.github$", "\n", file = rbuildignore_path, append = TRUE, sep = "")
  }
  cli::cli_alert_info(
    "Wrote vouch database to {.path {vouched_path}}."
  )
  invisible(NULL)
}
