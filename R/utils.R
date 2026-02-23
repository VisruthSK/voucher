exit_if_vouch_project <- function(
  paths = c("VOUCHED.td", ".github/VOUCHED.td")
) {
  found <- paths[fs::file_exists(paths)]
  if (length(found) > 0) {
    cli::cli_alert_info(
      "Existing vouch database found at {.path {found}}. Exiting without any changes."
    )
    return(TRUE)
  }
  FALSE
}

#' @param filename path to file, including extensions, where `text` should be written
write_to_path <- function(text, filepath) {
  fs::dir_create(fs::path_dir(filepath), recurse = TRUE)
  writeLines(text, filepath)
}
