# https://github.com/mitchellh/vouch/tree/main/action

#' @export
vouch_gha <- function(
  action = c(
    "check-issue",
    "check-pr",
    "manage-by-discussion",
    "manage-by-issue",
    "sync-codeowners"
  )
) {
  if (missing(action)) {
    cli::cli_abort(
      c(
        "{.arg action} must be provided.",
        "x" = "No workflow action was selected"
      )
    )
  }
  actions <- match.arg(action, several.ok = TRUE)
  workflow_paths <- character(length(actions))

  for (i in seq_along(actions)) {
    workflow_paths[[i]] <- fs::path(
      ".github",
      "workflows",
      paste0(actions[[i]], ".yaml")
    )

    find_vouch_workflow_template(actions[[i]]) |>
      readLines(warn = FALSE) |>
      write_to_path(workflow_paths[[i]])

    cli::cli_alert_info(
      "Wrote GitHub Actions workflow to {.path {workflow_paths[[i]]}}."
    )
  }

  invisible(workflow_paths)
}

find_vouch_workflow_template <- function(action, package = "voucher") {
  if (length(action) != 1L) {
    cli::cli_abort("{.arg action} must contain exactly one action name.")
  }

  installed_path <- system.file(
    "vouch_example_workflows",
    paste0(action, ".yaml"),
    package = package
  )
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  cli::cli_abort(
    "Could not find workflow template for action {.val {action}}."
  )
}
