# https://github.com/mitchellh/vouch/tree/main/action

vouch_gha <- function(
  action = c(
    "check-issue",
    "check-pr",
    "check-user",
    "manage-by-discussion",
    "manage-by-issue",
    "setup-vouch",
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
  action <- match.arg(action, several.ok = TRUE)

  workflow_path <- fs::path(".github", "workflows", paste0(action, ".yaml"))
  find_vouch_workflow_template(action) |>
    readLines(warn = FALSE) |>
    write_to_path(workflow_path)

  cli::cli_alert_info(
    "Wrote GitHub Actions workflow to {.path {workflow_path}}."
  )

  invisible(workflow_path)
}

find_vouch_workflow_template <- function(action, package = "voucher") {
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
