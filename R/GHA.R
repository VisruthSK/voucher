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
  action <- match.arg(action)

  workflow_path <- fs::path(".github", "workflows", paste0(action, ".yaml"))
  template_lines <- readLines(
    find_vouch_workflow_template(action),
    warn = FALSE
  )

  write_to_path(template_lines, workflow_path)
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
