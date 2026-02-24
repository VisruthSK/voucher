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
  template_paths <- vapply(actions, find_vouch_workflow_template, character(1))
  workflow_paths <- fs::path(".github", "workflows", paste0(actions, ".yaml"))

  Map(
    function(src, dst) {
      write_to_path(readLines(src, warn = FALSE), dst)
      cli::cli_alert_info("Wrote GitHub Actions workflow to {.path {dst}}.")
    },
    template_paths,
    workflow_paths
  )

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
