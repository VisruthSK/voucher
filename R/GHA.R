# https://github.com/mitchellh/vouch/tree/main/action

#' Use 'vouch' GitHub Actions
#'
#' Function to add some 'vouch' workflows, like
#' `usethis::use_github_action()`. Details about the actions can be found at
#' <https://github.com/mitchellh/vouch/tree/main/action>; short summaries
#' copied from there follow.
#'
#' check-issue: Check if an issue author is a vouched contributor. Bots and collaborators with write access are automatically allowed. Denounced users are always blocked. When require-vouch is true (default), unvouched users are also blocked. Use auto-close to close issues from blocked users.
#'
#' check-pr: Check if a PR author is a vouched contributor. Bots and collaborators with write access are automatically allowed. Denounced users are always blocked. When require-vouch is true (default), unvouched users are also blocked. Use auto-close to close PRs from blocked users.
#'
#' manage-by-discussion: Manage contributor vouch status via discussion comments. When a collaborator with sufficient permissions comments vouch on a discussion, the discussion author is added to the vouched contributors list. When they comment denounce, the user is denounced. When they comment unvouch, the user is removed from the list entirely. The trigger keywords and required permission levels are configurable.
#'
#' manage-by-issue: Manage contributor vouch status via issue comments. When a collaborator with sufficient permissions comments vouch on an issue, the issue author is added to the vouched contributors list. When they comment denounce, the user is denounced. When they comment unvouch, the user is removed from the list entirely. The trigger keywords and required permission levels are configurable.
#'
#' sync-codeowners: Sync CODEOWNERS entries into the VOUCHED list. The action expands any team owners to their members and adds missing users to the vouch file.
#'
#' @param action The GitHub Action template workflow to add.
#' @param save_as Name of the local workflow file. Defaults to `action` with a
#'   `.yaml` extension. Do not specify any other part of the path; workflow
#'   files are always written under `.github/workflows`.
#'
#' @return Invisibly returns a character vector of workflow file paths. Each
#'   element is a length-1 character string giving the path written for the
#'   corresponding requested action under `.github/workflows`.
#'
#' @examples
#' \dontrun{
#' project <- file.path(tempdir(), "voucher-gha-example")
#' dir.create(project, recursive = TRUE)
#' old <- setwd(project)
#' on.exit(setwd(old), add = TRUE)
#' vouch_gha("check-issue")
#' }
#'
#' @section Attribution:
#' Documentation for this function is copied nearly verbatim from [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell Hashimoto.
#' @export
vouch_gha <- function(
  action = c(
    "check-issue",
    "check-pr",
    "manage-by-discussion",
    "manage-by-issue",
    "sync-codeowners"
  ),
  save_as = NULL
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
  if (is.null(save_as)) {
    save_as <- actions
  }
  save_as <- as.character(save_as)
  if (length(save_as) != length(actions)) {
    cli::cli_abort("{.arg save_as} must have the same length as {.arg action}.")
  }
  if (any(grepl("[/\\\\]", save_as))) {
    cli::cli_abort(
      "{.arg save_as} must be file names, not paths. Files are written under {.path .github/workflows}."
    )
  }
  save_as <- vapply(
    save_as,
    \(x) if (fs::path_ext(x) == "") fs::path_ext_set(x, "yaml") else x,
    character(1)
  )
  template_paths <- vapply(actions, find_vouch_workflow_template, character(1))
  workflow_paths <- fs::path(".github", "workflows", save_as)

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
