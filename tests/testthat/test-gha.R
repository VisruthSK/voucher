# jarl-ignore-file internal_function: tests intentionally exercise voucher internal functions.
test_that("vouch_gha writes every workflow from inst templates", {
  actions <- c(
    "check-issue",
    "check-pr",
    "manage-by-discussion",
    "manage-by-issue",
    "sync-codeowners"
  )

  vouch_with_temp_project({
    for (action in actions) {
      expect_invisible(suppressMessages(voucher:::vouch_gha(action)))

      workflow_path <- fs::path(".github", "workflows", paste0(action, ".yaml"))
      template_path <- voucher:::find_vouch_workflow_template(action)

      expect_true(file.exists(workflow_path), info = action)
      expect_equal(
        readLines(workflow_path, warn = FALSE),
        readLines(template_path, warn = FALSE),
        info = action
      )
    }
  })
})

test_that("vouch_gha supports writing multiple workflows in one call", {
  actions <- c(
    "check-issue",
    "check-pr",
    "manage-by-discussion",
    "manage-by-issue"
  )

  vouch_with_temp_project({
    workflow_paths <- fs::path(".github", "workflows", paste0(actions, ".yaml"))

    expect_invisible(suppressMessages(voucher:::vouch_gha(actions)))

    for (i in seq_along(actions)) {
      expect_true(file.exists(workflow_paths[[i]]), info = actions[[i]])
      expect_equal(
        readLines(workflow_paths[[i]], warn = FALSE),
        readLines(
          voucher:::find_vouch_workflow_template(actions[[i]]),
          warn = FALSE
        ),
        info = actions[[i]]
      )
    }
  })
})

test_that("vouch_gha validates action argument", {
  expect_snapshot_failure(voucher:::vouch_gha("unknown-action"))
})

test_that("vouch_gha errors when action is missing", {
  expect_snapshot_failure(voucher:::vouch_gha())
})

test_that("find_vouch_workflow_template errors when template is missing", {
  expect_error(
    voucher:::find_vouch_workflow_template(
      "not-a-real-template",
      package = "voucher"
    ),
    "Could not find workflow template"
  )
})

test_that("find_vouch_workflow_template errors when action length is not one", {
  expect_error(
    voucher:::find_vouch_workflow_template(c("check-issue", "check-pr")),
    "`action` must contain exactly one action name."
  )
})
