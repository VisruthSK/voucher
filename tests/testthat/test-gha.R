# jarl-ignore-file internal_function: tests intentionally exercise voucher internal functions.
test_that("vouch_gha writes every workflow from inst templates", {
  actions <- c(
    "check-issue",
    "check-pr",
    "check-user",
    "manage-by-discussion",
    "manage-by-issue",
    "setup-vouch",
    "sync-codeowners"
  )

  temp_proj <- tempfile("voucher-test-")
  dir.create(temp_proj)
  old_wd <- setwd(temp_proj)
  on.exit(setwd(old_wd), add = TRUE)

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
