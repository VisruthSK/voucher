# vouch_gha validates action argument

    Code
      voucher:::vouch_gha("unknown-action")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "check-issue", "check-pr", "check-user", "manage-by-discussion", "manage-by-issue", "setup-vouch", "sync-codeowners"

# vouch_gha errors when action is missing

    Code
      voucher:::vouch_gha()
    Condition
      Error in `voucher:::vouch_gha()`:
      ! `action` must be provided.
      x No workflow action was selected

