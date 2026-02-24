# Use vouch GitHub Actions

Function to add some vouch workflows, like
`usethis::use_github_action()`. Details about the actions can be found
at <https://github.com/mitchellh/vouch/tree/main/action>; short
summaries copied from there follow.

## Usage

``` r
vouch_gha(
  action = c("check-issue", "check-pr", "manage-by-discussion", "manage-by-issue",
    "sync-codeowners")
)
```

## Arguments

- action:

  The GitHub Action template workflow to add.

## Details

check-issue: Check if an issue author is a vouched contributor. Bots and
collaborators with write access are automatically allowed. Denounced
users are always blocked. When require-vouch is true (default),
unvouched users are also blocked. Use auto-close to close issues from
blocked users.

check-pr: Check if a PR author is a vouched contributor. Bots and
collaborators with write access are automatically allowed. Denounced
users are always blocked. When require-vouch is true (default),
unvouched users are also blocked. Use auto-close to close PRs from
blocked users.

manage-by-discussion: Manage contributor vouch status via discussion
comments. When a collaborator with sufficient permissions comments vouch
on a discussion, the discussion author is added to the vouched
contributors list. When they comment denounce, the user is denounced.
When they comment unvouch, the user is removed from the list entirely.
The trigger keywords and required permission levels are configurable.

manage-by-issue: Manage contributor vouch status via issue comments.
When a collaborator with sufficient permissions comments vouch on an
issue, the issue author is added to the vouched contributors list. When
they comment denounce, the user is denounced. When they comment unvouch,
the user is removed from the list entirely. The trigger keywords and
required permission levels are configurable.

sync-codeowners: Sync CODEOWNERS entries into the VOUCHED list. The
action expands any team owners to their members and adds missing users
to the vouch file.

## Attribution

Documentation for this function is copied nearly verbatim from
[`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell
Hashimoto.
