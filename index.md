# voucher

[voucher](https://github.com/VisruthSK/voucher) is a R interface to
[`vouch`](https://github.com/mitchellh/vouch/), a “community trust
management system based on explicit vouches to participate.”

## What `voucher` provides

Subset of `vouch`’s behavior behavior:

- Initialize a project trust file (`VOUCHED.td`) with
  [`use_vouch()`](https://visruthsk.github.io/voucher/reference/use_vouch.md)
- Add vouched contributors with
  [`add()`](https://visruthsk.github.io/voucher/reference/add.md)
- Denounce contributors with
  [`denounce()`](https://visruthsk.github.io/voucher/reference/denounce.md)
- Check status (`vouched`, `denounced`, `unknown`) of users with
  [`check()`](https://visruthsk.github.io/voucher/reference/check.md)
- Add vouch GitHub Actions workflows with
  [`vouch_gha()`](https://visruthsk.github.io/voucher/reference/vouch_gha.md)

## Installation

``` r
# install.packages("pak")
pak::pak("VisruthSK/voucher")
```

## Small workflow

``` r
library(voucher)

# Initialize vouch files in the current project
use_vouch()

add("alice", write = TRUE)
denounce("spammer", reason = "spam", write = TRUE)

# Check status
check(c("alice","spammer"))
# Can also try to check git blame
check(c("alice","spammer"), blame = TRUE)

# Add GitHub Action workflows
vouch_gha(c("check-issue", "check-pr"))
```

## Licensing

- Code in this repository is licensed under GPLv3.
- Documentation in this repository is licensed under MIT. Some
  documentation is copied nearly verbatim from
  [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell
  Hashimoto.
