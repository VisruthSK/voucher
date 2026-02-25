# voucher

<!-- badges: start -->
[![R-CMD-check](https://github.com/VisruthSK/voucher/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/VisruthSK/voucher/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/VisruthSK/voucher/graph/badge.svg)](https://app.codecov.io/gh/VisruthSK/voucher)
[![pkgdown](https://github.com/VisruthSK/voucher/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/VisruthSK/voucher/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

`{voucher}` is a R interface to a [Trustdown database](https://github.com/mitchellh/vouch/?tab=readme-ov-file#vouched-file-format) as used by [`vouch`](https://github.com/mitchellh/vouch/), a "community trust management system based on explicit vouches to participate." This package directly interacts with the vouched file format and doesn't rely on `vouch` or Nushell.

From the `vouch` README:

> People must be **vouched for** before interacting with certain parts of a project (the exact parts are configurable to the project to enforce). People can also be explicitly **denounced** to block them from interacting with the project.

Read more about why you may want to use vouching in the [`vouch` README](https://github.com/mitchellh/vouch/?tab=readme-ov-file#why).

## What `voucher` provides

Subset of `vouch`'s behavior:

- Initialize a project trust file (`VOUCHED.td`) with `use_vouch()`
- Add vouched contributors with `add()`
- Denounce contributors with `denounce()`
- Check status (`vouched`, `denounced`, `unknown`) of users with `check()`
- Add vouch GitHub Actions workflows with `vouch_gha()`

## Installation

```r
# install.packages("pak")
pak::pak("VisruthSK/voucher")
```

## Small workflow

```r
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
- Documentation in this repository is licensed under MIT. Some documentation is copied nearly verbatim from [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell Hashimoto.
