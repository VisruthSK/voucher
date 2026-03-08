# Initialize vouch in the current project

Creates a starter `VOUCHED.td` file at `.github/VOUCHED.td` and ensures
`.github` is ignored in package builds by adding `^\\.github$` to
`.Rbuildignore` when needed.

## Usage

``` r
use_vouch()
```

## Value

Invisibly returns `NULL`. Called for the side effects of writing a
starter `VOUCHED.td` file and updating `.Rbuildignore`.

## Details

If a vouch file already exists (`VOUCHED.td` or `.github/VOUCHED.td`),
this function exits without making changes.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- file.path(tempdir(), "voucher-use-vouch-example")
dir.create(project, recursive = TRUE)
old <- setwd(project)
on.exit(setwd(old), add = TRUE)
use_vouch()
} # }
```
