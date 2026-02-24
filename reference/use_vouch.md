# Initialize vouch in the current project

Creates a starter `VOUCHED.td` file at `.github/VOUCHED.td` and ensures
`.github` is ignored in package builds by adding `^\\.github$` to
`.Rbuildignore` when needed.

## Usage

``` r
use_vouch()
```

## Value

Invisibly returns `NULL`.

## Details

If a vouch file already exists (`VOUCHED.td` or `.github/VOUCHED.td`),
this function exits without making changes.

## Examples

``` r
if (FALSE) { # \dontrun{
use_vouch()
} # }
```
