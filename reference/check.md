# Check a user's vouch status.

This checks if a user is vouched, denounced, or unknown.

## Usage

``` r
check(username, default_platform = "", vouched_file = "")
```

## Arguments

- username:

  Username to check (supports `platform:user` format).

- default_platform:

  Assumed platform for entries without explicit platform.

- vouched_file:

  Path to vouched contributors file (default: `VOUCHED.td` or
  `.github/VOUCHED.td`).

## Value

One of `"vouched"`, `"denounced"`, or `"unknown"`.

## Attribution

Documentation for this function is copied nearly verbatim from
[`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell
Hashimoto.

## Examples

``` r
if (FALSE) { # \dontrun{
check("someuser")
check("github:someuser")
} # }
```
