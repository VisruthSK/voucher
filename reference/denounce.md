# Denounce a user by adding them to the VOUCHED file with a minus prefix.

This removes any existing entry for the user and adds them as denounced.
An optional reason can be provided which will be added after the
username.

## Usage

``` r
denounce(
  username,
  write = FALSE,
  reason = "",
  default_platform = "",
  vouched_file = ""
)
```

## Arguments

- username:

  Username to denounce (supports `platform:user` format).

- write:

  Write the file in-place (default: output to stdout).

- reason:

  Optional reason for denouncement.

- default_platform:

  Assumed platform for entries without explicit platform.

- vouched_file:

  Path to vouched contributors file (default: `VOUCHED.td` or
  `.github/VOUCHED.td`).

## Value

Invisibly returns the updated trustdown text.

## Attribution

Documentation for this function is copied nearly verbatim from
[`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell
Hashimoto.

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview new file contents (default)
denounce("badactor")

# Denounce with a reason
denounce("badactor", reason = "Submitted AI slop")

# Write the file in-place
denounce("badactor", write = TRUE)

# Denounce with platform prefix
denounce("github:badactor", write = TRUE)
} # }
```
