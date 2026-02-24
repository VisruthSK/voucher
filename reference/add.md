# Add a user to the vouched contributors list.

This adds the user to the vouched list, removing any existing entry
(vouched or denounced) for that user first.

## Usage

``` r
add(username, write = FALSE, default_platform = "", vouched_file = "")
```

## Arguments

- username:

  Username to vouch for (supports `platform:user` format).

- write:

  Write the file in-place (default: output to stdout).

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
add("someuser")

# Write the file in-place
add("someuser", write = TRUE)

# Add with platform prefix
add("github:someuser", write = TRUE)
} # }
```
