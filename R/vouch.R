#' Add a user to the vouched contributors list.
#'
#' This adds the user to the vouched list, removing any existing entry
#' (vouched or denounced) for that user first.
#'
#' @param username Username to vouch for (supports `platform:user` format).
#' @param write Write the file in-place (default: output to stdout).
#' @param default_platform Assumed platform for entries without explicit platform.
#' @param vouched_file Path to vouched contributors file (default:
#'   `VOUCHED.td` or `.github/VOUCHED.td`).
#'
#' @return Invisibly returns the updated trustdown text.
#'
#' @examples
#' \dontrun{
#' # Preview new file contents (default)
#' add("someuser")
#'
#' # Write the file in-place
#' add("someuser", write = TRUE)
#'
#' # Add with platform prefix
#' add("github:someuser", write = TRUE)
#' }
#'
#' @section Attribution:
#' Documentation for this function is copied nearly verbatim from [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell Hashimoto.
add <- function(
  username,
  write = FALSE,
  default_platform = "",
  vouched_file = ""
) {
  vouch_update_file(
    username = username,
    write = write,
    default_platform = default_platform,
    vouched_file = vouched_file,
    type = "vouch"
  )
}

#' Denounce a user by adding them to the VOUCHED file with a minus prefix.
#'
#' This removes any existing entry for the user and adds them as denounced.
#' An optional reason can be provided which will be added after the username.
#'
#' @param username Username to denounce (supports `platform:user` format).
#' @param write Write the file in-place (default: output to stdout).
#' @param reason Optional reason for denouncement.
#' @param default_platform Assumed platform for entries without explicit platform.
#' @param vouched_file Path to vouched contributors file (default:
#'   `VOUCHED.td` or `.github/VOUCHED.td`).
#'
#' @return Invisibly returns the updated trustdown text.
#'
#' @examples
#' \dontrun{
#' # Preview new file contents (default)
#' denounce("badactor")
#'
#' # Denounce with a reason
#' denounce("badactor", reason = "Submitted AI slop")
#'
#' # Write the file in-place
#' denounce("badactor", write = TRUE)
#'
#' # Denounce with platform prefix
#' denounce("github:badactor", write = TRUE)
#' }
#'
#' @section Attribution:
#' Documentation for this function is copied nearly verbatim from [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell Hashimoto.
denounce <- function(
  username,
  write = FALSE,
  reason = "",
  default_platform = "",
  vouched_file = ""
) {
  vouch_update_file(
    username = username,
    write = write,
    default_platform = default_platform,
    vouched_file = vouched_file,
    type = "denounce",
    details = reason
  )
}

#' Check a user's vouch status.
#'
#' This checks if a user is vouched, denounced, or unknown.
#'
#' @param username Username to check (supports `platform:user` format).
#' @param default_platform Assumed platform for entries without explicit platform.
#' @param vouched_file Path to vouched contributors file (default:
#'   `VOUCHED.td` or `.github/VOUCHED.td`).
#'
#' @return One of `"vouched"`, `"denounced"`, or `"unknown"`.
#'
#' @examples
#' \dontrun{
#' check("someuser")
#' check("github:someuser")
#' }
#'
#' @section Attribution:
#' Documentation for this function is copied nearly verbatim from [`vouch`](https://github.com/mitchellh/vouch) and is owned by Mitchell Hashimoto.
check <- function(
  username,
  default_platform = "",
  vouched_file = ""
) {
  file <- vouch_resolve_existing_file(vouched_file)
  target <- vouch_split_handle(username, default_platform = default_platform)
  status <- "unknown"

  for (line in readLines(file, warn = FALSE)) {
    entry <- vouch_parse_line(line, default_platform = default_platform)
    if (is.null(entry)) {
      next
    }

    platform_matches <- !nzchar(target$platform) ||
      !nzchar(entry$platform) ||
      identical(entry$platform, target$platform)

    if (identical(entry$username, target$username) && platform_matches) {
      status <- if (identical(entry$type, "denounce")) {
        "denounced"
      } else {
        "vouched"
      }
      break
    }
  }

  cli::cli_alert_info("{username} is {status}")

  invisible(status)
}

# Helpers ---------------------------------------------------------------------

vouch_update_file <- function(
  username,
  write = FALSE,
  default_platform = "",
  vouched_file = "",
  type = c("vouch", "denounce"),
  details = ""
) {
  type <- match.arg(type)
  write_message <- if (identical(type, "denounce")) {
    "Denounced ({username})"
  } else {
    "Added ({username}) to vouched contributors"
  }
  file <- vouch_resolve_existing_file(vouched_file)
  target <- vouch_split_handle(username, default_platform = default_platform)
  handle <- if (nzchar(target$raw_platform)) {
    paste0(target$raw_platform, ":", target$username)
  } else {
    target$username
  }
  new_entry <- paste0(
    if (identical(type, "denounce")) "-" else "",
    handle,
    if (nzchar(details)) paste0(" ", details) else ""
  )
  existing <- readLines(file, warn = FALSE)
  parsed <- lapply(
    existing,
    vouch_parse_line,
    default_platform = default_platform
  )
  is_contributor <- !vapply(parsed, is.null, logical(1))

  contributor_lines <- existing[is_contributor]
  contributor_entries <- parsed[is_contributor]
  keep <- vapply(
    contributor_entries,
    function(entry) {
      platform_matches <- !nzchar(target$platform) ||
        !nzchar(entry$platform) ||
        identical(entry$platform, target$platform)

      !(identical(entry$username, target$username) && platform_matches)
    },
    logical(1)
  )

  updated_contributor_lines <- c(contributor_lines[keep], new_entry)
  updated_contributor_entries <- c(
    contributor_entries[keep],
    list(vouch_parse_line(new_entry, default_platform = default_platform))
  )
  contributor_order <- order(
    vapply(updated_contributor_entries, `[[`, character(1), "username"),
    method = "radix"
  )

  lines <- c(
    existing[!is_contributor],
    updated_contributor_lines[contributor_order]
  )
  text <- paste0(paste(lines, collapse = "\n"), "\n")

  if (isTRUE(write)) {
    write_to_path(lines, file)
    cli::cli_alert_success(write_message)
  } else {
    cli::cat_line(lines)
  }

  invisible(text)
}

vouch_split_handle <- function(handle, default_platform = "") {
  parts <- strsplit(
    tolower(trimws(as.character(handle))),
    ":",
    fixed = TRUE
  )[[1]]
  if (length(parts) > 1L) {
    raw_platform <- parts[[1]]
    username <- paste(parts[-1], collapse = ":")
  } else {
    raw_platform <- ""
    username <- parts[[1]]
  }

  list(
    username = username,
    raw_platform = raw_platform,
    platform = if (nzchar(raw_platform)) {
      raw_platform
    } else {
      tolower(default_platform)
    }
  )
}

vouch_parse_line <- function(line, default_platform = "") {
  trimmed <- trimws(line)
  if (!nzchar(trimmed) || startsWith(trimmed, "#")) {
    return(NULL)
  }

  type <- if (startsWith(trimmed, "-")) "denounce" else "vouch"
  handle <- strsplit(sub("^-", "", trimmed), " ", fixed = TRUE)[[1]][[1]]
  parsed <- vouch_split_handle(handle, default_platform = default_platform)
  list(type = type, username = parsed$username, platform = parsed$platform)
}
