#' Add a user to the vouched contributors list.
#'
#' This adds the user to the vouched list, removing any existing entry
#' (vouched or denounced) for that user first.
#'
#' @param username Username to vouch for (supports `platform:user` format).
#' @param default_platform Assumed platform for entries without explicit platform.
#' @param vouched_file Path to vouched contributors file (default:
#'   `VOUCHED.td` or `.github/VOUCHED.td`).
#' @param write Write the file in-place (default: output to stdout).
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
#' Documentation for this function is copied nearly verbatim from the upstream
#' vouch Nushell command help and is owned by Mitchell Hashimoto.
add <- function(
  username,
  default_platform = "",
  vouched_file = "",
  write = FALSE
) {
  file <- vouch_resolve_existing_file(vouched_file)
  lines <- readLines(file, warn = FALSE)
  updated <- vouch_add_or_denounce(
    lines = lines,
    username = username,
    default_platform = default_platform,
    type = "vouch"
  )
  text <- if (length(updated) == 0L) {
    ""
  } else {
    paste0(paste(updated, collapse = "\n"), "\n")
  }

  if (isTRUE(write)) {
    write_to_path(updated, file)
    cat(sprintf("Added (%s) to vouched contributors\n", username))
  } else {
    cat(text, sep = "")
  }

  invisible(text)
}

#' Denounce a user by adding them to the VOUCHED file with a minus prefix.
#'
#' This removes any existing entry for the user and adds them as denounced.
#' An optional reason can be provided which will be added after the username.
#'
#' @param username Username to denounce (supports `platform:user` format).
#' @param default_platform Assumed platform for entries without explicit platform.
#' @param reason Optional reason for denouncement.
#' @param vouched_file Path to vouched contributors file (default:
#'   `VOUCHED.td` or `.github/VOUCHED.td`).
#' @param write Write the file in-place (default: output to stdout).
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
#' Documentation for this function is copied nearly verbatim from the upstream
#' vouch Nushell command help and is owned by Mitchell Hashimoto.
denounce <- function(
  username,
  default_platform = "",
  reason = "",
  vouched_file = "",
  write = FALSE
) {
  file <- vouch_resolve_existing_file(vouched_file)
  lines <- readLines(file, warn = FALSE)
  updated <- vouch_add_or_denounce(
    lines = lines,
    username = username,
    default_platform = default_platform,
    type = "denounce",
    details = reason
  )
  text <- if (length(updated) == 0L) {
    ""
  } else {
    paste0(paste(updated, collapse = "\n"), "\n")
  }

  if (isTRUE(write)) {
    write_to_path(updated, file)
    cat(sprintf("Denounced (%s)\n", username))
  } else {
    cat(text, sep = "")
  }

  invisible(text)
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
#' Documentation for this function is copied nearly verbatim from the upstream
#' vouch Nushell command help and is owned by Mitchell Hashimoto.
check <- function(
  username,
  default_platform = "",
  vouched_file = ""
) {
  file <- vouch_resolve_existing_file(vouched_file)
  lines <- readLines(file, warn = FALSE)
  status <- vouch_status(lines, username, default_platform = default_platform)

  if (identical(status, "vouched")) {
    cat(sprintf("%s is vouched\n", username))
  } else if (identical(status, "denounced")) {
    cat(sprintf("%s is denounced\n", username))
  } else {
    cat(sprintf("%s is unknown\n", username))
  }

  status
}

# Helpers ---------------------------------------------------------------------

vouch_resolve_existing_file <- function(vouched_file = "") {
  if (nzchar(vouched_file)) {
    return(vouched_file)
  }

  candidates <- c("VOUCHED.td", ".github/VOUCHED.td")
  found <- candidates[fs::file_exists(candidates)]
  if (length(found) == 0L) {
    cli::cli_abort("no VOUCHED file found")
  }

  found[[1]]
}

vouch_parse_handle <- function(handle) {
  parts <- strsplit(tolower(handle), ":", fixed = TRUE)[[1]]
  if (length(parts) > 1L) {
    list(platform = parts[[1]], username = paste(parts[-1], collapse = ":"))
  } else {
    list(platform = NULL, username = parts[[1]])
  }
}

vouch_parse_entry <- function(line) {
  trimmed <- trimws(line)
  if (!nzchar(trimmed) || startsWith(trimmed, "#")) {
    return(NULL)
  }

  type <- if (startsWith(trimmed, "-")) "denounce" else "vouch"
  rest <- if (identical(type, "denounce")) substring(trimmed, 2L) else trimmed
  first_space <- regexpr(" ", rest, fixed = TRUE)[1]

  if (first_space > 0L) {
    handle <- substr(rest, 1L, first_space - 1L)
    details <- substr(rest, first_space + 1L, nchar(rest))
    if (!nzchar(details)) {
      details <- NULL
    }
  } else {
    handle <- rest
    details <- NULL
  }

  parsed <- vouch_parse_handle(handle)
  list(
    type = type,
    platform = parsed$platform,
    username = parsed$username,
    details = details
  )
}

vouch_status <- function(lines, username, default_platform = "") {
  target <- vouch_parse_handle(username)
  for (line in lines) {
    entry <- vouch_parse_entry(line)
    if (is.null(entry)) {
      next
    }
    if (vouch_entry_matches(entry, target, default_platform)) {
      if (identical(entry$type, "denounce")) {
        return("denounced")
      }
      return("vouched")
    }
  }
  "unknown"
}

vouch_add_or_denounce <- function(
  lines,
  username,
  default_platform = "",
  type = c("vouch", "denounce"),
  details = ""
) {
  type <- match.arg(type)
  target <- vouch_parse_handle(username)
  cleaned <- lines[vapply(
    lines,
    function(line) {
      entry <- vouch_parse_entry(line)
      if (is.null(entry)) {
        return(TRUE)
      }
      !vouch_entry_matches(entry, target, default_platform)
    },
    logical(1)
  )]

  new_entry <- list(
    type = type,
    platform = target$platform,
    username = target$username,
    details = if (nzchar(details)) details else NULL
  )
  parsed <- lapply(cleaned, vouch_parse_entry)
  is_entry <- !vapply(parsed, is.null, logical(1))

  header <- cleaned[!is_entry]
  entries <- c(parsed[is_entry], list(new_entry))

  ord <- order(tolower(vapply(entries, `[[`, character(1), "username")))
  entry_lines <- vapply(
    entries[ord],
    function(entry) {
      prefix <- if (identical(entry$type, "denounce")) "-" else ""
      handle <- if (!is.null(entry$platform) && nzchar(entry$platform)) {
        paste0(entry$platform, ":", entry$username)
      } else {
        entry$username
      }
      suffix <- if (!is.null(entry$details) && nzchar(entry$details)) {
        paste0(" ", entry$details)
      } else {
        ""
      }
      paste0(prefix, handle, suffix)
    },
    character(1)
  )

  c(header, entry_lines)
}

vouch_entry_matches <- function(entry, target, default_platform = "") {
  entry_platform <- if (!is.null(entry$platform) && nzchar(entry$platform)) {
    tolower(entry$platform)
  } else if (nzchar(default_platform)) {
    tolower(default_platform)
  } else {
    NULL
  }

  target_platform <- if (!is.null(target$platform) && nzchar(target$platform)) {
    tolower(target$platform)
  } else if (nzchar(default_platform)) {
    tolower(default_platform)
  } else {
    NULL
  }

  platform_matches <- is.null(target_platform) ||
    is.null(entry_platform) ||
    identical(entry_platform, target_platform)

  identical(entry$username, target$username) && platform_matches
}
