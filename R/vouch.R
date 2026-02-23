use_vouch <- function(path = ".") {
  if (
    file.exists("VOUCHED.td") ||
      file.exists(".github/VOUCHED.td")
  ) {
    message("Existing vouch database found. Exiting without any changes.")
    return()
  }
  file.create()
}
