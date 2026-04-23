# Filename sanitization utilities

#' Sanitize a string for safe use as a filename
#'
#' Strips or replaces characters that are unsafe on Windows, macOS, and Linux.
#' Collapses whitespace and punctuation runs to underscores, trims edges, and
#' truncates to \code{max_length} characters.
#'
#' @param name Character scalar to sanitize.
#' @param max_length Maximum length of the returned string.
#' @return A safe filename string, or \code{""} if the input is empty or
#'   reduces to nothing after sanitization.
sanitize_filename <- function(name, max_length = 80L) {
  if (is.null(name) || !nzchar(trimws(name))) {
    return("")
  }
  name <- as.character(name)[1]
  # Remove characters forbidden on Windows / macOS / Linux

  name <- gsub('[/\\\\:*?"<>|]', "", name)
  # Collapse whitespace and underscores to a single underscore

  name <- gsub("[[:space:]_]+", "_", name)
  # Remove leading/trailing underscores and dots (avoid hidden/empty names)
  name <- gsub("^[_.]+|[_.]+$", "", name)
  # Truncate
  if (nchar(name) > max_length) {
    name <- substr(name, 1, max_length)
    # Trim any trailing underscore left by the cut
    name <- sub("_+$", "", name)
  }
  name
}
