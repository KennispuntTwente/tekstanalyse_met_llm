# Prompt sanitization: escape XML-like closing tags in user-supplied content
# before inserting into tagged prompt sections. Prevents delimiter-breaking
# injection where e.g. "</text>" in user text prematurely closes a <text> block.

#' Escape XML-like closing tags in user content for prompt safety
#'
#' Replaces closing tags that match the supplied tag names (e.g. `</text>`)
#' with a backslash-escaped variant (`<\/text>`) so they cannot break
#' the prompt's tagged section boundaries.
#'
#' Handles both plain tags (`</text>`) and numbered variants (`</text 1>`).
#'
#' @param content Character string to sanitize.
#' @param tag_names Character vector of tag names to escape (e.g.
#'   `c("text", "code", "research_background")`).
#' @return The sanitized string with closing tags escaped.
escape_prompt_delimiters <- function(content, tag_names) {
  stopifnot(is.character(content), length(content) == 1)
  stopifnot(is.character(tag_names), length(tag_names) > 0)

  for (tag in tag_names) {
    # Escape plain closing tag:  </tag>  ->  <\/tag>
    # Escape numbered variants:  </tag 1>  ->  <\/tag 1>
    pattern <- paste0("</", tag, "(\\s*\\d*)\\s*>")
    replacement <- paste0("<\\\\/", tag, "\\1>")
    content <- gsub(pattern, replacement, content, perl = TRUE)
  }

  content
}
