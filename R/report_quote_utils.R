# Utilities for extracting and verifying quotes in report paragraphs
#
# This module centralizes logic used across report Rmds to:
# - find quoted fragments in LLM paragraphs
# - verify their presence in supporting texts
# - decorate quotes with a verification icon (success/warning)

# Perform a single fixed substring replacement (first occurrence only)
sub_fixed <- function(pattern, replacement, text) {
  loc <- regexpr(pattern, text, fixed = TRUE)
  if (loc == -1) {
    return(text)
  }
  paste0(
    substr(text, 1, loc - 1),
    replacement,
    substr(text, loc + attr(loc, "match.length"), nchar(text))
  )
}

# Extract quotes from a single string, returning a two-column character matrix
# Columns: "Full Match" (including quotes), "Content" (inside quotes)
extract_quotes_matrix <- function(text) {
  # Validate input
  if (!is.character(text) || length(text) != 1) {
    warning(
      "Input 'text' must be a single character string. Returning empty matrix.",
      call. = FALSE
    )
    result_matrix <- matrix(character(0), ncol = 2, nrow = 0)
    colnames(result_matrix) <- c("Full Match", "Content")
    return(result_matrix)
  }

  # Support common quote pairs via alternation (straight, curly, guillemets, German style)
  # This avoids relying on \p escapes in R strings and handles asymmetric pairs like “ ... ”.
  quote_regex <- paste(
    '"([^"\r\n]+)"', # 1: straight double
    "'([^'\r\n]+)'", # 2: straight single
    '“([^”\r\n]+)”', # 3: curly double
    '‘([^’\r\n]+)’', # 4: curly single
    '«([^»\r\n]+)»', # 5: guillemets
    '„([^“\r\n]+)“', # 6: German low-high
    sep = "|"
  )

  all_groups_list <- stringr::str_match_all(text, quote_regex)
  all_groups_matrix <- all_groups_list[[1]]

  if (is.null(all_groups_matrix) || nrow(all_groups_matrix) == 0) {
    result_matrix <- matrix(character(0), ncol = 2, nrow = 0)
    colnames(result_matrix) <- c("Full Match", "Content")
    return(result_matrix)
  }

  # Column 1 is the overall match; content is in columns 2,4,6,8,10,12 (pick first non-NA per row)
  full <- all_groups_matrix[, 1]
  # Safely coalesce across possible capture positions
  content_cols <- all_groups_matrix[, -1, drop = FALSE]
  content <- apply(content_cols, 1, function(row) {
    first <- which(!is.na(row))[1]
    if (length(first)) row[first] else NA_character_
  })

  result_matrix <- cbind(`Full Match` = full, Content = content)
  return(result_matrix)
}

# Verify and decorate quotes in a paragraph.
# - paragraph_text: character(1) paragraph possibly containing quotes
# - supporting_texts: character vector or single string with texts to verify against
# - lang: 'nl' or 'en' (tooltips localized)
# - escape_html: when TRUE, escape remaining paragraph text before injecting HTML icons
# Returns processed paragraph text with quotes followed by <sup><icon></sup>.
verify_and_decorate_quotes <- function(
  paragraph_text,
  supporting_texts,
  lang = c("nl", "en"),
  escape_html = TRUE
) {
  lang <- match.arg(lang)

  # Normalize supporting texts to single string
  if (length(supporting_texts) > 1) {
    supporting_texts <- paste(supporting_texts, collapse = " ")
  }
  if (!is.character(supporting_texts)) {
    supporting_texts <- as.character(supporting_texts)
  }

  # Localized tooltips
  tt_ok <- if (lang == "nl") "Quote geverifieerd" else "Quote verified"
  tt_miss <- if (lang == "nl") {
    "Quote niet teruggevonden in teksten"
  } else {
    "Quote not found in texts"
  }

  # Build placeholder map and replace in a second pass (to avoid escaping issues)
  placeholder_map <- list()
  quote_matches <- extract_quotes_matrix(paragraph_text)

  if (nrow(quote_matches) > 0) {
    for (j in seq_len(nrow(quote_matches))) {
      full <- quote_matches[j, 1]
      q <- quote_matches[j, 2]
      # Remove trailing punctuation/symbols (e.g., .,;:!?), quotes stuck to parentheses, etc.
      q_clean <- stringr::str_remove(q, "[\\u2000-\\u206F\\p{P}\\p{S}]+$")
      placeholder <- paste0("___QUOTEPLACEHOLDER", j, "___")

      if (
        length(q_clean) > 0 &&
          !is.na(q_clean) &&
          stringr::str_detect(
            supporting_texts,
            stringr::fixed(q_clean, ignore_case = TRUE)
          )
      ) {
        icon_html <- as.character(bsicons::bs_icon(
          "check-circle-fill",
          title = tt_ok,
          class = "text-success"
        ))
      } else {
        icon_html <- as.character(bsicons::bs_icon(
          "exclamation-triangle-fill",
          title = tt_miss,
          class = "text-warning"
        ))
      }

      replacement <- paste0(full, '<sup>', icon_html, '</sup>')
      placeholder_map[[placeholder]] <- replacement

      paragraph_text <- sub_fixed(full, placeholder, paragraph_text)
    }
  }

  # Escape remainder if requested, then resolve placeholders
  if (isTRUE(escape_html)) {
    paragraph_text <- htmltools::htmlEscape(paragraph_text)
  }
  for (ph in names(placeholder_map)) {
    paragraph_text <- stringr::str_replace_all(
      paragraph_text,
      stringr::fixed(ph),
      placeholder_map[[ph]]
    )
  }

  paragraph_text
}
