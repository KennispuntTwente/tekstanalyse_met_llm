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

# Helper predicates for delimiter-aware quote scanning.
.quote_prev_char <- function(chars, i) {
  if (i <= 1L) "" else chars[i - 1L]
}

.quote_next_char <- function(chars, i) {
  if (i >= length(chars)) "" else chars[i + 1L]
}

.quote_prev_nonspace_char <- function(chars, i) {
  if (i <= 1L) {
    return("")
  }

  j <- i - 1L
  while (j >= 1L && .quote_is_space_char(chars[j])) {
    j <- j - 1L
  }

  if (j >= 1L) chars[j] else ""
}

.quote_next_nonspace_char <- function(chars, i) {
  if (i >= length(chars)) {
    return("")
  }

  j <- i + 1L
  while (j <= length(chars) && .quote_is_space_char(chars[j])) {
    j <- j + 1L
  }

  if (j <= length(chars)) chars[j] else ""
}

.quote_is_word_char <- function(ch) {
  is.character(ch) &&
    length(ch) == 1L &&
    nzchar(ch) &&
    isTRUE(stringr::str_detect(ch, "^[\\p{L}\\p{N}\\p{M}]$"))
}

.quote_is_space_char <- function(ch) {
  is.character(ch) &&
    length(ch) == 1L &&
    nzchar(ch) &&
    isTRUE(stringr::str_detect(ch, "^\\p{White_Space}$"))
}

.quote_is_embedded_apostrophe <- function(chars, i) {
  .quote_is_word_char(.quote_prev_char(chars, i)) &&
    .quote_is_word_char(.quote_next_char(chars, i))
}

.quote_can_open_symmetric <- function(chars, i, apostrophe_sensitive = FALSE) {
  prev <- .quote_prev_char(chars, i)
  next_sig <- .quote_next_nonspace_char(chars, i)

  if (!nzchar(next_sig)) {
    return(FALSE)
  }
  if (apostrophe_sensitive && .quote_is_embedded_apostrophe(chars, i)) {
    return(FALSE)
  }

  !nzchar(prev) || !.quote_is_word_char(prev)
}

.quote_can_close_symmetric <- function(chars, i, apostrophe_sensitive = FALSE) {
  prev_sig <- .quote_prev_nonspace_char(chars, i)
  next_ch <- .quote_next_char(chars, i)

  if (!nzchar(prev_sig)) {
    return(FALSE)
  }
  if (apostrophe_sensitive && .quote_is_embedded_apostrophe(chars, i)) {
    return(FALSE)
  }

  !nzchar(next_ch) || !.quote_is_word_char(next_ch)
}

.quote_can_open_asymmetric <- function(chars, i) {
  next_sig <- .quote_next_nonspace_char(chars, i)

  nzchar(next_sig)
}

.quote_can_close_asymmetric <- function(
  chars,
  i,
  apostrophe_sensitive = FALSE
) {
  prev_sig <- .quote_prev_nonspace_char(chars, i)

  if (!nzchar(prev_sig)) {
    return(FALSE)
  }
  if (apostrophe_sensitive && .quote_is_embedded_apostrophe(chars, i)) {
    return(FALSE)
  }

  TRUE
}

.quote_specs <- function() {
  list(
    list(
      open = "\"",
      close = "\"",
      symmetric = TRUE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "'",
      close = "'",
      symmetric = TRUE,
      apostrophe_sensitive = TRUE
    ),
    list(
      open = "\uFF02",
      close = "\uFF02",
      symmetric = TRUE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\uFF07",
      close = "\uFF07",
      symmetric = TRUE,
      apostrophe_sensitive = TRUE
    ),
    list(
      open = "\u201C",
      close = "\u201D",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\u201E",
      close = "\u201C",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\u00AB",
      close = "\u00BB",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\u2039",
      close = "\u203A",
      symmetric = FALSE,
      apostrophe_sensitive = TRUE
    ),
    list(
      open = "\u2018",
      close = "\u2019",
      symmetric = FALSE,
      apostrophe_sensitive = TRUE
    ),
    list(
      open = "\u201A",
      close = "\u2018",
      symmetric = FALSE,
      apostrophe_sensitive = TRUE
    ),
    list(
      open = "\u300C",
      close = "\u300D",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\u300E",
      close = "\u300F",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    ),
    list(
      open = "\uFF62",
      close = "\uFF63",
      symmetric = FALSE,
      apostrophe_sensitive = FALSE
    )
  )
}

.quote_find_open_spec <- function(chars, i) {
  ch <- chars[i]

  for (spec in .quote_specs()) {
    if (!identical(ch, spec$open)) {
      next
    }

    can_open <- if (isTRUE(spec$symmetric)) {
      .quote_can_open_symmetric(
        chars,
        i,
        apostrophe_sensitive = spec$apostrophe_sensitive
      )
    } else {
      .quote_can_open_asymmetric(chars, i)
    }

    if (isTRUE(can_open)) {
      return(spec)
    }
  }

  NULL
}

.quote_find_close_index <- function(chars, open_idx, spec) {
  if (open_idx >= length(chars)) {
    return(NA_integer_)
  }

  for (j in seq.int(open_idx + 1L, length(chars))) {
    ch <- chars[j]

    if (ch %in% c("\r", "\n")) {
      return(NA_integer_)
    }
    if (!identical(ch, spec$close)) {
      next
    }

    if (
      isTRUE(spec$symmetric) &&
        isTRUE(spec$apostrophe_sensitive) &&
        .quote_can_open_symmetric(
          chars,
          j,
          apostrophe_sensitive = spec$apostrophe_sensitive
        )
    ) {
      return(NA_integer_)
    }

    can_close <- if (isTRUE(spec$symmetric)) {
      .quote_can_close_symmetric(
        chars,
        j,
        apostrophe_sensitive = spec$apostrophe_sensitive
      )
    } else {
      .quote_can_close_asymmetric(
        chars,
        j,
        apostrophe_sensitive = spec$apostrophe_sensitive
      )
    }

    if (isTRUE(can_close)) {
      return(as.integer(j))
    }
  }

  NA_integer_
}

normalize_quote_verification_text <- function(text) {
  if (!is.character(text)) {
    text <- as.character(text)
  }

  text <- stringi::stri_trans_nfc(enc2utf8(text))

  stringr::str_replace_all(
    text,
    c(
      "\u2018" = "'",
      "\u2019" = "'",
      "\u201A" = "'",
      "\u2039" = "'",
      "\u203A" = "'",
      "\uFF07" = "'",
      "\u201C" = "\"",
      "\u201D" = "\"",
      "\u201E" = "\"",
      "\u00AB" = "\"",
      "\u00BB" = "\"",
      "\u300C" = "\"",
      "\u300D" = "\"",
      "\u300E" = "\"",
      "\u300F" = "\"",
      "\uFF02" = "\"",
      "\uFF62" = "\"",
      "\uFF63" = "\"",
      "\u2013" = "-",
      "\u2014" = "-"
    )
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

  if (is.na(text) || !nzchar(text)) {
    result_matrix <- matrix(character(0), ncol = 2, nrow = 0)
    colnames(result_matrix) <- c("Full Match", "Content")
    return(result_matrix)
  }

  text <- enc2utf8(text)
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  matches <- list()
  i <- 1L

  while (i <= length(chars)) {
    spec <- .quote_find_open_spec(chars, i)
    if (is.null(spec)) {
      i <- i + 1L
      next
    }

    close_idx <- .quote_find_close_index(chars, i, spec)
    if (is.na(close_idx) || close_idx <= i + 1L) {
      i <- i + 1L
      next
    }

    content <- paste0(chars[seq.int(i + 1L, close_idx - 1L)], collapse = "")
    if (!nzchar(stringr::str_trim(content))) {
      i <- close_idx + 1L
      next
    }

    matches[[length(matches) + 1L]] <- c(
      paste0(chars[seq.int(i, close_idx)], collapse = ""),
      content
    )
    i <- close_idx + 1L
  }

  if (!length(matches)) {
    result_matrix <- matrix(character(0), ncol = 2, nrow = 0)
    colnames(result_matrix) <- c("Full Match", "Content")
    return(result_matrix)
  }

  result_matrix <- do.call(rbind, matches)
  colnames(result_matrix) <- c("Full Match", "Content")
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

  if (is.character(paragraph_text)) {
    paragraph_text <- enc2utf8(paragraph_text)
  }

  if (!is.character(supporting_texts)) {
    supporting_texts <- as.character(supporting_texts)
  }

  supporting_texts <- enc2utf8(supporting_texts)
  supporting_texts <- supporting_texts[!is.na(supporting_texts)]
  supporting_texts <- supporting_texts[nzchar(supporting_texts)]
  supporting_texts_norm <- normalize_quote_verification_text(supporting_texts)

  quote_present_in_supporting_texts <- function(query, texts) {
    if (
      !is.character(query) ||
        length(query) != 1 ||
        is.na(query) ||
        !nzchar(query) ||
        !length(texts)
    ) {
      return(FALSE)
    }

    query_norm <- normalize_quote_verification_text(query)

    any(vapply(
      texts,
      function(txt) {
        isTRUE(stringr::str_detect(
          txt,
          stringr::fixed(query_norm, ignore_case = TRUE)
        ))
      },
      logical(1)
    ))
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
      q_clean <- stringr::str_trim(q_clean)
      placeholder <- paste0("___QUOTEPLACEHOLDER", j, "___")

      has_query <- is.character(q_clean) &&
        length(q_clean) == 1 &&
        !is.na(q_clean) &&
        nzchar(q_clean)
      is_present <- FALSE
      if (has_query) {
        is_present <- quote_present_in_supporting_texts(
          q_clean,
          supporting_texts_norm
        )
      }

      if (is_present) {
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
