# Defines a function to mark texts based on qualitative codes using an LLM.
# The function splits texts into semantic chunks, sends prompts to the LLM to identify
# relevant sections for each qualitative cod (e.g., 'it is raining' gets marked for code 'weather'),
# and optionally generates summary paragraphs highlighting the marked sections
# Function can take progress indicators and an interrupter for Shiny app integration

mark_texts <- function(
  texts = c(
    "hi my super boi, its raining!",
    "hello my mister man, the sky is blue",
    "hello to the world, its a hot day today",
    "for sure! i like cake"
  ),
  codes = c("greeting", "weather"),
  text_size_tokens = 128,
  overlap_size_tokens = 64,
  research_background = "",
  style_prompt = "",
  llm_provider = tidyprompt::llm_provider_openai(),
  progress_primary = NULL,
  progress_secondary = NULL,
  interrupter = NULL,
  lang = shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  ),
  write_paragraphs = TRUE,
  max_interactions = getOption("send_prompt_with_retries__max_interaction", 10),
  llm_stream_async = NULL,
  streaming_enabled = getOption("paragraph_streaming", TRUE)
) {
  stopifnot(
    is.character(texts),
    is.vector(texts),
    length(texts) > 0,
    is.character(codes),
    is.vector(codes),
    length(codes) > 0
  )
  total_steps <- 3
  if (write_paragraphs) {
    total_steps <- total_steps + 1
  }

  # Set initial progress
  try(progress_primary$set_with_total(
    1,
    total_steps,
    lang$t("Teksten splitsen...")
  ))

  # Load chunker
  chunker_name <- paste0("semchunker_", text_size_tokens)
  if (
    !exists(
      chunker_name
    )
  ) {
    semchunker <- semchunk_load_chunker(chunk_size = text_size_tokens)
    assign(chunker_name, semchunker, envir = .GlobalEnv)
  } else {
    semchunker <- get(chunker_name, envir = .GlobalEnv)
  }

  # Create dataframe with original texts
  df <- tibble::tibble(text = texts)
  # Split texts into semantic chunks, creating additional rows where needed
  df <- df |>
    dplyr::mutate(
      sub_text = purrr::map(text, function(x) {
        # Split text into semantic chunks
        semchunker(x, overlap = overlap_size_tokens)
      })
    ) |>
    tidyr::unnest(sub_text)

  # Verify that longest text does not exceed token limit
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) {
    n_tokens_context_window <- 2048
  }
  longest_prompt_tokens <- mark_text_prompt(
    text = df$sub_text[which.max(count_tokens(df$sub_text))],
    code = codes[which.max(count_tokens(codes))]
  ) |>
    tidyprompt::construct_prompt_text() |>
    count_tokens()
  if (longest_prompt_tokens > n_tokens_context_window) {
    stop(paste0(
      "The longest prompt (with longest text, longest code) exceeds the context window token limit of ",
      n_tokens_context_window,
      " tokens (the longest prompt has ",
      longest_prompt_tokens,
      " tokens)"
    ))
  }

  # For each code & sub_text, ask LLM to mark relevant sections in the text
  # Create own row for each text, sub_text, code, and marked_text
  total_combinations <- nrow(df) * length(codes)
  current_count <- 0
  try({
    progress_primary$set_with_total(
      2,
      total_steps,
      lang$t(
        "Teksten markeren..."
      )
    )
    progress_secondary$show()
    progress_secondary$set_with_total(
      current_count,
      total_combinations,
      "..."
    )
  })
  df_result <- df |>
    tidyr::crossing(code = codes) |>
    dplyr::mutate(
      marked_text = purrr::map2(sub_text, code, function(txt, cd) {
        current_count <<- current_count + 1
        try({
          progress_secondary$set_with_total(
            current_count,
            total_combinations,
            paste0(
              lang$t("Tekst markeren voor code '"),
              cd,
              "'..."
            )
          )
        })

        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        prompt <- mark_text_prompt(txt, cd, max_interactions = max_interactions)
        result <- send_prompt_with_retries(
          prompt,
          llm_provider,
          max_interactions = max_interactions
        )

        # Ensure result is always a character vector for unnesting
        if (length(result) == 0 || is.null(result)) {
          return(NA_character_)
        }

        return(result)
      })
    ) |>
    tidyr::unnest(marked_text) # This creates one row per marked section
  try({
    progress_secondary$hide()
    progress_primary$set_with_total(
      2.5,
      total_steps,
      lang$t(
        "Resultaten opschonen..."
      )
    )
  })

  # Clean up the result: remove NA marked_text, normalize, and check for substrings
  # (substring is when overlapped text parts are present in other marked sections;
  # i.e., same text, same code)
  df_result_clean <- df_result |>
    dplyr::filter(!is.na(marked_text)) |>
    dplyr::group_by(text, code) |>
    dplyr::mutate(
      # Normalize for comparison: lowercase, remove punctuation and spaces
      norm_text = stringr::str_remove_all(
        stringr::str_to_lower(marked_text),
        "[[:punct:]\\s]+"
      ),
      is_substring = purrr::map_lgl(norm_text, function(txt) {
        others <- setdiff(norm_text, txt)
        any(stringr::str_detect(others, stringr::fixed(txt)))
      })
    ) |>
    dplyr::filter(!is_substring) |>
    dplyr::select(-norm_text, -is_substring) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  # Write paragraphs if requested
  if (write_paragraphs) {
    try({
      progress_primary$set_with_total(
        3,
        total_steps,
        lang$t("Rapport schrijven...")
      )
    })

    # Collect the marked snippets for each text-code combo;
    #  this will be used to highlight the marked texts for each original text & code
    df_highlight <- df_result_clean |>
      dplyr::group_by(text, code) |>
      dplyr::summarise(
        marked_texts = list(unique(na.omit(marked_text))),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        highlighted_text = purrr::pmap_chr(
          list(text, marked_texts),
          function(orig, mlist) {
            highlighted <- orig
            for (m in mlist) {
              highlighted <- stringr::str_replace_all(
                highlighted,
                stringr::fixed(m),
                paste0("**", m, "**")
              )
            }
            highlighted
          }
        )
      )

    # Create list of code + all relevant original texts with highlighted marked texts
    text_list <- df_highlight |>
      dplyr::group_by(code) |>
      dplyr::summarise(
        paragraphs = list(highlighted_text),
        .groups = "drop"
      ) |>
      tibble::deframe()

    # Create paragraphs for each code
    try({
      progress_secondary$set_with_total(
        0,
        length(text_list),
        "..."
      )
      progress_secondary$show()
    })

    # Create streaming callback if streaming is enabled
    stream_callback <- NULL
    if (streaming_enabled && !is.null(llm_stream_async)) {
      try(llm_stream_async$show())
      stream_callback <- function(token, meta) {
        llm_stream_async$set(meta$partial_response %||% "")
        invisible(TRUE)
      }
    }

    i <- -1
    paragraphs <- purrr::imap(
      text_list,
      function(texts, code) {
        i <<- i + 1
        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        try({
          progress_secondary$set_with_total(
            i,
            length(text_list),
            paste0(
              lang$t("Schrijven over '"),
              code,
              "'..."
            )
          )
        })

        # Clear streaming panel before this paragraph
        if (streaming_enabled && !is.null(llm_stream_async)) {
          try(llm_stream_async$clear())
        }

        paragraph <- write_paragraph(
          texts = texts,
          topic = code,
          research_background = research_background,
          style_prompt = style_prompt,
          llm_provider = llm_provider,
          language = lang$get_translation_language(),
          focus_on_highlighted_text = TRUE,
          stream_callback = stream_callback
        )

        return(paragraph)
      }
    )
    try({
      progress_secondary$hide()
      progress_primary$set_with_total(
        3.5,
        total_steps,
        lang$t("Afronden...")
      )
    })

    # Set paragraphs as an attribute of the result
    attr(df_result_clean, "paragraphs") <- paragraphs
  }

  return(df_result_clean)
}

# Helper: prompt to mark text
mark_text_prompt <- function(
  text,
  code,
  research_background = "",
  max_interactions = getOption("send_prompt_with_retries__max_interaction", 10)
) {
  prompt <- "You are given a qualitative 'code' and a 'text'.\nYour task is to mark the relevant parts in the text that correspond to the code."
  if (!is.null(research_background) && research_background != "") {
    prompt <- prompt |>
      tidyprompt::add_text(
        glue::glue_safe(
          "The text was obtained during a research project. Read 'research background'.\n\n<research background>\n{research_background}\n</research background>"
        ),
        sep = "\n"
      )
  }

  prompt <- prompt |>
    tidyprompt::add_text(
      glue::glue_safe(
        "<code>\n{code}\n</code>\n\n<text>\n{text}\n</text>\n\nYou need to return literal parts of the text that are relevant to the code.",
        " If there are no relevant parts, return an empty array under key 'text_parts'."
      )
    ) |>
    tidyprompt::answer_as_json(
      schema = list(
        type = "object",
        properties = list(
          text_parts = list(
            type = "array",
            items = list(type = "string")
          )
        ),
        required = list("text_parts"),
        additionalProperties = FALSE
      ),
      type = "auto"
    )

  interaction_count <- 1

  prompt <- prompt |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        interaction_count <<- interaction_count + 1

        if (!is.list(x) || length(x) == 0 || !("text_parts" %in% names(x))) {
          return(tidyprompt::llm_feedback(paste0(
            "Invalid response format. Please return a JSON object with a 'text_parts' key containing an array of relevant text parts.",
            " Return an empty array if there are no relevant parts."
          )))
        }

        text_parts <- x$text_parts

        # Empty handling
        if (length(text_parts) == 0) {
          return(character(0))
        }
        if (length(text_parts) == 1 && identical(text_parts[1], "")) {
          return(character(0))
        }

        # Find matches
        res <- find_matches(
          haystack = text,
          needles = text_parts,
          rel = 0.12,
          abs = 2,
          step_div = 5L
        )

        missing_idx <- which(is.na(res$match))
        if (length(missing_idx)) {
          # If we've hit max interactions, drop unmatched parts and return what *did* match
          if (interaction_count >= max_interactions) {
            matched <- unname(res$match[!is.na(res$match)])
            return(matched)
          }

          # Otherwise, ask the model to correct by quoting literally
          return(tidyprompt::llm_feedback(paste0(
            "Error: below text parts are not present in the original text:\n\n  - ",
            paste(shQuote(res$needle[missing_idx]), collapse = "\n\n  - "),
            "\n\nYou must quote literally from the original text."
          )))
        }

        # Return the *literal* substrings from the original text
        unname(res$match)
      }
    )

  return(prompt)
}

#' Fuzzy literal matching of candidate snippets against a haystack
#'
#' Attempts to "snap" each needle to a *literal* substring in `haystack`
#' using a normalized Levenshtein distance with a leniency threshold:
#' max(abs, ceil(rel * nchar(needle))).
#'
#' @param haystack Character scalar. The full text to search within.
#' @param needles  Character vector. Candidate snippets to match.
#' @param rel      Numeric. Relative tolerance (default 0.12).
#' @param abs      Integer. Absolute minimum tolerance (default 2).
#' @param step_div Integer. Window step divisor; step = max(1, floor(nlen / step_div)).
#'
#' @return A tibble with columns:
#'   - needle: original input
#'   - match:  literal substring from `haystack` (or NA if no match within threshold)
#'   - distance: Levenshtein distance (on normalized strings) to the chosen window
#'   - start, end: 1-based indices of the match in `haystack` (NA if no match)
#'
#' @export
find_matches <- function(
  haystack,
  needles,
  rel = 0.12,
  abs = 2,
  step_div = 5L
) {
  stopifnot(is.character(haystack), length(haystack) == 1L)
  if (!length(needles)) {
    return(tibble::tibble(
      needle = character(0),
      match = character(0),
      distance = integer(0),
      start = integer(0),
      end = integer(0)
    ))
  }

  rows <- lapply(
    needles,
    function(nd) {
      best_literal_substring(
        needle = nd,
        haystack = haystack,
        rel = rel,
        abs = abs,
        step_div = step_div
      )
    }
  )

  tibble::tibble(
    needle = needles,
    match = vapply(rows, `[[`, "", "match"),
    distance = vapply(
      rows,
      function(r) {
        ifelse(is.na(r$distance), NA_integer_, as.integer(r$distance))
      },
      integer(1)
    ),
    start = vapply(
      rows,
      function(r) ifelse(is.na(r$start), NA_integer_, as.integer(r$start)),
      integer(1)
    ),
    end = vapply(
      rows,
      function(r) ifelse(is.na(r$end), NA_integer_, as.integer(r$end)),
      integer(1)
    )
  )
}
# --- helper: normalize + index map back to original (unchanged if you already added it) ---
normalize_with_map <- function(s) {
  if (is.null(s) || is.na(s)) {
    return(list(norm = "", start_idx = integer(0), end_idx = integer(0)))
  }
  chars <- strsplit(s, "", fixed = FALSE, perl = FALSE)[[1]]
  n <- character(0)
  start_idx <- integer(0)
  end_idx <- integer(0)
  i <- 1L
  L <- length(chars)
  add <- function(ch, st, en) {
    n <<- c(n, ch)
    start_idx <<- c(start_idx, st)
    end_idx <<- c(end_idx, en)
  }
  is_space <- function(ch) grepl("^[[:space:]]$", ch)

  while (i <= L) {
    ch <- chars[i]
    if (is_space(ch)) {
      # collapse runs of whitespace to a single space
      j <- i
      while (j <= L && is_space(chars[j])) {
        j <- j + 1L
      }
      add(" ", i, j - 1L)
      i <- j
      next
    }
    if (ch %in% c("\u2018", "\u2019")) {
      ch <- "'"
    } else if (ch %in% c("\u201C", "\u201D")) {
      ch <- "\""
    } else if (ch %in% c("\u2013", "\u2014")) {
      ch <- "-"
    }
    add(tolower(ch), i, i)
    i <- i + 1L
  }
  if (length(n) && n[1] == " ") {
    n <- n[-1]
    start_idx <- start_idx[-1]
    end_idx <- end_idx[-1]
  }
  if (length(n) && tail(n, 1) == " ") {
    n <- n[-length(n)]
    start_idx <- start_idx[-length(start_idx)]
    end_idx <- end_idx[-length(end_idx)]
  }
  list(
    norm = paste0(n, collapse = ""),
    start_idx = start_idx,
    end_idx = end_idx
  )
}

normalize_for_dist <- function(s) normalize_with_map(s)$norm

# --- patched matcher ---
best_literal_substring <- function(
  needle,
  haystack,
  rel = 0.12,
  abs = 2,
  step_div = 5L
) {
  # 0) guard: NA/empty needles and empty haystacks
  if (is.na(needle) || is.null(needle)) {
    return(list(
      match = NA_character_,
      distance = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    ))
  }
  n <- normalize_for_dist(needle)
  if (nchar(n) == 0L || is.na(haystack) || nchar(haystack) == 0L) {
    return(list(
      match = NA_character_,
      distance = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    ))
  }

  # 1) exact literal (no normalization)
  #    safe now because we've ruled out empty/NA pattern
  exact_loc <- regexpr(needle, haystack, fixed = TRUE)
  if (exact_loc[1] != -1L) {
    st <- as.integer(exact_loc[1])
    en <- st + attr(exact_loc, "match.length") - 1L
    return(list(
      match = substr(haystack, st, en),
      distance = 0L,
      start = st,
      end = en
    ))
  }

  # 2) exact-on-normalized
  nm <- normalize_with_map(haystack)
  Hn <- nm$norm
  nlen <- nchar(n)
  Ln <- nchar(Hn)
  if (Ln == 0L) {
    return(list(
      match = NA_character_,
      distance = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    ))
  }
  md <- fuzzy_threshold(nlen, rel = rel, abs = abs)

  pos <- regexpr(n, Hn, fixed = TRUE)
  if (pos[1] != -1L) {
    stn <- as.integer(pos[1])
    enn <- stn + nlen - 1L
    st <- nm$start_idx[stn]
    en <- nm$end_idx[enn]
    return(list(
      match = substr(haystack, st, en),
      distance = 0L,
      start = st,
      end = en
    ))
  }

  # 3) fuzzy on normalized; coarse scan then refine to guarantee correctness
  minw <- max(1L, nlen - md)
  maxw <- min(Ln, nlen + md)
  if (Ln < minw) {
    return(list(
      match = NA_character_,
      distance = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    ))
  }

  scan_step <- function(step) {
    cands <- list()
    for (w in seq.int(minw, maxw)) {
      last_start <- Ln - w + 1L
      if (last_start <= 0L) {
        next
      }
      for (i in seq.int(1L, last_start)) {
        subn <- substr(Hn, i, i + w - 1L)
        d <- stringdist::stringdist(n, subn, method = "lv")
        if (d <= md) {
          cands[[length(cands) + 1L]] <- list(
            d = as.integer(d),
            w = as.integer(w),
            i = as.integer(i)
          )
        }
      }
    }
    cands
  }

  # coarse pass using requested granularity
  step_coarse <- max(1L, floor(nlen / step_div))
  cands <- scan_step(step_coarse)

  if (!length(cands)) {
    return(list(
      match = NA_character_,
      distance = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    ))
  }

  # refine pass (step = 1) only if coarse found nothing
  if (!length(cands)) {
    cands <- scan_step(1L)
    if (!length(cands)) {
      return(list(
        match = NA_character_,
        distance = NA_integer_,
        start = NA_integer_,
        end = NA_integer_
      ))
    }
  }

  # Prefer: (1) window length closest to needle, (2) smaller distance, (3) earlier start
  wdiffs <- vapply(cands, function(c) as.integer(abs(c$w - nlen)), integer(1))
  ds <- vapply(cands, function(c) c$d, integer(1))
  is <- vapply(cands, function(c) c$i, integer(1))
  pick <- order(wdiffs, ds, is)[1]
  c <- cands[[pick]]

  stn <- c$i
  enn <- c$i + c$w - 1L
  st <- nm$start_idx[stn]
  en <- nm$end_idx[enn]
  list(match = substr(haystack, st, en), distance = c$d, start = st, end = en)
}

fuzzy_threshold <- function(needle_len, rel = 0.12, abs = 2) {
  max(abs, ceiling(needle_len * rel))
}
