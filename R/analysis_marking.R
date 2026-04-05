# Defines a function to mark texts based on qualitative codes using an LLM.
# The function splits texts into semantic chunks, sends prompts to the LLM to identify
# relevant sections for each qualitative cod (e.g., 'it is raining' gets marked for code 'weather'),
# and optionally generates summary paragraphs highlighting the marked sections
# Function can take progress indicators and an interrupter for Shiny app integration

mark_texts <- function(
  texts,
  analysis_unit_ids,
  codes,
  text_size_tokens = 128,
  overlap_size_tokens = 64,
  research_background = "",
  style_prompt = "",
  llm_provider,
  progress_primary = NULL,
  progress_secondary = NULL,
  interrupter = NULL,
  lang = NULL,
  write_paragraphs = TRUE,
  max_interactions = getOption(
    "send_prompt_with_retries__max_interactions",
    10
  ),
  llm_stream_async = NULL,
  streaming_enabled = getOption("paragraph_streaming", TRUE)
) {
  stopifnot(
    is.character(texts),
    is.vector(texts),
    length(texts) > 0,
    is.numeric(analysis_unit_ids),
    length(analysis_unit_ids) == length(texts),
    is.character(codes),
    is.vector(codes),
    length(codes) > 0
  )

  stage_options <- options(kwallm__prompt_execution_stage = "marking")
  on.exit(options(stage_options), add = TRUE)

  total_steps <- 3
  if (write_paragraphs) {
    total_steps <- total_steps + 1
  }

  translate <- function(text) {
    if (is.null(lang)) {
      return(text)
    }

    lang$t(text)
  }

  paragraph_language <- if (is.null(lang)) {
    "en"
  } else {
    lang$get_translation_language()
  }

  progress_set_with_total <- function(progress, i, total, txt) {
    if (is.null(progress)) {
      return(invisible(NULL))
    }

    progress$set_with_total(i, total, txt)
  }

  progress_show <- function(progress) {
    if (is.null(progress)) {
      return(invisible(NULL))
    }

    progress$show()
  }

  progress_hide <- function(progress) {
    if (is.null(progress)) {
      return(invisible(NULL))
    }

    progress$hide()
  }

  # Set initial progress
  log_info("Marking Step 1: Splitting texts...", component = "analysis")
  try(
    progress_set_with_total(
      progress_primary,
      1,
      total_steps,
      translate("Teksten splitsen...")
    ),
    silent = TRUE
  )

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

  # `texts` here are already unique analysis-unit texts, not source/document rows.
  # `analysis_unit_id` identifies that LLM input, and `chunk_id` identifies the
  # chunk rows created from it.
  df <- tibble::tibble(
    analysis_unit_id = as.integer(analysis_unit_ids),
    analysis_unit_text = texts
  )

  # Split analysis units into semantic chunks, keeping chunk lineage explicit.
  df <- df |>
    dplyr::mutate(
      chunk_text = purrr::map(analysis_unit_text, function(x) {
        semchunker(x, overlap = overlap_size_tokens)
      }),
      chunk_index = purrr::map(chunk_text, seq_along)
    ) |>
    tidyr::unnest(c(chunk_text, chunk_index)) |>
    dplyr::mutate(chunk_id = dplyr::row_number())

  # Verify that longest text does not exceed token limit
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) {
    n_tokens_context_window <- 2048
  }
  longest_prompt_tokens <- mark_text_prompt(
    text = df$chunk_text[which.max(count_tokens(df$chunk_text))],
    code = codes[which.max(count_tokens(codes))],
    research_background = research_background
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

  # For each code and chunk, ask the LLM to mark relevant sections.
  total_combinations <- nrow(df) * length(codes)

  # Guard against runaway cost: the cross-product of split chunks x codes
  # can grow very large. Default limit is generous but prevents accidents.
  max_combinations <- getOption("marking__max_combinations", 50000)
  if (total_combinations > max_combinations) {
    stop(sprintf(
      paste0(
        "Marking would require %d LLM calls (%d text chunks x %d codes), ",
        "which exceeds the safety limit of %d. ",
        "Reduce the number of texts, codes, or increase the chunk size."
      ),
      total_combinations,
      nrow(df),
      length(codes),
      max_combinations
    ))
  }

  current_count <- 0
  try(
    {
      log_info("Marking Step 2: Marking texts...", component = "analysis")
      progress_set_with_total(
        progress_primary,
        2,
        total_steps,
        translate(
          "Teksten markeren..."
        )
      )
      progress_show(progress_secondary)
      progress_set_with_total(
        progress_secondary,
        current_count,
        total_combinations,
        "..."
      )
    },
    silent = TRUE
  )
  df_result <- df |>
    tidyr::crossing(code = codes) |>
    dplyr::mutate(
      marking_match = purrr::map2(chunk_text, code, function(txt, cd) {
        current_count <<- current_count + 1
        if (current_count == 1 || current_count %% 10 == 0) {
          log_info(
            sprintf(
              "Marking progress: %d/%d",
              current_count,
              total_combinations
            ),
            component = "analysis"
          )
        }
        try(
          {
            progress_set_with_total(
              progress_secondary,
              current_count,
              total_combinations,
              paste0(
                translate("Tekst markeren voor code '"),
                cd,
                "'..."
              )
            )
          },
          silent = TRUE
        )

        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        prompt <- mark_text_prompt(
          txt,
          cd,
          research_background = research_background,
          max_interactions = max_interactions
        )
        result <- send_prompt_with_retries(
          prompt,
          llm_provider,
          max_interactions = max_interactions
        )

        .kwallm_normalize_marking_matches(txt, result)
      })
    ) |>
    tidyr::unnest(
      marking_match,
      keep_empty = TRUE
    ) # Keep chunk/code rows even when nothing was marked.
  try(
    {
      progress_hide(progress_secondary)
      progress_set_with_total(
        progress_primary,
        2.5,
        total_steps,
        translate(
          "Resultaten opschonen..."
        )
      )
    },
    silent = TRUE
  )

  # Clean up the result. Keep distinct matched spans and only collapse overlap
  # duplicates when the same absolute span can be resolved back into the full
  # analysis-unit text.
  df_result_clean <- .kwallm_marking_clean_results(df_result)

  paragraphs <- NULL

  # Write paragraphs if requested
  if (write_paragraphs) {
    try(
      {
        log_info(
          "Marking Step 4: Writing paragraphs...",
          component = "analysis"
        )
        progress_set_with_total(
          progress_primary,
          3,
          total_steps,
          translate("Rapport schrijven...")
        )
      },
      silent = TRUE
    )

    # Build one highlighted excerpt per matched span. This keeps paragraph
    # evidence tied to the exact snippet the model matched instead of globally
    # replacing every repeated string in the full analysis-unit text.
    text_list <- .kwallm_marking_collect_paragraph_inputs(df_result_clean)

    # Create paragraphs for each code
    try(
      {
        progress_set_with_total(
          progress_secondary,
          0,
          length(text_list),
          "..."
        )
        progress_show(progress_secondary)
      },
      silent = TRUE
    )

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
      function(paragraph_input, code) {
        i <<- i + 1
        if (!is.null(interrupter)) {
          interrupter$execInterrupts()
        }

        try(
          {
            progress_set_with_total(
              progress_secondary,
              i,
              length(text_list),
              paste0(
                translate("Schrijven over '"),
                code,
                "'..."
              )
            )
          },
          silent = TRUE
        )

        # Clear streaming panel before this paragraph
        if (streaming_enabled && !is.null(llm_stream_async)) {
          try(llm_stream_async$clear())
        }

        paragraph <- write_paragraph(
          texts = paragraph_input$texts,
          analysis_unit_ids = paragraph_input$analysis_unit_ids,
          topic = code,
          research_background = research_background,
          style_prompt = style_prompt,
          llm_provider = llm_provider,
          language = paragraph_language,
          focus_on_highlighted_text = TRUE,
          stream_callback = stream_callback
        )

        return(paragraph)
      }
    )
    try(
      {
        progress_hide(progress_secondary)
        progress_set_with_total(
          progress_primary,
          3.5,
          total_steps,
          translate("Afronden...")
        )
      },
      silent = TRUE
    )

    attr(df_result_clean, "paragraphs") <- paragraphs
  }

  df_result_clean |>
    dplyr::select(
      analysis_unit_id,
      chunk_id,
      chunk_index,
      chunk_text,
      code,
      source_marked_text,
      marked_text,
      match_start,
      match_end,
      match_distance,
      match_method,
      response_status
    )
}


.kwallm_marking_find_absolute_span <- function(
  analysis_unit_text,
  chunk_text,
  match_start,
  match_end,
  marked_text
) {
  if (
    is.na(analysis_unit_text) ||
      is.na(chunk_text) ||
      is.na(match_start) ||
      is.na(match_end)
  ) {
    return(list(start = NA_integer_, end = NA_integer_))
  }

  analysis_unit_text <- as.character(analysis_unit_text)
  chunk_text <- as.character(chunk_text)
  marked_text <- as.character(marked_text)
  match_start <- as.integer(match_start)
  match_end <- as.integer(match_end)

  if (
    !nzchar(analysis_unit_text) ||
      !nzchar(chunk_text) ||
      match_start < 1L ||
      match_end < match_start ||
      match_end > nchar(chunk_text)
  ) {
    return(list(start = NA_integer_, end = NA_integer_))
  }

  expected_match <- if (!is.na(marked_text) && nzchar(marked_text)) {
    marked_text
  } else {
    substr(chunk_text, match_start, match_end)
  }

  chunk_starts <- gregexpr(chunk_text, analysis_unit_text, fixed = TRUE)[[1]]
  if (length(chunk_starts) == 1L && identical(chunk_starts[[1]], -1L)) {
    return(list(start = NA_integer_, end = NA_integer_))
  }

  candidates <- lapply(chunk_starts, function(chunk_start) {
    abs_start <- as.integer(chunk_start + match_start - 1L)
    abs_end <- as.integer(chunk_start + match_end - 1L)

    if (abs_end > nchar(analysis_unit_text)) {
      return(NULL)
    }

    if (
      !identical(substr(analysis_unit_text, abs_start, abs_end), expected_match)
    ) {
      return(NULL)
    }

    data.frame(
      start = abs_start,
      end = abs_end,
      stringsAsFactors = FALSE
    )
  })
  candidates <- Filter(Negate(is.null), candidates)

  if (!length(candidates)) {
    return(list(start = NA_integer_, end = NA_integer_))
  }

  candidates <- unique(do.call(rbind, candidates))
  if (nrow(candidates) != 1L) {
    return(list(start = NA_integer_, end = NA_integer_))
  }

  list(
    start = as.integer(candidates$start[[1]]),
    end = as.integer(candidates$end[[1]])
  )
}


.kwallm_marking_clean_results <- function(df_result) {
  stopifnot(is.data.frame(df_result))

  if (!nrow(df_result)) {
    return(df_result)
  }

  cleaned <- df_result |>
    dplyr::distinct(
      analysis_unit_id,
      code,
      chunk_id,
      match_start,
      match_end,
      marked_text,
      .keep_all = TRUE
    )

  matched_rows <- cleaned |>
    dplyr::filter(!is.na(marked_text) & nzchar(marked_text)) |>
    dplyr::distinct(
      analysis_unit_id,
      code,
      chunk_id,
      match_start,
      match_end,
      marked_text,
      .keep_all = TRUE
    )

  unmatched_rows <- cleaned |>
    dplyr::filter(is.na(marked_text) | !nzchar(marked_text)) |>
    dplyr::distinct(
      analysis_unit_id,
      code,
      chunk_id,
      .keep_all = TRUE
    )

  if (!nrow(matched_rows)) {
    return(unmatched_rows)
  }

  absolute_spans <- purrr::pmap(
    list(
      matched_rows$analysis_unit_text,
      matched_rows$chunk_text,
      matched_rows$match_start,
      matched_rows$match_end,
      matched_rows$marked_text
    ),
    .kwallm_marking_find_absolute_span
  )

  matched_rows$absolute_match_start <- vapply(
    absolute_spans,
    function(span) span$start,
    integer(1)
  )
  matched_rows$absolute_match_end <- vapply(
    absolute_spans,
    function(span) span$end,
    integer(1)
  )

  resolved <- matched_rows |>
    dplyr::filter(!is.na(absolute_match_start) & !is.na(absolute_match_end)) |>
    dplyr::distinct(
      analysis_unit_id,
      code,
      absolute_match_start,
      absolute_match_end,
      .keep_all = TRUE
    )

  unresolved <- matched_rows |>
    dplyr::filter(is.na(absolute_match_start) | is.na(absolute_match_end))

  dplyr::bind_rows(resolved, unresolved, unmatched_rows) |>
    dplyr::arrange(analysis_unit_id, code, chunk_id, match_start, match_end)
}


.kwallm_marking_build_highlighted_excerpt <- function(
  chunk_text,
  match_start,
  match_end,
  context_chars = 120L
) {
  if (is.na(chunk_text) || !nzchar(chunk_text)) {
    return("")
  }

  chunk_text <- as.character(chunk_text)
  match_start <- as.integer(match_start)
  match_end <- as.integer(match_end)
  context_chars <- max(as.integer(context_chars), 0L)

  if (
    is.na(match_start) ||
      is.na(match_end) ||
      match_start < 1L ||
      match_end < match_start ||
      match_end > nchar(chunk_text)
  ) {
    return(chunk_text)
  }

  excerpt_start <- max(1L, match_start - context_chars)
  excerpt_end <- min(nchar(chunk_text), match_end + context_chars)

  prefix <- substr(chunk_text, excerpt_start, match_start - 1L)
  matched <- substr(chunk_text, match_start, match_end)
  suffix <- substr(chunk_text, match_end + 1L, excerpt_end)

  paste0(
    if (excerpt_start > 1L) "..." else "",
    prefix,
    "**",
    matched,
    "**",
    suffix,
    if (excerpt_end < nchar(chunk_text)) "..." else ""
  )
}


.kwallm_marking_collect_paragraph_inputs <- function(df_result_clean) {
  stopifnot(is.data.frame(df_result_clean))

  matched_rows <- df_result_clean |>
    dplyr::filter(!is.na(marked_text) & nzchar(marked_text))

  if (!nrow(matched_rows)) {
    return(list())
  }

  excerpt_rows <- matched_rows |>
    dplyr::mutate(
      excerpt_text = purrr::pmap_chr(
        list(chunk_text, match_start, match_end),
        .kwallm_marking_build_highlighted_excerpt
      )
    ) |>
    dplyr::distinct(analysis_unit_id, code, excerpt_text)

  paragraph_input_df <- excerpt_rows |>
    dplyr::group_by(code) |>
    dplyr::summarise(
      texts = list(excerpt_text),
      analysis_unit_ids = list(as.integer(analysis_unit_id)),
      .groups = "drop"
    )

  stats::setNames(
    lapply(seq_len(nrow(paragraph_input_df)), function(i) {
      list(
        texts = paragraph_input_df$texts[[i]],
        analysis_unit_ids = paragraph_input_df$analysis_unit_ids[[i]]
      )
    }),
    paragraph_input_df$code
  )
}

# Helper: prompt to mark text
mark_text_prompt <- function(
  text,
  code,
  research_background = "",
  max_interactions = getOption("send_prompt_with_retries__max_interactions", 10)
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
          return(.kwallm_marking_status_row("matched_all"))
        }
        if (length(text_parts) == 1 && identical(text_parts[1], "")) {
          return(.kwallm_marking_status_row("matched_all"))
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
            matched <- res[!is.na(res$match), , drop = FALSE]
            if (!nrow(matched)) {
              return(.kwallm_marking_status_row(
                "partial_after_max_interactions"
              ))
            }
            return(.kwallm_marking_matches_from_find_matches(
              matched,
              response_status = "partial_after_max_interactions"
            ))
          }

          # Otherwise, ask the model to correct by quoting literally
          return(tidyprompt::llm_feedback(paste0(
            "Error: below text parts are not present in the original text:\n\n  - ",
            paste(shQuote(res$needle[missing_idx]), collapse = "\n\n  - "),
            "\n\nYou must quote literally from the original text."
          )))
        }

        .kwallm_marking_matches_from_find_matches(res)
      }
    )

  return(prompt)
}

# Helper: create an empty marking-match table.
.kwallm_empty_marking_matches <- function() {
  tibble::tibble(
    source_marked_text = character(),
    marked_text = character(),
    match_start = integer(),
    match_end = integer(),
    match_distance = integer(),
    match_method = character(),
    response_status = character()
  )
}


.kwallm_marking_status_row <- function(response_status = NA_character_) {
  tibble::tibble(
    source_marked_text = NA_character_,
    marked_text = NA_character_,
    match_start = NA_integer_,
    match_end = NA_integer_,
    match_distance = NA_integer_,
    match_method = NA_character_,
    response_status = as.character(response_status)
  )
}

# Helper: convert raw fuzzy-match output into the stored marking schema.
.kwallm_marking_matches_from_find_matches <- function(
  matches,
  response_status = "matched_all"
) {
  if (!nrow(matches)) {
    return(.kwallm_marking_status_row(response_status))
  }

  tibble::tibble(
    source_marked_text = as.character(matches$needle),
    marked_text = as.character(matches$match),
    match_start = as.integer(matches$start),
    match_end = as.integer(matches$end),
    match_distance = as.integer(matches$distance),
    match_method = ifelse(
      is.na(matches$distance),
      NA_character_,
      ifelse(matches$distance == 0, "exact", "fuzzy")
    ),
    response_status = rep(response_status, nrow(matches))
  )
}

# Helper: normalize different send_prompt_with_retries return shapes.
.kwallm_normalize_marking_matches <- function(text, result) {
  required_cols <- c(
    "source_marked_text",
    "marked_text",
    "match_start",
    "match_end",
    "match_distance",
    "match_method",
    "response_status"
  )

  if (is.null(result)) {
    return(.kwallm_empty_marking_matches())
  }

  if (is.data.frame(result)) {
    if (!nrow(result)) {
      return(.kwallm_empty_marking_matches())
    }

    if (all(required_cols %in% names(result))) {
      return(tibble::as_tibble(result[required_cols]))
    }

    if (
      all(c("needle", "match", "distance", "start", "end") %in% names(result))
    ) {
      response_status <- if ("response_status" %in% names(result)) {
        as.character(result$response_status[[1]])
      } else {
        "matched_all"
      }
      return(.kwallm_marking_matches_from_find_matches(
        result,
        response_status = response_status
      ))
    }
  }

  if (is.character(result)) {
    if (!length(result)) {
      return(.kwallm_empty_marking_matches())
    }

    matches <- find_matches(
      haystack = text,
      needles = result,
      rel = 0.12,
      abs = 2,
      step_div = 5L
    )
    matches <- matches[!is.na(matches$match), , drop = FALSE]
    return(.kwallm_marking_matches_from_find_matches(matches))
  }

  stop("Unexpected marking match result type")
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
