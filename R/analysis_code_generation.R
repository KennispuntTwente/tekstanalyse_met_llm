#' Generate marking codes by reading texts
#'
#' @param texts Character vector of texts.
#' @param text_size_tokens Chunk size used for semantic sub-text splitting.
#' @param overlap_size_tokens Overlap used for semantic sub-text splitting.
#' @param research_background Optional research background string.
#' @param llm_provider LLM provider object.
#' @param queue Optional async queue used by the app to surface progress messages.
#' @param interrupter Optional object with \code{$execInterrupts()} method for
#'   cancellation support.
#' @param language Output language for generated codes.
#'
#' @return Character vector with generated codes.
#' @export
generate_codes_by_reading_texts <- function(
  texts,
  text_size_tokens = 256,
  overlap_size_tokens = 64,
  research_background = "",
  llm_provider,
  queue = NULL,
  interrupter = NULL,
  language = c("nl", "en")
) {
  language <- match.arg(language)

  stopifnot(
    is.character(texts),
    length(texts) > 0,
    all(nzchar(texts)),
    is.numeric(text_size_tokens),
    length(text_size_tokens) == 1,
    text_size_tokens > 0,
    is.numeric(overlap_size_tokens),
    length(overlap_size_tokens) == 1,
    overlap_size_tokens >= 0,
    is.character(research_background),
    length(research_background) == 1
  )

  print_message <- function(
    message,
    type = c("info", "success")
  ) {
    type <- match.arg(type)
    if (type == "success") {
      cli::cli_alert_success(message)
    } else {
      cli::cli_alert_info(message)
    }

    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive(
        "generate_codes_message",
        message
      ))
    }
  }

  # Translate progress messages using shiny.i18n, same pattern as mark_texts.
  translate <- local({
    lang <- tryCatch(
      {
        tr <- shiny.i18n::Translator$new(
          translation_json_path = "language/language.json"
        )
        tr$set_translation_language(language)
        tr
      },
      error = function(e) NULL
    )
    function(text) {
      if (is.null(lang)) {
        return(text)
      }
      lang$t(text)
    }
  })

  print_message(translate("Semantische chunker laden..."))
  chunker_name <- paste0("semchunker_", text_size_tokens)
  if (!exists(chunker_name)) {
    semchunker <- semchunk_load_chunker(chunk_size = text_size_tokens)
    assign(chunker_name, semchunker, envir = .GlobalEnv)
  } else {
    semchunker <- get(chunker_name, envir = .GlobalEnv)
  }

  if (!is.null(interrupter)) {
    interrupter$execInterrupts()
  }

  split_texts <- semchunker(
    texts,
    overlap = overlap_size_tokens
  ) |>
    unlist()

  if (length(split_texts) == length(texts)) {
    print_message(translate(
      "Geen splitsing nodig, originele teksten worden gebruikt..."
    ))
  } else {
    msg <- translate(
      "{n_texts} teksten gesplitst in {n_split} kleinere teksten..."
    )
    msg <- gsub("{n_texts}", length(texts), msg, fixed = TRUE)
    msg <- gsub("{n_split}", length(split_texts), msg, fixed = TRUE)
    print_message(msg)
  }

  if (!is.null(interrupter)) {
    interrupter$execInterrupts()
  }

  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) {
    n_tokens_context_window <- 2048
  }

  # Subtract prompt overhead so chunks don't overflow once the prompt is added
  base_prompt_text <- prompt_candidate_topics(
    text_batch = c(""),
    research_background = research_background,
    language = language
  ) |>
    tidyprompt::construct_prompt_text()

  batches <- create_text_batches(
    split_texts,
    batch_size = 50,
    draws = 1,
    n_tokens_context_window = n_tokens_context_window,
    base_prompt_text = base_prompt_text,
    text_formatter = function(text, index) {
      paste0("<text ", index, ">\n", text, "\n</text ", index, ">")
    }
  )

  if (is.null(batches) || length(batches) == 0) {
    stop(
      "Cannot generate codes: at least one text exceeds the context window ",
      "after subtracting prompt overhead (",
      n_tokens_context_window,
      " tokens available). Try using a model with a larger context window or ",
      "shorter input texts."
    )
  }

  {
    msg <- translate(
      "{n_batches} tekstbatch(es) aangemaakt van de teksten..."
    )
    msg <- gsub("{n_batches}", length(batches), msg, fixed = TRUE)
    print_message(msg)
  }

  candidate_topics <- unique(create_candidate_topics(
    text_batches = batches,
    research_background = research_background,
    llm_provider = llm_provider,
    language = language,
    on_progress = function(i, n, batch, result) {
      force(batch)
      msg <- translate(
        "Batch {i} van {n} gelezen ({n_codes} kandidaat-codes)"
      )
      msg <- gsub("{i}", i, msg, fixed = TRUE)
      msg <- gsub("{n}", n, msg, fixed = TRUE)
      msg <- gsub("{n_codes}", length(result), msg, fixed = TRUE)
      print_message(msg)
    },
    interrupter = interrupter
  ))

  {
    msg <- translate(
      "{n_candidates} kandidaat-codes aangemaakt, reduceren naar definitieve lijst..."
    )
    msg <- gsub("{n_candidates}", length(candidate_topics), msg, fixed = TRUE)
    print_message(msg)
  }

  final_topics <- reduce_topics(
    candidate_topics = candidate_topics,
    research_background = research_background,
    llm_provider = llm_provider,
    always_add_not_applicable = FALSE,
    interrupter = interrupter,
    language = language
  )

  {
    msg <- translate("{n_codes} codes gegenereerd")
    msg <- gsub("{n_codes}", length(final_topics), msg, fixed = TRUE)
    print_message(msg, type = "success")
  }

  final_topics
}
