#' Generate marking codes by reading texts
#'
#' @param texts Character vector of texts.
#' @param text_size_tokens Chunk size used for semantic sub-text splitting.
#' @param overlap_size_tokens Overlap used for semantic sub-text splitting.
#' @param research_background Optional research background string.
#' @param llm_provider LLM provider object.
#' @param queue Optional IPC queue used by the app to surface progress messages.
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
      message <- paste0(
        cli::col_green("OK"),
        " ",
        message
      )
    } else {
      message <- paste0(
        cli::col_blue("i"),
        " ",
        message
      )
      cli::cli_alert_info(message)
    }

    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive(
        "generate_codes_message",
        message
      ))
    }
  }

  print_message("Loading semantic chunker...")
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
    print_message("No splitting needed, using original texts...")
  } else {
    print_message(paste0(
      "Split ",
      length(texts),
      " texts into ",
      length(split_texts),
      " smaller texts..."
    ))
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

  print_message(paste0(
    "Created ",
    length(batches),
    " text batch(es) from the texts..."
  ))

  candidate_topics <- unique(create_candidate_topics(
    text_batches = batches,
    research_background = research_background,
    llm_provider = llm_provider,
    language = language,
    on_progress = function(i, n, batch, result) {
      force(batch)
      print_message(paste0(
        "Read batch ",
        i,
        " of ",
        n,
        " (",
        length(result),
        " candidate codes)"
      ))
    },
    interrupter = interrupter
  ))

  print_message(paste0(
    "Created ",
    length(candidate_topics),
    " candidate codes, reducing to final list..."
  ))

  final_topics <- reduce_topics(
    candidate_topics = candidate_topics,
    research_background = research_background,
    llm_provider = llm_provider,
    always_add_not_applicable = FALSE,
    interrupter = interrupter,
    language = language
  )

  print_message(
    paste0("Generated ", length(final_topics), " codes"),
    type = "success"
  )

  final_topics
}
