# Helper function with some hardcoded context window sizes for common models
# Will default to 2048 if the model is not recognized
# Better approach may be to retrieve via API or configuration file
get_context_window_size_in_tokens <- function(model) {
  if (
    model %in%
      c(
        "kwallm-fake-main-1024"
      )
  ) {
    return(1024)
  }

  if (
    model %in%
      c(
        "kwallm-fake-reducer-320"
      )
  ) {
    return(320)
  }

  if (
    model %in%
      c(
        "gpt-4.1-mini-2025-04-14",
        "gpt-4.1-2025-04-14",
        "gpt-4.1",
        "gpt-4.1-mini"
      )
  ) {
    return(1047576)
  }

  if (
    model %in%
      c(
        "gpt-5",
        "gpt-5-2025-08-07",
        "gpt-5-mini",
        "gpt-5-mini-2025-08-07",
        "gpt-5-nano",
        "gpt-5-nano-2025-08-07"
      )
  ) {
    return(400000)
  }

  if (
    model %in%
      c(
        "o4-mini-2025-04-16",
        "o3-2025-04-16",
        "o3-mini-2025-01-31",
        "o1-2024-12-17",
        "o1-pro-2025-03-19",
        "o4-mini",
        "o3",
        "o3-mini",
        "o1",
        "o1-pro"
      )
  ) {
    return(200000)
  }

  if (
    model %in%
      c(
        "gpt-4o-2024-08-06",
        "chatgpt-4o-latest",
        "gpt-4o-mini-2024-07-18",
        "gpt-4o-mini",
        "gpt-4o",
        "gpt-5-main",
        "gpt-5-chat-latest"
      )
  ) {
    return(128000)
  }

  if (
    model %in%
      c(
        "gpt-3.5-turbo-0125"
      )
  ) {
    return(4096)
  }

  return(NULL)
}


# Check whether the real topic-assignment prompt fits in the model context window.
# This uses the current topic list plus the longest text that will be assigned.
topic_assignment_prompt_context_window_check <- function(
  texts,
  topics,
  research_background = "",
  llm_provider,
  assign_multiple_categories = FALSE,
  exclusive_topics = character(),
  n_tokens_context_window = NULL
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.character(topics),
    length(topics) > 0,
    is.character(research_background),
    length(research_background) == 1,
    all(exclusive_topics %in% topics)
  )

  provider_model <- tryCatch(
    llm_provider$parameters$model,
    error = function(e) NULL
  )

  stopifnot(
    !is.null(provider_model),
    is.character(provider_model),
    length(provider_model) == 1
  )

  longest_text <- texts[[which.max(count_tokens(texts))]]

  assignment_prompt <- if (isTRUE(assign_multiple_categories)) {
    prompt_multi_category(
      text = longest_text,
      categories = topics,
      research_background = research_background,
      exclusive_categories = exclusive_topics
    )
  } else {
    prompt_category(
      text = longest_text,
      categories = topics,
      research_background = research_background
    )
  }

  if (!is.null(n_tokens_context_window)) {
    assignment_context_window <- n_tokens_context_window
  } else {
    assignment_context_window <- get_context_window_size_in_tokens(
      provider_model
    )
    if (is.null(assignment_context_window)) {
      assignment_context_window <- 2048
      tryCatch(
        log_warn(
          sprintf(
            "Unknown context window for model '%s'; falling back to %d tokens.",
            provider_model,
            assignment_context_window
          ),
          component = "context_window"
        ),
        error = function(e) NULL
      )
    }
  }

  assignment_prompt_tokens <- assignment_prompt |>
    tidyprompt::construct_prompt_text() |>
    count_tokens()

  list(
    fits = assignment_prompt_tokens <= assignment_context_window,
    prompt_tokens = as.integer(assignment_prompt_tokens),
    context_window_tokens = as.integer(assignment_context_window)
  )
}
