# Helper function with some hardcoded context window sizes for common models
# Will default to 2048 if the model is not recognized
# Better approach may be to retrieve via API or configuration file
get_context_window_size_in_tokens <- function(model) {
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
