# Function to send a prompt to a LLM, with retry logic in case of errors
# This is a wrapper around `tidyprompt::send_prompt()`

#' Send prompt with retries
#'
#' @param prompt A tidyprompt object representing the prompt to be sent
#' @param llm_provider A tidyprompt LLM provider object
#' @param max_tries Maximum number of attempts in connecting to the LLM
#' @param retry_delay_seconds Number of seconds to wait before retrying
#' @param max_interactions Maximum number of interactions with the LLM
#'  (this is the maximum number of messages that will be sent to the LLM
#'  before stopping an interaction; this is used to prevent indefinite
#'  loops in case the LLM does not respond in the expected format)
#' @return The response from the LLM
#' @export
send_prompt_with_retries <- function(
  prompt,
  llm_provider = tidyprompt::llm_provider_openai(),
  max_tries = getOption("send_prompt_with_retries__max_tries", 10),
  retry_delay_seconds = getOption(
    "send_prompt_with_retries__retry_delay_seconds",
    3
  ),
  max_interactions = getOption(
    "send_prompt_with_retries__max_interactions",
    10
  ),
  debug_logging = getOption("send_prompt_with_retries__log_prompts", FALSE)
) {
  tries <- 0
  result <- NULL

  while (tries < max_tries) {
    tries <- tries + 1
    result <- tryCatch(
      {
        result <- prompt |>
          tidyprompt::send_prompt(
            llm_provider,
            return_mode = "full",
            max_interactions = max_interactions
          )

        if (debug_logging) {
          dir.create("prompt_logs", showWarnings = FALSE)
          # Create timestamped filename
          timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          log_file <- paste0("prompt_logs/log_", timestamp, ".json")
          # Convert to JSON and write to file
          result |>
            jsonlite::toJSON(pretty = TRUE, force = TRUE, auto_unbox = TRUE) |>
            writeLines(con = log_file)
        }

        result
      },
      error = function(e) {
        if (tries == max_tries) {
          stop(sprintf(
            "Error in LLM call after %d attempts: %s\nFinal error:\n%s",
            max_tries,
            conditionMessage(e),
            paste(capture.output(str(e)), collapse = "\n")
          ))
        }
        Sys.sleep(retry_delay_seconds)
        NULL
      }
    )

    if (!is.null(result)) {
      break
    }
  }

  if (is.null(result)) {
    stop(paste0(
      "Failed to get a response from the LLM after ",
      max_tries,
      " attempts. Please check your connection or the LLM provider settings"
    ))
  }

  if (is.null(result$response)) {
    stop(paste0(
      "Reached the LLM, but failed to get a valid reply",
      "\n\n--- Chat history: ---\n\n",
      if (is.data.frame(result$chat_history)) {
        tidyprompt::df_to_string(result$chat_history, how = "long")
      } else {
        'NULL'
      }
    ))
  }

  return(result$response)
}
