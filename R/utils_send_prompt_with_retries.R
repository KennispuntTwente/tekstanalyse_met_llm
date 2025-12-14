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
#' @param stream_callback Optional callback function for streaming. If provided,
#'  will be attached to a cloned provider. The callback receives (token, meta)
#'  where meta$partial_response contains accumulated text.
#' @return The response from the LLM
#' @export
send_prompt_with_retries <- function(
  prompt,
  llm_provider,
  max_tries = getOption("send_prompt_with_retries__max_tries", 10),
  retry_delay_seconds = getOption(
    "send_prompt_with_retries__retry_delay_seconds",
    3
  ),
  max_interactions = getOption(
    "send_prompt_with_retries__max_interactions",
    10
  ),
  stream_callback = NULL
) {
  tries <- 0
  result <- NULL
  call_start_time <- Sys.time()
  model_name <- llm_provider$parameters$model %||% "unknown"

  # Log LLM call start
  tryCatch(
    log_debug(
      sprintf("LLM call started: model=%s", model_name),
      component = "llm"
    ),
    error = function(e) NULL
  )

  # If stream_callback is provided, clone provider and attach callback
  if (!is.null(stream_callback) && is.function(stream_callback)) {
    llm_provider <- llm_provider$clone()
    llm_provider$parameters$stream <- TRUE
    llm_provider$stream_callback <- stream_callback
  }

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

        if (tries == 1) {
          # Log initial prompt (debug only)
          tryCatch(
            log_debug(
              sprintf("Sending prompt to %s", model_name),
              component = "llm_trace"
            ),
            error = function(e) NULL
          )
        }

        result
      },
      error = function(e) {
        # Log retry attempt
        tryCatch(
          log_warn(
            sprintf(
              "LLM call failed (attempt %d/%d): %s",
              tries,
              max_tries,
              conditionMessage(e)
            ),
            component = "llm"
          ),
          error = function(e2) NULL
        )

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
    # Log final failure
    tryCatch(
      log_error(
        sprintf(
          "LLM call failed after %d attempts: model=%s",
          max_tries,
          model_name
        ),
        component = "llm"
      ),
      error = function(e) NULL
    )
    stop(paste0(
      "Failed to get a response from the LLM after ",
      max_tries,
      " attempts. Please check your connection or the LLM provider settings"
    ))
  }

  if (is.null(result$response)) {
    # Log invalid response
    tryCatch(
      log_error(
        sprintf("LLM returned NULL response: model=%s", model_name),
        component = "llm"
      ),
      error = function(e) NULL
    )
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

  # Log successful LLM call
  duration_ms <- as.numeric(difftime(
    Sys.time(),
    call_start_time,
    units = "secs"
  )) *
    1000
  tryCatch(
    log_debug(
      sprintf(
        "LLM call success: model=%s, attempts=%d, duration=%.0fms",
        model_name,
        tries,
        duration_ms
      ),
      component = "llm"
    ),
    error = function(e) NULL
  )

  return(result$response)
}
