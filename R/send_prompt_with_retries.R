# Function to send a prompt to a LLM, with retry logic in case of errors
# This is a wrapper around `tidyprompt::send_prompt()`

#' Send prompt with retries
#'
#' @param prompt A tidyprompt object representing the prompt to be sent
#' @param llm_provider A tidyprompt LLM provider object
#' @param max_tries Maximum number of attempts to send the prompt
#' @param retry_delay_seconds Number of seconds to wait before retrying
#'
#' @return The response from the LLM
#' @export
send_prompt_with_retries <- function(
  prompt,
  llm_provider = tidyprompt::llm_provider_openai(),
  max_tries = getOption("send_prompt_with_retries___max_tries", 10),
  retry_delay_seconds = getOption(
    "send_prompt_with_retries___retry_delay_seconds",
    3
  )
) {
  tries <- 0
  result <- NULL

  while (tries < max_tries) {
    tries <- tries + 1
    result <- tryCatch(
      {
        prompt |> tidyprompt::send_prompt(llm_provider)
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
    stop(
      "Failed to get a response from the LLM after ",
      max_tries,
      " attempts. Please check your connection or the LLM provider settings."
    )
  }

  return(result)
}
