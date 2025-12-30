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
  prompt_id <- .kwallm__prompt_trace_new_id()
  prompt_text <- NULL

  # Log LLM call start
  tryCatch(
    log_debug(
      sprintf("LLM call started: prompt_id=%s, model=%s", prompt_id, model_name),
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

    # Log prompt text once (first send), but include prompt_id for later correlation.
    if (tries == 1 && (is.null(prompt_text) || !is.character(prompt_text))) {
      prompt_text <- .kwallm__prompt_trace_extract_prompt_text(prompt)
      .kwallm__prompt_trace_log_send(
        prompt_id = prompt_id,
        model_name = model_name,
        attempt = tries,
        max_tries = max_tries,
        prompt_text = prompt_text
      )
    }

    result <- tryCatch(
      {
        result <- prompt |>
          tidyprompt::send_prompt(
            llm_provider,
            return_mode = "full",
            max_interactions = max_interactions,
            verbose = FALSE
          )

        if (tries == 1) {
          # Log initial prompt (debug only)
          tryCatch(
            log_debug(
              sprintf("Sending prompt: prompt_id=%s, model=%s", prompt_id, model_name),
              component = "llm_trace"
            ),
            error = function(e) NULL
          )
        }

        result
      },
      error = function(e) {
        .kwallm__prompt_trace_log_error(
          prompt_id = prompt_id,
          model_name = model_name,
          attempt = tries,
          max_tries = max_tries,
          err_message = conditionMessage(e)
        )

        # Log retry attempt
        tryCatch(
          log_warn(
            sprintf(
              "LLM call failed (prompt_id=%s, attempt %d/%d): %s",
              prompt_id,
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
          "LLM call failed after %d attempts: prompt_id=%s, model=%s",
          max_tries,
          prompt_id,
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
        sprintf("LLM returned NULL response: prompt_id=%s, model=%s", prompt_id, model_name),
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
        "LLM call success: prompt_id=%s, model=%s, attempts=%d, duration=%.0fms",
        prompt_id,
        model_name,
        tries,
        duration_ms
      ),
      component = "llm"
    ),
    error = function(e) NULL
  )

  # Optional: log final response text (correlated via prompt_id)
  .kwallm__prompt_trace_log_reply(
    prompt_id = prompt_id,
    model_name = model_name,
    attempts = tries,
    duration_ms = duration_ms,
    response_text = .kwallm__prompt_trace_serialize(result$response)
  )

  return(result$response)
}


# Helpers (prompt/response tracing) ------------------------------------------

.kwallm__prompt_trace_new_id <- function() {
  ts <- format(Sys.time(), "%Y%m%dT%H%M%OS3Z", tz = "UTC")
  pid <- tryCatch(as.character(Sys.getpid()), error = function(e) "?")
  rand <- paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  paste0(ts, "-", pid, "-", rand)
}

.kwallm__prompt_trace_enabled_to_file <- function() {
  isTRUE(getOption("send_prompt_with_retries__log_prompts_to_file", FALSE)) ||
    isTRUE(getOption("send_prompt_with_retries__log_prompts", FALSE))
}

.kwallm__prompt_trace_enabled_to_logs <- function() {
  isTRUE(getOption("send_prompt_with_retries__log_prompts_to_logs", FALSE))
}

.kwallm__prompt_trace_retention_files <- function() {
  # Number of prompt trace files to keep. NULL = keep indefinitely.
  retention <- getOption("send_prompt_with_retries__prompt_trace_retention_files", NULL)
  if (is.null(retention)) {
    # Convenience fallback: reuse main logger retention if configured
    retention <- getOption("logger__retention", NULL)
  }
  if (is.null(retention)) return(NULL)
  if (!is.numeric(retention) || length(retention) != 1) return(NULL)
  retention <- as.integer(retention)
  if (is.na(retention) || retention <= 0) return(NULL)
  retention
}

.kwallm__prompt_trace_file_path <- function() {
  configured <- getOption("send_prompt_with_retries__prompt_trace_file", NULL)
  if (!is.null(configured) && is.character(configured) && nzchar(configured)) {
    return(configured)
  }

  log_dir <- getOption("logger__dir", "logs")
  file.path(
    log_dir,
    "prompt_logs",
    paste0("prompt_trace_", format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
}

.kwallm__prompt_trace_append <- function(lines, file_path = .kwallm__prompt_trace_file_path()) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  cat(paste0(lines, collapse = "\n"), "\n", file = file_path, append = TRUE)
  .kwallm__prompt_trace_cleanup(file_path)
  invisible(NULL)
}

.kwallm__prompt_trace_cleanup <- function(file_path) {
  retention <- .kwallm__prompt_trace_retention_files()
  if (is.null(retention)) return(invisible(NULL))

  # Avoid doing filesystem scans too frequently.
  last <- getOption("kwallm__prompt_trace_last_cleanup", NULL)
  if (inherits(last, "POSIXct")) {
    if (as.numeric(difftime(Sys.time(), last, units = "secs")) < 3600) {
      return(invisible(NULL))
    }
  }
  options(kwallm__prompt_trace_last_cleanup = Sys.time())

  dir <- dirname(file_path)
  if (!dir.exists(dir)) return(invisible(NULL))

  # Only target our own trace files.
  trace_files <- list.files(
    dir,
    pattern = "^prompt_trace_.*\\.log$",
    full.names = TRUE,
    ignore.case = FALSE
  )
  if (length(trace_files) <= retention) return(invisible(NULL))

  # Prefer ordering by date embedded in filename (prompt_trace_YYYY-MM-DD.log),
  # since mtimes can be unreliable across platforms/filesystems.
  base <- basename(trace_files)
  date_str <- sub("^prompt_trace_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.log$", "\\1", base)
  parsed_date <- suppressWarnings(as.Date(date_str))
  parsed_date[is.na(parsed_date) | date_str == base] <- NA

  info <- tryCatch(file.info(trace_files), error = function(e) NULL)
  mtime <- if (!is.null(info) && !is.null(info$mtime)) info$mtime else rep(as.POSIXct(NA), length(trace_files))

  # Order old -> new (by parsed date, then mtime, then name)
  ord <- order(parsed_date, mtime, base, decreasing = FALSE, na.last = TRUE)
  sorted <- trace_files[ord]
  to_remove <- head(sorted, length(sorted) - retention)

  # Best-effort cleanup; must never crash.
  tryCatch(unlink(to_remove, force = TRUE), error = function(e) NULL)
  invisible(NULL)
}

.kwallm__prompt_trace_serialize <- function(x) {
  if (is.null(x)) return("NULL")

  if (is.character(x)) {
    return(paste(x, collapse = ""))
  }

  if (is.data.frame(x)) {
    return(tryCatch(
      tidyprompt::df_to_string(x, how = "long"),
      error = function(e) paste(capture.output(print(x)), collapse = "\n")
    ))
  }

  paste(capture.output(print(x)), collapse = "\n")
}

.kwallm__prompt_trace_extract_prompt_text <- function(prompt) {
  # Best-effort prompt extraction; must never crash.
  tryCatch(
    {
      if (is.list(prompt)) {
        # Common patterns: prompt$messages or prompt$chat_history
        if (!is.null(prompt$messages) && is.data.frame(prompt$messages) && "content" %in% names(prompt$messages)) {
          return(paste(prompt$messages$content, collapse = "\n\n"))
        }
        if (!is.null(prompt$chat_history) && is.data.frame(prompt$chat_history) && "content" %in% names(prompt$chat_history)) {
          return(paste(prompt$chat_history$content, collapse = "\n\n"))
        }
      }
      .kwallm__prompt_trace_serialize(prompt)
    },
    error = function(e) paste0("<failed-to-serialize-prompt: ", conditionMessage(e), ">")
  )
}

.kwallm__prompt_trace_log_send <- function(prompt_id, model_name, attempt, max_tries, prompt_text) {
  if (.kwallm__prompt_trace_enabled_to_logs()) {
    tryCatch(
      log_debug(
        sprintf(
          "LLM prompt send: prompt_id=%s, model=%s, attempt=%d/%d\n%s",
          prompt_id,
          model_name,
          attempt,
          max_tries,
          prompt_text
        ),
        component = "llm_prompt"
      ),
      error = function(e) NULL
    )
  }

  if (.kwallm__prompt_trace_enabled_to_file()) {
    tryCatch(
      .kwallm__prompt_trace_append(c(
        "---- PROMPT_SEND ----",
        paste0("time_utc=", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
        paste0("prompt_id=", prompt_id),
        paste0("model=", model_name),
        paste0("attempt=", attempt),
        paste0("max_tries=", max_tries),
        "prompt_text:",
        prompt_text,
        "---- /PROMPT_SEND ----"
      )),
      error = function(e) NULL
    )
  }
}

.kwallm__prompt_trace_log_reply <- function(prompt_id, model_name, attempts, duration_ms, response_text) {
  if (.kwallm__prompt_trace_enabled_to_logs()) {
    tryCatch(
      log_debug(
        sprintf(
          "LLM reply received: prompt_id=%s, model=%s, attempts=%d, duration=%.0fms\n%s",
          prompt_id,
          model_name,
          attempts,
          duration_ms,
          response_text
        ),
        component = "llm_reply"
      ),
      error = function(e) NULL
    )
  }

  if (.kwallm__prompt_trace_enabled_to_file()) {
    tryCatch(
      .kwallm__prompt_trace_append(c(
        "---- RESPONSE_RECEIVED ----",
        paste0("time_utc=", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
        paste0("prompt_id=", prompt_id),
        paste0("model=", model_name),
        paste0("attempts=", attempts),
        paste0("duration_ms=", sprintf("%.0f", duration_ms)),
        "response_text:",
        response_text,
        "---- /RESPONSE_RECEIVED ----"
      )),
      error = function(e) NULL
    )
  }
}

.kwallm__prompt_trace_log_error <- function(prompt_id, model_name, attempt, max_tries, err_message) {
  if (.kwallm__prompt_trace_enabled_to_logs()) {
    tryCatch(
      log_warn(
        sprintf(
          "LLM call error: prompt_id=%s, model=%s, attempt=%d/%d: %s",
          prompt_id,
          model_name,
          attempt,
          max_tries,
          err_message
        ),
        component = "llm"
      ),
      error = function(e) NULL
    )
  }

  if (.kwallm__prompt_trace_enabled_to_file()) {
    tryCatch(
      .kwallm__prompt_trace_append(c(
        "---- CALL_ERROR ----",
        paste0("time_utc=", format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")),
        paste0("prompt_id=", prompt_id),
        paste0("model=", model_name),
        paste0("attempt=", attempt),
        paste0("max_tries=", max_tries),
        "error_message:",
        err_message,
        "---- /CALL_ERROR ----"
      )),
      error = function(e) NULL
    )
  }
}
