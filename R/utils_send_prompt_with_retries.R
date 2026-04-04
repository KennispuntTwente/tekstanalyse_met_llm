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

  # Inline helper resolver (needed for mirai async contexts where helpers
  # are passed via .args and not in lexical scope)
  resolve_helper <- function(name) {
    # First try to find in calling environment chain (for mirai .args)
    for (e in sys.frames()) {
      if (exists(name, envir = e, inherits = FALSE)) {
        fn <- get(name, envir = e, inherits = FALSE)
        if (is.function(fn)) return(fn)
      }
    }
    # Fall back to regular lookup (works in normal context)
    tryCatch(
      get(name, envir = parent.frame(2), inherits = TRUE),
      error = function(err) {
        # Last resort: try global environment
        get(name, envir = globalenv(), inherits = TRUE)
      }
    )
  }

  # Resolve ALL trace helpers dynamically at start
  # These will be passed as arguments to log functions to avoid nested resolution
  .trace_new_id <- resolve_helper(".kwallm__prompt_trace_new_id")
  .trace_extract_prompt_text <- resolve_helper(
    ".kwallm__prompt_trace_extract_prompt_text"
  )
  .trace_log_send <- resolve_helper(".kwallm__prompt_trace_log_send")
  .trace_log_reply <- resolve_helper(".kwallm__prompt_trace_log_reply")
  .trace_log_error <- resolve_helper(".kwallm__prompt_trace_log_error")
  .trace_serialize <- resolve_helper(".kwallm__prompt_trace_serialize")

  # Also resolve the dependencies needed BY the log functions
  .trace_enabled <- resolve_helper(".kwallm__prompt_trace_enabled_to_file")
  .trace_append <- resolve_helper(".kwallm__prompt_trace_append")
  .trace_session_id <- resolve_helper(".kwallm__prompt_trace_session_id")
  .exec_current_stage <- resolve_helper(
    ".kwallm__prompt_execution_current_stage"
  )
  .exec_record <- resolve_helper(".kwallm__prompt_execution_record")

  # Bundle trace context for passing to log functions
  .trace_ctx <- list(
    enabled = .trace_enabled,
    append = .trace_append,
    session_id = .trace_session_id
  )

  prompt_id <- .trace_new_id()
  prompt_text <- NULL
  error_messages <- character()

  record_execution <- function(completion_status, final_error_message = NULL) {
    duration_ms <- as.numeric(difftime(
      Sys.time(),
      call_start_time,
      units = "secs"
    )) *
      1000

    .exec_record(data.frame(
      prompt_id = prompt_id,
      stage_id = .exec_current_stage(),
      model_id = as.character(model_name),
      started_at = format(
        call_start_time,
        "%Y-%m-%dT%H:%M:%OS3Z",
        tz = "UTC"
      ),
      completed_at = format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%OS3Z",
        tz = "UTC"
      ),
      duration_ms = duration_ms,
      attempt_count = as.integer(tries),
      retry_count = as.integer(max(tries - 1, 0)),
      max_tries = as.integer(max_tries),
      retry_delay_seconds = as.numeric(retry_delay_seconds),
      max_interactions = as.integer(max_interactions),
      completion_status = as.character(completion_status),
      error_messages = if (length(error_messages)) {
        paste(unique(error_messages), collapse = " || ")
      } else {
        NA_character_
      },
      final_error_message = if (is.null(final_error_message)) {
        NA_character_
      } else {
        as.character(final_error_message)
      },
      stringsAsFactors = FALSE
    ))
  }

  # Log LLM call start
  tryCatch(
    log_debug(
      sprintf(
        "LLM call started: prompt_id=%s, model=%s",
        prompt_id,
        model_name
      ),
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
      prompt_text <- .trace_extract_prompt_text(prompt)
      .trace_log_send(
        prompt_id = prompt_id,
        model_name = model_name,
        attempt = tries,
        max_tries = max_tries,
        prompt_text = prompt_text,
        .ctx = .trace_ctx
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
              sprintf(
                "Sending prompt: prompt_id=%s, model=%s",
                prompt_id,
                model_name
              ),
              component = "llm_trace"
            ),
            error = function(e) NULL
          )
        }

        result
      },
      error = function(e) {
        error_messages <<- c(error_messages, conditionMessage(e))
        .trace_log_error(
          prompt_id = prompt_id,
          model_name = model_name,
          attempt = tries,
          max_tries = max_tries,
          err_message = conditionMessage(e),
          .ctx = .trace_ctx
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
          record_execution(
            completion_status = "error",
            final_error_message = conditionMessage(e)
          )
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
    record_execution(
      completion_status = "error",
      final_error_message = paste0(
        "Failed to get a response from the LLM after ",
        max_tries,
        " attempts."
      )
    )
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
    record_execution(
      completion_status = "invalid_response",
      final_error_message = "Reached the LLM, but failed to get a valid reply"
    )
    # Log invalid response
    tryCatch(
      log_error(
        sprintf(
          "LLM returned NULL response: prompt_id=%s, model=%s",
          prompt_id,
          model_name
        ),
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

  # Record execution provenance before logging so duration_ms and completed_at
  # reflect the actual LLM call time rather than including logging overhead.
  record_execution(completion_status = "success")

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
  .trace_log_reply(
    prompt_id = prompt_id,
    model_name = model_name,
    attempts = tries,
    duration_ms = duration_ms,
    response_text = .trace_serialize(result$response),
    .ctx = .trace_ctx
  )

  return(result$response)
}


# Helpers (execution provenance) ---------------------------------------------

.kwallm__prompt_execution_current_stage <- function() {
  stage_id <- getOption("kwallm__prompt_execution_stage", "unknown")
  stage_id <- as.character(stage_id %||% "unknown")
  if (!length(stage_id) || is.na(stage_id[[1]]) || !nzchar(stage_id[[1]])) {
    return("unknown")
  }

  stage_id[[1]]
}

.kwallm__prompt_execution_reset <- function() {
  options(kwallm__prompt_execution_records = list())
  invisible(NULL)
}

.kwallm__prompt_execution_record <- function(record) {
  if (!is.data.frame(record) || !nrow(record)) {
    return(invisible(NULL))
  }

  records <- getOption("kwallm__prompt_execution_records", list())
  options(kwallm__prompt_execution_records = c(records, list(record)))
  invisible(NULL)
}

.kwallm__prompt_execution_get <- function() {
  records <- getOption("kwallm__prompt_execution_records", list())

  if (!length(records)) {
    return(data.frame(
      prompt_id = character(),
      stage_id = character(),
      model_id = character(),
      started_at = character(),
      completed_at = character(),
      duration_ms = numeric(),
      attempt_count = integer(),
      retry_count = integer(),
      max_tries = integer(),
      retry_delay_seconds = numeric(),
      max_interactions = integer(),
      completion_status = character(),
      error_messages = character(),
      final_error_message = character(),
      stringsAsFactors = FALSE
    ))
  }

  unique(do.call(rbind, records))
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

.kwallm__prompt_trace_session_id <- function() {
  sid <- tryCatch(
    getOption("kwallm__log_session_id", NULL),
    error = function(e) NULL
  )
  if (!is.null(sid) && nzchar(as.character(sid))) {
    return(substr(as.character(sid), 1, 8))
  }

  "system"
}

.kwallm__prompt_trace_retention_files <- function() {
  # Number of prompt trace files to keep. NULL = keep indefinitely.
  retention <- getOption(
    "send_prompt_with_retries__prompt_trace_retention_files",
    NULL
  )
  if (is.null(retention)) {
    # Convenience fallback: reuse main logger retention if configured
    retention <- getOption("logger__retention", NULL)
  }
  if (is.null(retention)) {
    return(NULL)
  }
  if (!is.numeric(retention) || length(retention) != 1) {
    return(NULL)
  }
  retention <- as.integer(retention)
  if (is.na(retention) || retention <= 0) {
    return(NULL)
  }
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

.kwallm__prompt_trace_append <- function(
  lines,
  file_path = .kwallm__prompt_trace_file_path()
) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  cat(paste0(lines, collapse = "\n"), "\n", file = file_path, append = TRUE)
  .kwallm__prompt_trace_cleanup(file_path)
  invisible(NULL)
}

.kwallm__prompt_trace_cleanup <- function(file_path) {
  retention <- .kwallm__prompt_trace_retention_files()
  if (is.null(retention)) {
    return(invisible(NULL))
  }

  # Avoid doing filesystem scans too frequently.
  last <- getOption("kwallm__prompt_trace_last_cleanup", NULL)
  if (inherits(last, "POSIXct")) {
    if (as.numeric(difftime(Sys.time(), last, units = "secs")) < 3600) {
      return(invisible(NULL))
    }
  }
  options(kwallm__prompt_trace_last_cleanup = Sys.time())

  dir <- dirname(file_path)
  if (!dir.exists(dir)) {
    return(invisible(NULL))
  }

  # Only target our own trace files.
  trace_files <- list.files(
    dir,
    pattern = "^prompt_trace_.*\\.log$",
    full.names = TRUE,
    ignore.case = FALSE
  )
  if (length(trace_files) <= retention) {
    return(invisible(NULL))
  }

  # Prefer ordering by date embedded in filename (prompt_trace_YYYY-MM-DD.log),
  # since mtimes can be unreliable across platforms/filesystems.
  base <- basename(trace_files)
  date_str <- sub(
    "^prompt_trace_([0-9]{4}-[0-9]{2}-[0-9]{2})\\.log$",
    "\\1",
    base
  )
  parsed_date <- suppressWarnings(as.Date(date_str))
  parsed_date[is.na(parsed_date) | date_str == base] <- NA

  info <- tryCatch(file.info(trace_files), error = function(e) NULL)
  mtime <- if (!is.null(info) && !is.null(info$mtime)) {
    info$mtime
  } else {
    rep(as.POSIXct(NA), length(trace_files))
  }

  # Order old -> new (by parsed date, then mtime, then name)
  ord <- order(parsed_date, mtime, base, decreasing = FALSE, na.last = TRUE)
  sorted <- trace_files[ord]
  to_remove <- head(sorted, length(sorted) - retention)

  # Best-effort cleanup; must never crash.
  tryCatch(unlink(to_remove, force = TRUE), error = function(e) NULL)
  invisible(NULL)
}

.kwallm__prompt_trace_serialize <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }

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
        if (
          !is.null(prompt$messages) &&
            is.data.frame(prompt$messages) &&
            "content" %in% names(prompt$messages)
        ) {
          return(paste(prompt$messages$content, collapse = "\n\n"))
        }
        if (
          !is.null(prompt$chat_history) &&
            is.data.frame(prompt$chat_history) &&
            "content" %in% names(prompt$chat_history)
        ) {
          return(paste(prompt$chat_history$content, collapse = "\n\n"))
        }
      }
      .kwallm__prompt_trace_serialize(prompt)
    },
    error = function(e) {
      paste0("<failed-to-serialize-prompt: ", conditionMessage(e), ">")
    }
  )
}

.kwallm__prompt_trace_log_send <- function(
  prompt_id,
  model_name,
  attempt,
  max_tries,
  prompt_text,
  .ctx = NULL
) {
  # Use pre-resolved context from caller, or fall back to direct calls
  .enabled <- if (!is.null(.ctx)) {
    .ctx$enabled
  } else {
    .kwallm__prompt_trace_enabled_to_file
  }
  .append <- if (!is.null(.ctx)) .ctx$append else .kwallm__prompt_trace_append
  .session_id <- if (!is.null(.ctx)) {
    .ctx$session_id
  } else {
    .kwallm__prompt_trace_session_id
  }

  if (.enabled()) {
    tryCatch(
      .append(c(
        "---- PROMPT_SEND ----",
        paste0(
          "time_utc=",
          format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        ),
        paste0("session_id=", .session_id()),
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

.kwallm__prompt_trace_log_reply <- function(
  prompt_id,
  model_name,
  attempts,
  duration_ms,
  response_text,
  .ctx = NULL
) {
  # Use pre-resolved context from caller, or fall back to direct calls
  .enabled <- if (!is.null(.ctx)) {
    .ctx$enabled
  } else {
    .kwallm__prompt_trace_enabled_to_file
  }
  .append <- if (!is.null(.ctx)) .ctx$append else .kwallm__prompt_trace_append
  .session_id <- if (!is.null(.ctx)) {
    .ctx$session_id
  } else {
    .kwallm__prompt_trace_session_id
  }

  if (.enabled()) {
    tryCatch(
      .append(c(
        "---- RESPONSE_RECEIVED ----",
        paste0(
          "time_utc=",
          format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        ),
        paste0("session_id=", .session_id()),
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

.kwallm__prompt_trace_log_error <- function(
  prompt_id,
  model_name,
  attempt,
  max_tries,
  err_message,
  .ctx = NULL
) {
  # Use pre-resolved context from caller, or fall back to direct calls
  .enabled <- if (!is.null(.ctx)) {
    .ctx$enabled
  } else {
    .kwallm__prompt_trace_enabled_to_file
  }
  .append <- if (!is.null(.ctx)) .ctx$append else .kwallm__prompt_trace_append
  .session_id <- if (!is.null(.ctx)) {
    .ctx$session_id
  } else {
    .kwallm__prompt_trace_session_id
  }

  if (.enabled()) {
    tryCatch(
      .append(c(
        "---- CALL_ERROR ----",
        paste0(
          "time_utc=",
          format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
        ),
        paste0("session_id=", .session_id()),
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


# Helper for async globals ---------------------------------------------------

#' Get globals required for send_prompt_with_retries in async contexts
#'
#' When using `mirai::mirai()` with an explicit `.args = ...`,
#' the worker runs in a separate R process and will not automatically have
#' access to the internal helper functions used by `send_prompt_with_retries()`.
#'
#' Use this helper to consistently export these functions alongside
#' `send_prompt_with_retries` itself.
#'
#' @return A named list suitable for merging into an `.args =` list.
#' @export
send_prompt_with_retries_async_globals <- function() {
  list(
    send_prompt_with_retries = send_prompt_with_retries,
    .kwallm__prompt_trace_new_id = .kwallm__prompt_trace_new_id,
    .kwallm__prompt_trace_enabled_to_file = .kwallm__prompt_trace_enabled_to_file,
    .kwallm__prompt_trace_session_id = .kwallm__prompt_trace_session_id,
    .kwallm__prompt_trace_retention_files = .kwallm__prompt_trace_retention_files,
    .kwallm__prompt_trace_file_path = .kwallm__prompt_trace_file_path,
    .kwallm__prompt_trace_append = .kwallm__prompt_trace_append,
    .kwallm__prompt_trace_cleanup = .kwallm__prompt_trace_cleanup,
    .kwallm__prompt_trace_serialize = .kwallm__prompt_trace_serialize,
    .kwallm__prompt_trace_extract_prompt_text = .kwallm__prompt_trace_extract_prompt_text,
    .kwallm__prompt_trace_log_send = .kwallm__prompt_trace_log_send,
    .kwallm__prompt_trace_log_reply = .kwallm__prompt_trace_log_reply,
    .kwallm__prompt_trace_log_error = .kwallm__prompt_trace_log_error,
    .kwallm__prompt_execution_current_stage = .kwallm__prompt_execution_current_stage,
    .kwallm__prompt_execution_reset = .kwallm__prompt_execution_reset,
    .kwallm__prompt_execution_record = .kwallm__prompt_execution_record,
    .kwallm__prompt_execution_get = .kwallm__prompt_execution_get
  )
}
