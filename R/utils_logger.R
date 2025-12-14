#' Application Logger
#'
#' Provides structured logging for the KWALLM application.
#' Logs app startup, session lifecycle, analysis progress, and errors.
#'
#' @details
#' Uses the `logger` package for robust logging infrastructure.
#' Logs are written to both console and files in the `logs/` directory.
#'
#' Configuration options (set via `options()`):
#' - `logger__level`: Log level ("DEBUG", "INFO", "WARN", "ERROR"). Default: "INFO"
#' - `logger__dir`: Log directory. Default: "logs"
#' - `logger__retention`: Number of log files to keep, or NULL for indefinite. Default: NULL
#'
#' @examples
#' # Initialize logger (typically called in load_dependencies.R)
#' log_init()
#'
#' # Log messages at different levels
#' log_info("Application started", component = "startup")
#' log_debug("Processing text 1 of 100", component = "processing")
#' log_warn("API rate limit approaching", component = "llm")
#' log_error("Failed to connect to LLM", component = "llm")
#'
#' # Session logging
#' log_session_start("abc123")
#' log_session_end("abc123")
#'
#' # Analysis logging
#' log_analysis_start(mode = "Categorisatie", n_texts = 50, model = "gpt-4")
#' log_analysis_progress(current = 10, total = 50, step = "categorizing")
#' log_analysis_complete(mode = "Categorisatie", duration_secs = 120, n_texts = 50)

# 1 Internal state ------------------------------------------------------------

# Store logger state in an environment to avoid global variables
.logger_env <- new.env(parent = emptyenv())
.logger_env$initialized <- FALSE
.logger_env$log_dir <- NULL


# 2 Initialization ------------------------------------------------------------

#' Initialize the application logger
#'
#' Sets up logging to console and file. Should be called once at app startup.
#'
#' @param level Log level: "DEBUG", "INFO", "WARN", or "ERROR"
#' @param log_dir Directory for log files
#' @param retention Number of log files to keep (NULL = indefinite)
#' @param mode App mode ("regular", "docker", "electron") - included in startup log
#'
#' @return Invisible NULL
#' @export
log_init <- function(
  level = getOption("logger__level", "INFO"),
  log_dir = getOption("logger__dir", "logs"),
  retention = getOption("logger__retention", NULL),
  mode = "unknown"
) {
  log_dir_abs <- tryCatch(
    normalizePath(log_dir, winslash = "/", mustWork = FALSE),
    error = function(e) log_dir
  )

  # Create log directory if needed
  if (!dir.exists(log_dir_abs)) {
    dir.create(log_dir_abs, recursive = TRUE, showWarnings = FALSE)
  }

  .logger_env$log_dir <- log_dir
  .logger_env$log_dir_abs <- log_dir_abs
  .logger_env$retention <- retention
  .logger_env$app_mode <- mode

  # Check if logger package is available
  if (requireNamespace("logger", quietly = TRUE)) {
    # Set log level
    log_level <- switch(
      toupper(level),
      "DEBUG" = logger::DEBUG,
      "INFO" = logger::INFO,
      "WARN" = logger::WARN,
      "ERROR" = logger::ERROR,
      logger::INFO
    )

    logger::log_threshold(log_level)

    # Set up file appender with daily rotation
    log_file <- file.path(
      log_dir_abs,
      paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
    )

    logger::log_appender(
      logger::appender_tee(log_file),
      namespace = "global"
    )

    # Structured layout (with timezone and Shiny session id)
    logger::log_layout(
      function(
        level,
        msg,
        namespace = NA,
        .logcall = sys.call(),
        .topcall = sys.call(-1),
        .topenv = parent.frame(),
        .timestamp = Sys.time()
      ) {
        session_id <- tryCatch(get_session_id(), error = function(e) "system")
        sprintf(
          "[%s] [%s] [%s] [%s] %s",
          format(.timestamp, "%Y-%m-%d %H:%M:%S%z"),
          session_id,
          level,
          namespace,
          msg
        )
      },
      namespace = "global"
    )

    .logger_env$initialized <- TRUE
    .logger_env$use_logger_pkg <- TRUE
  } else {
    # Fallback to simple file logging
    .logger_env$initialized <- TRUE
    .logger_env$use_logger_pkg <- FALSE
    .logger_env$level <- toupper(level)
  }

  # Apply retention policy (clean old logs)
  if (!is.null(retention) && is.numeric(retention) && retention > 0) {
    .apply_retention_policy(log_dir_abs, retention)
  }

  invisible(NULL)
}


#' Get current app mode safely
#' @keywords internal
get_app_mode <- function() {
  mode <- NULL
  if (exists("app_mode", envir = .logger_env, inherits = FALSE)) {
    mode <- .logger_env$app_mode
  }

  if (
    !is.null(mode) && is.character(mode) && length(mode) == 1 && nzchar(mode)
  ) {
    return(mode)
  }

  opt <- getOption("app__mode", NULL)
  if (!is.null(opt) && is.character(opt) && length(opt) == 1 && nzchar(opt)) {
    return(opt)
  }

  env <- Sys.getenv("KWALLM_APP_MODE", "")
  if (nzchar(env)) {
    return(env)
  }

  # Fallback heuristic: detect Docker on Linux
  if (.Platform$OS.type == "unix" && file.exists("/.dockerenv")) {
    return("docker")
  }

  return("unknown")
}


#' Get current session ID safely
#' @keywords internal
get_session_id <- function() {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session) && !is.null(session$token)) {
    return(substr(session$token, 1, 8))
  }
  return("system")
}


#' Apply log retention policy
#'
#' @param log_dir Log directory
#' @param retention Number of files to keep
#' @keywords internal
.apply_retention_policy <- function(log_dir, retention) {
  log_files <- list.files(
    log_dir,
    pattern = "^\\d{4}-\\d{2}-\\d{2}\\.log$",
    full.names = TRUE
  )

  if (length(log_files) > retention) {
    # Sort by modification time (oldest first)
    file_info <- file.info(log_files)
    sorted_files <- log_files[order(file_info$mtime)]

    # Remove oldest files
    files_to_remove <- head(sorted_files, length(log_files) - retention)
    file.remove(files_to_remove)
  }
}


# 3 Core logging functions ----------------------------------------------------

#' Write a log message (internal)
#'
#' @param level Log level
#' @param message Log message
#' @param component Component name for namespacing
#' @keywords internal
.write_log <- function(level, message, component = "app") {
  if (!.logger_env$initialized) {
    # Auto-initialize with defaults if not done
    log_init()
  }

  if (.logger_env$use_logger_pkg) {
    # Use logger package
    log_dir_abs <- NULL
    if (exists("log_dir_abs", envir = .logger_env, inherits = FALSE)) {
      log_dir_abs <- .logger_env$log_dir_abs
    }
    if (
      !is.null(log_dir_abs) &&
        is.character(log_dir_abs) &&
        length(log_dir_abs) == 1 &&
        nzchar(log_dir_abs)
    ) {
      if (!dir.exists(log_dir_abs)) {
        dir.create(log_dir_abs, recursive = TRUE, showWarnings = FALSE)
      }
    }

    log_fn <- switch(
      toupper(level),
      "DEBUG" = logger::log_debug,
      "INFO" = logger::log_info,
      "WARN" = logger::log_warn,
      "ERROR" = logger::log_error,
      logger::log_info
    )
    log_fn("{message}", namespace = component)
  } else {
    # Fallback: simple file logging
    levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
    current_level <- levels[.logger_env$level]
    msg_level <- levels[toupper(level)]

    if (is.na(msg_level)) {
      msg_level <- 2
    }
    if (is.na(current_level)) {
      current_level <- 2
    }

    if (msg_level >= current_level) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S%z")
      session_id <- get_session_id()
      log_line <- sprintf(
        "[%s] [%s] [%s] [%s] %s",
        timestamp,
        session_id,
        level,
        component,
        message
      )

      # Write to file
      log_file <- file.path(
        .logger_env$log_dir,
        paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
      )
      cat(log_line, "\n", file = log_file, append = TRUE)

      # Also print to console
      message(log_line)
    }
  }
}


#' Log an INFO message
#'
#' @param message The message to log
#' @param component Component name (e.g., "startup", "session", "processing")
#' @export
log_info <- function(message, component = "app") {
  .write_log("INFO", message, component)
}


#' Log a DEBUG message
#'
#' @param message The message to log
#' @param component Component name
#' @export
log_debug <- function(message, component = "app") {
  .write_log("DEBUG", message, component)
}


#' Log a WARN message
#'
#' @param message The message to log
#' @param component Component name
#' @export
log_warn <- function(message, component = "app") {
  .write_log("WARN", message, component)
}


#' Log an ERROR message
#'
#' @param message The message to log
#' @param component Component name
#' @param fatal Whether this is a fatal error
#' @export
log_error <- function(message, component = "app", fatal = FALSE) {
  level <- if (fatal) "ERROR" else "ERROR"
  prefix <- if (fatal) "[FATAL] " else ""
  .write_log(level, paste0(prefix, message), component)
}


# 4 Session logging -----------------------------------------------------------

#' Log session start
#'
#' @param session_id The Shiny session token/ID
#' @export
log_session_start <- function(session_id) {
  log_info(
    sprintf(
      "Session started: %s app_mode=%s",
      substr(session_id, 1, 8),
      get_app_mode()
    ),
    component = "session"
  )
}


#' Log session end
#'
#' @param session_id The Shiny session token/ID
#' @export
log_session_end <- function(session_id) {
  log_info(
    sprintf("Session ended: %s", substr(session_id, 1, 8)),
    component = "session"
  )
}


# 5 Analysis logging ----------------------------------------------------------

#' Log analysis start
#'
#' @param mode Analysis mode (e.g., "Categorisatie", "Scoren", "Onderwerpextractie")
#' @param n_texts Number of texts to process
#' @param model Model name being used
#' @export
log_analysis_start <- function(mode, n_texts, model) {
  log_info(
    sprintf(
      "Analysis started: mode=%s, texts=%d, model=%s",
      mode,
      n_texts,
      model
    ),
    component = "analysis"
  )
}


#' Log analysis progress
#'
#' @param current Current item number
#' @param total Total items
#' @param step Current step name
#' @export
log_analysis_progress <- function(current, total, step = "processing") {
  log_debug(
    sprintf("Progress: %d/%d (%s)", current, total, step),
    component = "analysis"
  )
}


#' Log analysis completion
#'
#' @param mode Analysis mode
#' @param duration_secs Duration in seconds
#' @param n_texts Number of texts processed
#' @param success Whether analysis completed successfully
#' @export
log_analysis_complete <- function(
  mode,
  duration_secs,
  n_texts,
  success = TRUE
) {
  status <- if (success) "completed" else "failed"
  log_info(
    sprintf(
      "Analysis %s: mode=%s, texts=%d, duration=%.1fs",
      status,
      mode,
      n_texts,
      duration_secs
    ),
    component = "analysis"
  )
}


#' Log analysis interruption
#'
#' @param mode Analysis mode
#' @param reason Reason for interruption
#' @export
log_analysis_interrupted <- function(mode, reason = "user requested") {
  log_warn(
    sprintf("Analysis interrupted: mode=%s, reason=%s", mode, reason),
    component = "analysis"
  )
}


# 6 Action logging ------------------------------------------------------------

#' Log a user action
#'
#' @param action Action name
#' @param details Additional details
#' @export
log_action <- function(action, details = NULL) {
  msg <- if (!is.null(details)) {
    sprintf("Action: %s (%s)", action, details)
  } else {
    sprintf("Action: %s", action)
  }
  log_debug(msg, component = "action")
}
