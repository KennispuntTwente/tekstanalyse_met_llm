library(testthat)
library(shiny)
library(htmltools)

# Stub UI side effects.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(...) invisible(NULL)

source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)
source(here::here("R", "utils_logger.R"), local = TRUE)
source(here::here("R", "utils_app_error.R"), local = TRUE)

make_translator <- function(lang_code = "nl") {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  tr$set_translation_language(lang_code)
  tr
}

make_fake_session <- function() {
  closed <- FALSE
  list(
    token = "deadbeefcafebabe",
    close = function() {
      closed <<- TRUE
    },
    is_closed = function() closed
  )
}


test_that("app_error: nonfatal logs to nonfatal folder and does not close session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})


test_that("app_error: fatal logs to fatal folder and closes session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_true(sess$is_closed())
})


test_that("app_error: with NULL session stops after logging", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  expect_error(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = NULL,
      lang = make_translator("nl")
    ),
    "boom",
    fixed = TRUE
  )
})


test_that("app_error shows condition messages without call-object wrappers", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()
  provider_error <- paste0(
    "Invalid parameter: 'response_format' of type 'json_schema' ",
    "is not supported with this model."
  )
  wrapped_error <- structure(
    list(
      message = provider_error,
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )

  output <- capture.output(app_error(
    wrapped_error,
    when = "main processing",
    fatal = FALSE,
    shiny_session = sess,
    lang = make_translator("nl")
  ))

  expect_match(paste(output, collapse = "\n"), provider_error, fixed = TRUE)
  expect_false(any(grepl("<simpleError", output, fixed = TRUE)))
  expect_false(any(grepl("onFulfilled", output, fixed = TRUE)))
})


test_that("app_error writes complete marking failure context to the log file", {
  log_dir <- withr::local_tempdir(pattern = "kwallm-app-error-logs-")
  withr::local_options(
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      retention = NULL,
      app_mode = "test"
    ),
    kwallm__log_session_id = "deadbeef"
  )

  provider_error <- paste0(
    "Invalid parameter: 'response_format' of type 'json_schema' ",
    "is not supported with this model."
  )
  marking_error <- paste0(
    "Marking failed for analysis_unit_id=17, chunk_id=4, ",
    "chunk_index=2, code='Housing'.\nProvider error: ",
    provider_error
  )
  wrapped_error <- structure(
    list(
      message = marking_error,
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )
  sess <- make_fake_session()

  suppressMessages(capture.output(app_error(
    wrapped_error,
    when = "main processing of marking",
    fatal = TRUE,
    shiny_session = sess,
    lang = make_translator("nl")
  )))
  expect_true(sess$is_closed())

  log_file <- file.path(
    log_dir,
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))

  log_lines <- readLines(log_file, warn = FALSE)
  error_lines <- log_lines[grepl(
    "[ERROR] [error]",
    log_lines,
    fixed = TRUE
  )]

  expect_length(error_lines, 1L)
  expect_match(
    error_lines,
    "^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}[+-]\\d{4}\\]"
  )
  expect_match(error_lines, "[deadbeef] [sync] [ERROR] [error]", fixed = TRUE)
  expect_match(error_lines, "[FATAL] Error occurred:", fixed = TRUE)
  expect_match(error_lines, "analysis_unit_id=17", fixed = TRUE)
  expect_match(error_lines, "chunk_id=4", fixed = TRUE)
  expect_match(error_lines, "chunk_index=2", fixed = TRUE)
  expect_match(error_lines, "code='Housing'", fixed = TRUE)
  expect_match(error_lines, provider_error, fixed = TRUE)
  expect_match(error_lines, "When: main processing of marking", fixed = TRUE)
  expect_match(error_lines, "Session ID: deadbeef", fixed = TRUE)
  expect_false(grepl("<simpleError", error_lines, fixed = TRUE))
  expect_false(grepl("onFulfilled", error_lines, fixed = TRUE))
})


test_that("app error messages retain the deepest purrr cause without a backtrace", {
  indexed_error <- tryCatch(
    purrr::imap(
      list(batch_one = "text"),
      function(value, name) {
        force(value)
        force(name)
        stop("PROVIDER_ERROR_SENTINEL", call. = FALSE)
      }
    ),
    error = identity
  )

  message <- kwallm_error_message(indexed_error)

  expect_match(message, "PROVIDER_ERROR_SENTINEL", fixed = TRUE)
  expect_match(message, "batch_one", fixed = TRUE)
  expect_false(grepl("Backtrace:", message, fixed = TRUE))
})


test_that("app_error: downgrades legacy interrupt transport error from fatal to nonfatal", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  ipc_err <- simpleError("Cannot pop from destroyed TextFileSource")
  expect_output(
    app_error(
      ipc_err,
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})


test_that("app_error: downgrades local async interrupt error from fatal to nonfatal", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  interrupt_err <- structure(
    list(message = "user cancelled"),
    class = c("kwallm_async_interrupt", "error", "condition")
  )

  expect_output(
    app_error(
      interrupt_err,
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})
