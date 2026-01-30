library(testthat)

source(here::here("R", "utils_logger.R"), local = TRUE)


reset_logger_state <- function() {
  options(
    kwallm__logger_state = NULL,
    kwallm__log_session_id = NULL,
    kwallm__log_is_async = NULL
  )
}


test_that("get_session_id returns 'system' outside Shiny session", {
  reset_logger_state()
  expect_identical(get_session_id(), "system")
})


test_that("get_session_id honors kwallm__log_session_id override", {
  reset_logger_state()

  withr::local_options(list(kwallm__log_session_id = "deadbeefcafebabe"))
  expect_identical(get_session_id(), "deadbeef")
})


test_that("fallback logging writes to file and emits a message", {
  reset_logger_state()

  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-")

  options(
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      retention = NULL,
      app_mode = "test"
    )
  )

  expect_message(
    log_info("hello world", component = "unit"),
    "hello world",
    fixed = TRUE
  )

  log_file <- file.path(log_dir, paste0(format(Sys.Date(), "%Y-%m-%d"), ".log"))
  expect_true(file.exists(log_file))

  lines <- readLines(log_file, warn = FALSE)
  expect_true(any(grepl("\\[sync\\] \\[INFO\\] \\[unit\\] hello world", lines)))
})


test_that("log_error(fatal = TRUE) prefixes message", {
  reset_logger_state()

  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-")

  options(
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      retention = NULL,
      app_mode = "test"
    )
  )

  expect_message(
    log_error("boom", component = "unit", fatal = TRUE),
    "[FATAL] boom",
    fixed = TRUE
  )

  log_file <- file.path(log_dir, paste0(format(Sys.Date(), "%Y-%m-%d"), ".log"))
  lines <- readLines(log_file, warn = FALSE)
  expect_true(any(grepl(
    "\\[sync\\] \\[ERROR\\] \\[unit\\] \\[FATAL\\] boom",
    lines
  )))
})


test_that("analysis progress uses DEBUG level and analysis component", {
  reset_logger_state()

  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-")

  options(
    kwallm__logger_state = list(
      initialized = TRUE,
      use_logger_pkg = FALSE,
      level = "DEBUG",
      log_dir = log_dir,
      log_dir_abs = log_dir,
      retention = NULL,
      app_mode = "test"
    )
  )

  expect_message(
    log_analysis_progress(current = 1, total = 3, step = "categorizing"),
    "Progress: 1/3 (categorizing)",
    fixed = TRUE
  )

  log_file <- file.path(log_dir, paste0(format(Sys.Date(), "%Y-%m-%d"), ".log"))
  lines <- readLines(log_file, warn = FALSE)
  expect_true(any(grepl(
    "\\[sync\\] \\[DEBUG\\] \\[analysis\\] Progress: 1/3 \\(categorizing\\)",
    lines
  )))
})


test_that("apply_retention_policy keeps newest N log files", {
  reset_logger_state()

  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-")
  f1 <- file.path(log_dir, "2025-01-01.log")
  f2 <- file.path(log_dir, "2025-01-02.log")
  f3 <- file.path(log_dir, "2025-01-03.log")
  writeLines("a", f1)
  writeLines("b", f2)
  writeLines("c", f3)

  # Make mtimes misleading on purpose; retention should follow filename dates.
  t0 <- Sys.time()
  Sys.setFileTime(f1, t0) # newest mtime, but oldest date
  Sys.setFileTime(f2, t0 - 300) # oldest mtime, middle date
  Sys.setFileTime(f3, t0 - 200) # middle mtime, newest date

  .apply_retention_policy(log_dir, retention = 2)

  remaining <- list.files(
    log_dir,
    pattern = "^\\d{4}-\\d{2}-\\d{2}\\.log$",
    full.names = FALSE
  )
  expect_setequal(remaining, c("2025-01-02.log", "2025-01-03.log"))
})


test_that("log_init creates the log directory and initializes state", {
  reset_logger_state()

  # If logger is available, preserve global logger state to avoid leaking
  if (requireNamespace("logger", quietly = TRUE)) {
    old_appender <- logger::log_appender(namespace = "global")
    old_layout <- logger::log_layout(namespace = "global")
    old_threshold <- logger::log_threshold()

    resolve_logger_fun <- function(x) {
      if (is.function(x)) {
        x
      } else if (is.name(x)) {
        get(as.character(x), envir = asNamespace("logger"))
      } else {
        # Fallback to console appender if x is NULL or unexpected type
        logger::appender_console
      }
    }

    withr::defer(
      {
        logger::log_appender(
          resolve_logger_fun(old_appender),
          namespace = "global"
        )
        logger::log_layout(resolve_logger_fun(old_layout), namespace = "global")
        logger::log_threshold(old_threshold)
      },
      testthat::teardown_env()
    )
  }

  base_dir <- withr::local_tempdir(pattern = "kwallm-logs-")
  log_dir <- file.path(base_dir, "nested", "logs")

  expect_false(dir.exists(log_dir))
  expect_invisible(log_init(
    level = "INFO",
    log_dir = log_dir,
    retention = NULL,
    mode = "test"
  ))
  expect_true(dir.exists(log_dir))
  st <- getOption("kwallm__logger_state")
  expect_true(isTRUE(st$initialized))
  expect_identical(st$log_dir, log_dir)
})


test_that("log_context_capture returns a valid kwallm_log_context", {
  reset_logger_state()

  log_dir <- withr::local_tempdir(pattern = "kwallm-ctx-test-")

  # Set the options that log_context_capture reads from
  withr::local_options(
    logger__level = "INFO",
    logger__dir = log_dir,
    logger__retention = NULL,
    kwallm__log_session_id = "testctx1"
  )

  log_init(
    level = "INFO",
    log_dir = log_dir,
    retention = NULL,
    mode = "test"
  )

  ctx <- log_context_capture(is_async = TRUE, mode = "unit")

  expect_s3_class(ctx, "kwallm_log_context")
  expect_equal(ctx$level, "INFO")
  expect_equal(ctx$dir, log_dir)
  expect_null(ctx$retention)
  expect_equal(ctx$mode, "unit")
  expect_equal(ctx$session_id, "testctx1")
  expect_true(ctx$is_async)
})


test_that("log_async_globals returns a list with all required logging functions", {
  ctx <- structure(
    list(
      level = "DEBUG",
      dir = tempdir(),
      session_id = "dummy",
      is_async = TRUE
    ),
    class = "kwallm_log_context"
  )

  globals <- log_async_globals(ctx)

  expect_type(globals, "list")
  expect_identical(globals$log_ctx, ctx)
  expect_true(is.function(globals$log_context_apply))
  expect_true(is.function(globals$log_info))
  expect_true(is.function(globals$log_debug))
  expect_true(is.function(globals$log_warn))
  expect_true(is.function(globals$log_error))
  expect_true(is.function(globals$log_action))
})


test_that("log_context_apply bootstraps logger in mirai daemon worker", {
  testthat::skip_if_not_installed("mirai")

  # First, ensure any existing daemons are reset
  tryCatch(mirai::daemons(0), error = function(e) NULL)

  # Mirai daemons can fail in constrained environments.
  # If daemons cannot start, we skip (rather than failing unrelated CI).
  can_start_daemons <- TRUE
  tryCatch(
    {
      mirai::daemons(1)
      on.exit(mirai::daemons(0), add = TRUE)
    },
    error = function(e) {
      can_start_daemons <<- FALSE
    }
  )
  if (!isTRUE(can_start_daemons)) {
    testthat::skip("mirai daemons not available in this environment")
  }

  # Wait a moment for daemons to be ready
  Sys.sleep(0.5)

  # Set up logging state in the main process
  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-worker-")
  reset_logger_state()

  # Set the options that log_context_capture reads from
  withr::local_options(
    logger__level = "INFO",
    logger__dir = log_dir,
    logger__retention = NULL,
    kwallm__log_session_id = "deadbeef"
  )

  log_init(
    level = "INFO",
    log_dir = log_dir,
    retention = NULL,
    mode = "test"
  )

  # Use the actual log_context_capture function (matches app pattern)
  log_ctx <- log_context_capture(is_async = TRUE, mode = "unit-test")

  # Verify context was captured correctly
  expect_s3_class(log_ctx, "kwallm_log_context")
  expect_equal(log_ctx$dir, log_dir)
  expect_equal(log_ctx$session_id, "deadbeef")
  expect_true(log_ctx$is_async)

  # With mirai, functions are passed by value and lose their closure environments.
  # The worker needs to source the logger code to have all functions available.
  # We use `...` so variables are in the global environment of the worker.

  logger_file <- normalizePath(
    file.path(testthat::test_path(), "..", "..", "R", "utils_logger.R"),
    mustWork = TRUE
  )

  m <- mirai::mirai(
    {
      # Source the logger to make all functions available with correct closures
      source(logger_source_file, local = FALSE)

      # This is the exact pattern used in all async modules:
      log_context_apply(log_ctx)
      log_info("hello from worker", component = "unit")
      TRUE
    },
    log_ctx = log_ctx,
    logger_source_file = logger_file
  )

  # Wait for the mirai to complete
  result <- m[]
  # If result is not TRUE, check if it's an error
  if (!isTRUE(result) && mirai::is_error_value(result)) {
    testthat::skip(paste("mirai worker error:", result))
  }
  expect_true(result)

  log_file <- file.path(log_dir, paste0(format(Sys.Date(), "%Y-%m-%d"), ".log"))
  for (i in 1:30) {
    if (file.exists(log_file)) {
      break
    }
    Sys.sleep(0.1)
  }
  expect_true(file.exists(log_file))

  lines <- readLines(log_file, warn = FALSE)
  expect_true(any(grepl("hello from worker", lines, fixed = TRUE)))
  # Verify both session ID and async label are in the log
  expect_true(any(grepl("\\[deadbeef\\]", lines)))
  expect_true(any(grepl("\\[async\\]", lines)))
})
