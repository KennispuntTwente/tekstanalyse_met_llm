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

  options(kwallm__logger_state = list(
    initialized = TRUE,
    use_logger_pkg = FALSE,
    level = "DEBUG",
    log_dir = log_dir,
    log_dir_abs = log_dir,
    retention = NULL,
    app_mode = "test"
  ))

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

  options(kwallm__logger_state = list(
    initialized = TRUE,
    use_logger_pkg = FALSE,
    level = "DEBUG",
    log_dir = log_dir,
    log_dir_abs = log_dir,
    retention = NULL,
    app_mode = "test"
  ))

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

  options(kwallm__logger_state = list(
    initialized = TRUE,
    use_logger_pkg = FALSE,
    level = "DEBUG",
    log_dir = log_dir,
    log_dir_abs = log_dir,
    retention = NULL,
    app_mode = "test"
  ))

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

  # Ensure ordering by modification time (oldest first)
  t0 <- Sys.time()
  Sys.setFileTime(f1, t0 - 300)
  Sys.setFileTime(f2, t0 - 200)
  Sys.setFileTime(f3, t0 - 100)

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
      if (is.name(x)) {
        get(as.character(x), envir = asNamespace("logger"))
      } else {
        x
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


test_that("log_context_apply bootstraps logger in multisession future worker", {
  testthat::skip_if_not_installed("future")

  # Multisession plans can fail in constrained environments.
  # If a parallel plan cannot start, we skip (rather than failing unrelated CI).
  can_multisession <- TRUE
  tryCatch(
    {
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      future::plan(future::multisession)
    },
    error = function(e) {
      can_multisession <<- FALSE
    }
  )
  if (!isTRUE(can_multisession)) {
    testthat::skip("future::multisession not available in this environment")
  }

  log_dir <- withr::local_tempdir(pattern = "kwallm-logs-worker-")
  session_id <- "deadbeef"
  log_ctx <- structure(
    list(
      level = "INFO",
      dir = log_dir,
      retention = NULL,
      mode = "test",
      session_id = session_id,
      is_async = TRUE
    ),
    class = "kwallm_log_context"
  )

  f <- future::future(
    {
      log_context_apply(log_ctx)
      log_info("hello from worker", component = "unit")
      TRUE
    },
    globals = list(
      log_info = log_info,
      log_context_apply = log_context_apply,
      log_ctx = log_ctx
    ),
    seed = NULL
  )

  expect_true(future::value(f))

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
  expect_true(any(grepl("\\[deadbeef\\] \\[async\\]", lines)))
})
