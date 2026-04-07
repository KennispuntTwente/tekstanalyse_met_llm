library(testthat)

test_that("kwallm_worker_app_root normalizes to an absolute path", {
  expect_identical(
    kwallm_worker_app_root(here::here()),
    normalizePath(here::here(), winslash = "/", mustWork = TRUE)
  )
})

test_that("kwallm_worker_source errors when the app R directory is missing", {
  missing_root <- tempfile("kwallm-worker-missing-")
  dir.create(missing_root)

  expect_error(
    kwallm_worker_source(missing_root),
    "Async worker bootstrap could not find the R directory"
  )
})

test_that("kwallm_worker_capture_options keeps only configured worker options", {
  old_opts <- options(
    app__mode = "test",
    logger__level = "DEBUG",
    send_prompt_with_retries__max_tries = 5L,
    kwallm__worker_task = "ignore-me"
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  captured <- kwallm_worker_capture_options()

  expect_identical(captured$app__mode, "test")
  expect_identical(captured$logger__level, "DEBUG")
  expect_identical(captured$send_prompt_with_retries__max_tries, 5L)
  expect_false("kwallm__worker_task" %in% names(captured))
})

test_that("kwallm_worker_bootstrap_globals embeds a self-contained helper chain", {
  helper_env <- new.env(parent = emptyenv())
  helper_env$kwallm_worker_app_root <- function(path = ".") path
  helper_env$kwallm_worker_load_core_packages <- function(...) invisible(NULL)
  helper_env$kwallm_worker_source <- function(
    app_root = helper_env$kwallm_worker_app_root(),
    env = parent.frame()
  ) {
    force(app_root)
    force(env)
    invisible(NULL)
  }
  helper_env$kwallm_worker_bootstrap <- kwallm_worker_bootstrap

  globals <- kwallm_worker_bootstrap_globals(env = helper_env)

  expect_named(globals, "kwallm_worker_bootstrap")
  expect_true(is.function(globals$kwallm_worker_bootstrap))

  embedded_source <- get(
    "kwallm_worker_source",
    envir = environment(globals$kwallm_worker_bootstrap),
    inherits = FALSE
  )

  expect_true(is.function(embedded_source))
  expect_true(exists(
    "kwallm_worker_app_root",
    envir = environment(embedded_source),
    inherits = FALSE
  ))
  expect_true(exists(
    "kwallm_worker_load_core_packages",
    envir = environment(embedded_source),
    inherits = FALSE
  ))
})

test_that("kwallm_worker_bootstrap applies worker options and log context", {
  helper_env <- new.env(parent = emptyenv())
  helper_env$kwallm_worker_app_root <- function(path = ".") path
  helper_env$kwallm_worker_load_core_packages <- function(...) invisible(NULL)
  helper_env$kwallm_worker_source <- function(
    app_root = helper_env$kwallm_worker_app_root(),
    env = parent.frame()
  ) {
    env$loaded_app_root <- app_root
    env$log_context_apply <- function(ctx) {
      assign("applied_log_context", ctx, envir = env)
      invisible(NULL)
    }

    invisible(app_root)
  }
  helper_env$kwallm_worker_bootstrap <- kwallm_worker_bootstrap

  globals <- kwallm_worker_bootstrap_globals(env = helper_env)
  worker_env <- new.env(parent = baseenv())

  old_opts <- options(
    app__mode = NULL,
    paragraph_streaming = TRUE,
    kwallm__worker_task = NULL
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  globals$kwallm_worker_bootstrap(
    task = "demo-task",
    app_root = "app-root",
    worker_options = list(
      app__mode = "unit-test",
      paragraph_streaming = FALSE
    ),
    log_context = list(session_id = "ctx-1"),
    env = worker_env
  )

  expect_identical(worker_env$loaded_app_root, "app-root")
  expect_identical(worker_env$applied_log_context, list(session_id = "ctx-1"))
  expect_identical(getOption("app__mode"), "unit-test")
  expect_identical(getOption("paragraph_streaming"), FALSE)
  expect_identical(getOption("kwallm__worker_task"), "demo-task")
})
