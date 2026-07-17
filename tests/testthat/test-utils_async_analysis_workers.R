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
    mori__enabled = FALSE,
    mori__max_mb = 64,
    logger__level = "DEBUG",
    send_prompt_with_retries__max_tries = 5L,
    topic_modelling__reduction_max_prompt_batches = 24L,
    topic_modelling__reduction_max_iterations = 3L,
    kwallm__worker_task = "ignore-me"
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  captured <- kwallm_worker_capture_options()

  expect_identical(captured$app__mode, "test")
  expect_identical(captured$mori__enabled, FALSE)
  expect_identical(captured$mori__max_mb, 64)
  expect_identical(captured$logger__level, "DEBUG")
  expect_identical(captured$send_prompt_with_retries__max_tries, 5L)
  expect_identical(captured$topic_modelling__reduction_max_prompt_batches, 24L)
  expect_identical(captured$topic_modelling__reduction_max_iterations, 3L)
  expect_false("kwallm__worker_task" %in% names(captured))
})


test_that("kwallm_mori_max_mb prefers env var and disables non-positive caps", {
  expect_identical(
    kwallm_mori_max_mb(
      get_option = function(name, default = NULL) 32,
      getenv = function(name, unset = "") "48"
    ),
    48
  )

  expect_null(
    kwallm_mori_max_mb(
      get_option = function(name, default = NULL) 32,
      getenv = function(name, unset = "") "0"
    )
  )

  expect_identical(
    kwallm_mori_max_mb(
      get_option = function(name, default = NULL) 32,
      getenv = function(name, unset = "") NA_character_
    ),
    32
  )
})


test_that("kwallm_mori_total_max_mb provides a bounded default", {
  expect_identical(
    kwallm_mori_total_max_mb(
      get_option = function(name, default = NULL) default,
      getenv = function(name, unset = "") unset
    ),
    512
  )
  expect_identical(
    kwallm_mori_total_max_mb(
      get_option = function(name, default = NULL) 512,
      getenv = function(name, unset = "") "128"
    ),
    128
  )
})


test_that("kwallm_worker_load_core_packages does not require mori", {
  loaded_packages <- character()

  kwallm_worker_load_core_packages(
    packages = c("mirai", "promises"),
    require_namespace = function(pkg, quietly = TRUE) TRUE,
    library_fn = function(package, character.only = FALSE) {
      loaded_packages <<- c(loaded_packages, package)
      invisible(TRUE)
    }
  )

  expect_identical(loaded_packages, c("mirai", "promises"))
})


test_that("kwallm_mori_share_worker_payload falls back when disabled", {
  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("a", "b")),
    enabled = FALSE
  )

  expect_s3_class(payload, "kwallm_mori_worker_payload")
  expect_identical(payload$args$texts, c("a", "b"))
  expect_identical(payload$guard, list())
  expect_null(payload$scope_key)
  expect_identical(payload$shared_names, character())
})


test_that("kwallm_mori_share_worker_payload respects the configured size cap", {
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  fake_share <- function(x) {
    structure(x, shared_name = paste0("shared-", paste(x, collapse = "-")))
  }
  fake_shared_name <- function(x) attr(x, "shared_name")

  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("alpha", "beta"), ids = 1:3),
    keys = c("texts", "ids"),
    enabled = TRUE,
    max_mb = 1,
    object_size = function(x) {
      if (is.character(x)) {
        return(2 * 1024^2)
      }

      256 * 1024
    },
    share_fn = fake_share,
    shared_name_fn = fake_shared_name
  )

  expect_false(kwallm_mori_is_ref(payload$args$texts))
  expect_true(kwallm_mori_is_ref(payload$args$ids))
  expect_named(payload$shared_names, "ids")
  expect_length(payload$guard, 1)
})


test_that("mori aggregate budget rejects and later admits shared payloads", {
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  budget_state <- new.env(parent = emptyenv())
  budget_state$used_bytes <- 0
  fake_share <- function(x) structure(x, shared_name = paste0("shared-", x))
  fake_shared_name <- function(x) attr(x, "shared_name")
  share_payload <- function() {
    kwallm_mori_share_worker_payload(
      list(texts = "payload"),
      enabled = TRUE,
      total_max_mb = 1,
      object_size = function(x) 700 * 1024,
      share_fn = fake_share,
      shared_name_fn = fake_shared_name,
      budget_state = budget_state
    )
  }

  first <- share_payload()
  expect_true(kwallm_mori_is_ref(first$args$texts))
  expect_equal(budget_state$used_bytes, 700 * 1024)

  second <- share_payload()
  expect_false(kwallm_mori_is_ref(second$args$texts))
  expect_equal(budget_state$used_bytes, 700 * 1024)

  kwallm_mori_release_guard(first$guard)
  kwallm_mori_release_guard(first$guard)
  expect_identical(budget_state$used_bytes, 0)

  third <- share_payload()
  expect_true(kwallm_mori_is_ref(third$args$texts))
  kwallm_mori_release_guard(third$guard)
  expect_identical(budget_state$used_bytes, 0)
})


test_that("kwallm_mori_share_worker_payload maps refs by signed capability", {
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("alpha", "beta"), untouched = list(env = new.env())),
    keys = "texts",
    enabled = TRUE
  )

  expect_true(kwallm_mori_is_ref(payload$args$texts))
  expect_false(kwallm_mori_is_ref(payload$args$untouched))
  expect_length(payload$guard, 1)
  expect_true(kwallm_mori_validate_scope_key(payload$scope_key))
  expect_named(payload$shared_names, "texts")
  expect_null(payload$args$texts$scope_key)
  expect_match(payload$args$texts$nonce, "^[0-9a-f]{32}$")
  expect_match(payload$args$texts$signature, "^[0-9a-f]{64}$")

  mapped <- kwallm_mori_resolve_worker_arg(
    payload$args$texts,
    payload$scope_key
  )
  expect_identical(as.character(mapped), c("alpha", "beta"))
})


test_that("kwallm_mori refs reject missing, wrong, and tampered keys", {
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("secret")),
    enabled = TRUE
  )
  ref <- payload$args$texts

  expect_error(
    kwallm_mori_resolve_worker_arg(ref),
    "Invalid or missing mori worker scope key"
  )
  expect_error(
    kwallm_mori_resolve_worker_arg(ref, kwallm_mori_random_token()),
    "Rejected invalid mori worker payload capability"
  )

  tampered_name <- ref
  tampered_name$name <- paste0(ref$name, "_guessed")
  expect_error(
    kwallm_mori_resolve_worker_arg(tampered_name, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )

  tampered_signature <- ref
  tampered_signature$signature <- strrep("0", 64)
  expect_error(
    kwallm_mori_resolve_worker_arg(tampered_signature, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )

  tampered_algorithm <- ref
  tampered_algorithm$signature_algorithm <- "none"
  expect_error(
    kwallm_mori_resolve_worker_arg(tampered_algorithm, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )

  malformed_nonce <- ref
  malformed_nonce$nonce <- "guess"
  expect_error(
    kwallm_mori_resolve_worker_arg(malformed_nonce, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )

  malformed_key <- ref
  malformed_key$key <- c("texts", "other")
  expect_error(
    kwallm_mori_resolve_worker_arg(malformed_key, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )

  malformed_object <- structure(1, class = "kwallm_mori_ref")
  expect_error(
    kwallm_mori_resolve_worker_arg(malformed_object, payload$scope_key),
    "Rejected invalid mori worker payload capability"
  )
})


test_that("kwallm_mori shared refs are per-payload and not global", {
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  first <- kwallm_mori_share_worker_payload(
    list(texts = c("same")),
    enabled = TRUE
  )
  second <- kwallm_mori_share_worker_payload(
    list(texts = c("same")),
    enabled = TRUE
  )

  expect_true(kwallm_mori_is_ref(first$args$texts))
  expect_true(kwallm_mori_is_ref(second$args$texts))
  expect_false(identical(first$args$texts$name, second$args$texts$name))
  expect_false(identical(first$scope_key, second$scope_key))
  expect_identical(
    as.character(kwallm_mori_resolve_worker_arg(
      first$args$texts,
      first$scope_key
    )),
    "same"
  )
  expect_identical(
    as.character(kwallm_mori_resolve_worker_arg(
      second$args$texts,
      second$scope_key
    )),
    "same"
  )
  expect_error(
    kwallm_mori_resolve_worker_arg(second$args$texts, first$scope_key),
    "Rejected invalid mori worker payload capability"
  )
})


test_that("kwallm_mirai_submit retries without blocking when the queue is full", {
  attempts <- 0L
  scheduled <- list()
  now <- 0
  resolved <- NULL

  result <- kwallm_mirai_submit(
    42L,
    queue_timeout_ms = 1000,
    retry_delay_seconds = 0.1,
    try_mirai_fn = function(...) {
      attempts <<- attempts + 1L
      if (attempts < 3L) {
        return(NULL)
      }
      42L
    },
    later_fn = function(callback, delay) {
      scheduled[[length(scheduled) + 1L]] <<- callback
      now <<- now + delay
      invisible(NULL)
    },
    clock = function() now,
    promise_fn = function(action) {
      action(
        resolve = function(value) resolved <<- value,
        reject = function(error) stop(error)
      )
      structure(list(), class = "test_promise")
    },
    then_fn = function(worker, onFulfilled, onRejected) {
      onFulfilled(worker)
    }
  )

  expect_s3_class(result, "test_promise")
  expect_identical(attempts, 1L)
  expect_length(scheduled, 1L)

  scheduled[[1L]]()
  expect_identical(attempts, 2L)
  expect_length(scheduled, 2L)

  scheduled[[2L]]()
  expect_identical(attempts, 3L)
  expect_identical(resolved, 42L)
})


test_that("kwallm_mirai_submit rejects after its queue wait timeout", {
  scheduled <- NULL
  now <- 0
  rejection <- NULL

  result <- kwallm_mirai_submit(
    TRUE,
    queue_timeout_ms = 10,
    retry_delay_seconds = 0.1,
    try_mirai_fn = function(...) NULL,
    later_fn = function(callback, delay) {
      scheduled <<- callback
      now <<- now + delay
      invisible(NULL)
    },
    clock = function() now,
    promise_fn = function(action) {
      action(
        resolve = function(value) stop("unexpected resolution"),
        reject = function(error) rejection <<- error
      )
      structure(list(), class = "test_promise")
    }
  )

  expect_s3_class(result, "test_promise")
  expect_true(is.function(scheduled))
  scheduled()

  expect_s3_class(rejection, "error")
  expect_match(conditionMessage(rejection), "Timed out waiting for capacity")
})


test_that("kwallm_mori refs resolve through the app bootstrap in a real mirai worker", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")

  kwallm_test_start_mirai_daemons(n = 1L)

  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("worker", "shared")),
    enabled = TRUE
  )
  guard <- payload$guard

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "mori_payload_test",
        app_root = app_root,
        worker_options = worker_options
      )

      as.character(kwallm_mori_resolve_worker_arg(texts, mori_scope_key))
    },
    .args = c(
      list(
        app_root = normalizePath(here::here(), winslash = "/", mustWork = TRUE),
        worker_options = list(),
        texts = payload$args$texts,
        mori_scope_key = payload$scope_key
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  force(guard)
  result <- worker[]
  force(guard)
  if (mirai::is_error_value(result)) {
    fail(paste("mirai worker error:", as.character(result)))
  }

  expect_identical(result, c("worker", "shared"))
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
