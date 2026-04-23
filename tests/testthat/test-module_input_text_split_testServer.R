library(testthat)
library(shiny)
library(bslib)
suppressWarnings(library(promises))

# Keep these tests deterministic and avoid requiring the full app wiring.

shinyQueue <- function() {
  structure(
    list(
      consumer = list(
        start = function(millis = 50) invisible(millis),
        stop = function() invisible(NULL)
      ),
      producer = list(
        fireAssignReactive = function(...) invisible(NULL)
      )
    ),
    class = "Queue"
  )
}

source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
source(here::here("R", "module_input_text_split.R"), local = TRUE)

kwallm_worker_app_root <- function(path = here::here()) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

kwallm_worker_capture_options <- function() list()

kwallm_worker_bootstrap <- function(
  task = NULL,
  app_root = NULL,
  worker_options = list(),
  log_context = NULL,
  env = parent.frame()
) {
  force(task)
  force(app_root)

  if (length(worker_options) > 0) {
    options(worker_options)
  }

  env$split_texts_with_semchunk <- split_texts_with_semchunk
  env$semchunk_load_chunker <- semchunk_load_chunker
  env$log_context_apply <- function(...) invisible(NULL)

  invisible(log_context)
}

kwallm_worker_bootstrap_globals <- function(...) {
  list(kwallm_worker_bootstrap = kwallm_worker_bootstrap)
}

# Minimal stubs used by the module.
disable_when_processing <- function(processing, input_ids) {
  shiny::observe({
    invisible(processing())
    invisible(input_ids)
  })
}

card_header_with_tooltip <- function(...) {
  shiny::tagList(...)
}

tooltip <- function(...) {
  shiny::tagList(...)
}

# A simple semchunk loader stub (avoids Python/semchunk).
semchunk_load_chunker <- function(chunk_size = 128, queue = NULL) {
  force(chunk_size)
  force(queue)

  function(texts, progress = FALSE, offsets = FALSE, overlap = 0) {
    force(progress)
    force(offsets)
    force(overlap)

    # Return a list of vectors so split_texts_with_semchunk() can unlist it.
    lapply(texts, function(txt) {
      c(paste0(txt, "__1"), paste0(txt, "__2"))
    })
  }
}

async_message_printer <- function(...) invisible(NULL)
initialize_python_environment <- function(...) invisible(NULL)


test_that("text_split_server: returns document texts when toggle is off", {
  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("a", "b"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )
      texts <- split_result$texts
      source_document_texts <- split_result$source_document_texts

      list(
        texts = texts,
        source_document_texts = source_document_texts,
        document_texts = document_texts,
        lang = lang
      )
    },
    {
      expect_equal(texts(), document_texts())
      expect_null(source_document_texts())

      # Explicitly set toggle to off (default) to ensure stability.
      session$setInputs(`split-toggle` = "false")
      session$flushReact()

      expect_equal(texts(), document_texts())
      expect_null(source_document_texts())
    }
  )
})


test_that("text_split_server: clicking split produces split texts (sync-mocked mirai)", {
  testthat::skip_if_not_installed("mirai")

  mirai_ns <- asNamespace("mirai")

  old_mirai <- get("mirai", envir = mirai_ns)

  withr::defer({
    unlockBinding("mirai", mirai_ns)
    assign("mirai", old_mirai, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  # Make mirai run synchronously while still returning a promise.
  unlockBinding("mirai", mirai_ns)
  assign(
    "mirai",
    function(
      .expr,
      ...,
      .args = list(),
      .timeout = NULL,
      .compute = NULL
    ) {
      # Combine ... and .args
      args_from_dots <- list(...)
      all_args <- c(args_from_dots, .args)

      expr_sub <- substitute(.expr)
      eval_env <- list2env(all_args, parent = baseenv())
      value <- eval(expr_sub, envir = eval_env)
      promises::promise_resolve(value)
    },
    envir = mirai_ns
  )
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("alpha", "beta"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )
      texts <- split_result$texts
      source_document_texts <- split_result$source_document_texts

      list(
        texts = texts,
        source_document_texts = source_document_texts,
        document_texts = document_texts,
        lang = lang
      )
    },
    {
      # Turn splitting on.
      session$setInputs(`split-toggle` = "true")
      session$flushReact()

      expect_equal(texts(), document_texts())

      # Trigger splitting.
      session$setInputs(`split-max_tokens` = 5)
      session$flushReact()

      session$setInputs(`split-split_texts` = 1)
      session$flushReact()

      # Allow promise callbacks to run.
      later::run_now(0.25)
      session$flushReact()

      expect_true(is.character(texts()))
      expect_true(length(texts()) > length(document_texts()))
      expect_true(all(grepl("__", texts(), fixed = TRUE)))

      # source_document_texts maps each chunk back to its upload row text.
      expect_equal(length(source_document_texts()), length(texts()))
      expect_true(all(source_document_texts() %in% c("alpha", "beta")))

      # Changing document texts resets prior split results.
      document_texts(c("gamma"))
      session$flushReact()

      expect_equal(texts(), c("gamma"))
      expect_null(source_document_texts())
    }
  )
})


test_that("text_split_server ignores stale async split results after source text changes", {
  testthat::skip_if_not_installed("mirai")

  mirai_ns <- asNamespace("mirai")

  old_mirai <- get("mirai", envir = mirai_ns)
  deferred <- new.env(parent = emptyenv())

  withr::defer({
    unlockBinding("mirai", mirai_ns)
    assign("mirai", old_mirai, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  unlockBinding("mirai", mirai_ns)
  assign(
    "mirai",
    function(
      .expr,
      ...,
      .args = list(),
      .timeout = NULL,
      .compute = NULL
    ) {
      promises::promise(function(resolve, reject) {
        deferred$resolve <- resolve
        deferred$reject <- reject
      })
    },
    envir = mirai_ns
  )
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("alpha", "beta"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )

      texts <- split_result$texts
      source_document_texts <- split_result$source_document_texts
      split_in_progress <- split_result$split_in_progress

      list(
        texts = texts,
        source_document_texts = source_document_texts,
        split_in_progress = split_in_progress,
        document_texts = document_texts,
        lang = lang
      )
    },
    {
      session$setInputs(`split-toggle` = "true")
      session$flushReact()

      session$setInputs(`split-max_tokens` = 5)
      session$flushReact()

      session$setInputs(`split-split_texts` = 1)
      session$flushReact()

      # While the async worker is running, texts are NULL and split_in_progress
      # is TRUE.
      expect_null(texts())
      expect_true(split_in_progress())

      # Changing the source texts while the split is running should immediately
      # unblock: split_in_progress must go FALSE so analysis launch is no longer
      # gated, and the input texts should be available.
      document_texts(c("gamma"))
      session$flushReact()

      expect_false(
        split_in_progress(),
        info = "split_in_progress should be cleared when source texts change"
      )
      expect_identical(texts(), c("gamma"))

      # When the stale worker eventually resolves, the result must be ignored.
      stale_result <- split_texts_with_semchunk(
        texts = c("alpha", "beta"),
        source_document_ids = c(1L, 2L),
        source_document_texts = c("alpha", "beta"),
        chunk_size = 5
      )

      deferred$resolve(stale_result)
      later::run_now(0.25)
      session$flushReact()

      expect_identical(texts(), c("gamma"))
      expect_null(source_document_texts())
    }
  )
})


test_that("text_split_server passes worker setup globals for semchunk async work", {
  testthat::skip_if_not_installed("mirai")

  mirai_ns <- asNamespace("mirai")

  old_mirai <- get("mirai", envir = mirai_ns)
  captured <- new.env(parent = emptyenv())

  withr::defer({
    unlockBinding("mirai", mirai_ns)
    assign("mirai", old_mirai, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  unlockBinding("mirai", mirai_ns)
  assign(
    "mirai",
    function(
      .expr,
      ...,
      .args = list(),
      .timeout = NULL,
      .compute = NULL
    ) {
      force(.timeout)
      force(.compute)

      captured$args <- c(list(...), .args)

      promises::promise(function(resolve, reject) {
        captured$resolve <- resolve
        captured$reject <- reject
      })
    },
    envir = mirai_ns
  )
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("alpha", "beta"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )

      list(split_result = split_result, lang = lang)
    },
    {
      session$setInputs(`split-toggle` = "true")
      session$flushReact()

      session$setInputs(`split-max_tokens` = 5)
      session$flushReact()

      session$setInputs(`split-split_texts` = 1)
      session$flushReact()

      expect_true(all(
        c(
          "kwallm_worker_bootstrap",
          "app_root",
          "worker_options",
          "log_context"
        ) %in%
          names(captured$args)
      ))
    }
  )
})


test_that("text_split_server preserves toggle and numeric values across language re-render", {
  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("alpha"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )

      list(split_result = split_result, lang = lang)
    },
    {
      session$setInputs(`split-toggle` = "true")
      session$flushReact()

      session$setInputs(`split-max_tokens` = 64)
      session$setInputs(`split-overlap` = 4)
      session$flushReact()

      lang(make_test_lang("en")())
      session$flushReact()

      expect_true(split_result$split_settings()$enabled)
      expect_equal(split_result$split_settings()$chunk_size, 64)
      expect_equal(split_result$split_settings()$overlap, 4)
      expect_match(
        output$`split-card`$html,
        'value="true" checked="checked"',
        fixed = TRUE
      )
      expect_match(output$`split-split_ui`$html, 'value="64"', fixed = TRUE)
      expect_match(output$`split-split_ui`$html, 'value="4"', fixed = TRUE)
    }
  )
})


test_that("split_texts_with_semchunk preserves row lineage metadata", {
  result <- split_texts_with_semchunk(
    texts = c("alpha", "beta"),
    source_document_ids = c(10L, 20L),
    source_document_texts = c("Doc A", "Doc B"),
    chunk_size = 5
  )

  expect_identical(
    result$texts,
    c("alpha__1", "alpha__2", "beta__1", "beta__2")
  )
  expect_identical(
    result$source_document_text,
    c("Doc A", "Doc A", "Doc B", "Doc B")
  )
  expect_identical(result$rows$source_document_id, c(10L, 10L, 20L, 20L))
  expect_identical(result$rows$document_id, c(1L, 2L, 3L, 4L))
  expect_identical(
    result$rows$source_document_text,
    c("Doc A", "Doc A", "Doc B", "Doc B")
  )
  expect_identical(
    result$rows$document_text,
    c("alpha__1", "alpha__2", "beta__1", "beta__2")
  )
})


test_that("text_split_server ignores manual splitting in marking mode", {
  shiny::testServer(
    function(input, output, session) {
      document_texts <- reactiveVal(c("a", "b"))
      processing <- reactiveVal(FALSE)
      mode <- reactiveVal("Markeren")
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        document_texts = document_texts,
        processing = processing,
        mode = mode,
        lang = lang,
        enabled = TRUE
      )

      texts <- split_result$texts
      split_settings <- split_result$split_settings

      list(
        texts = texts,
        split_settings = split_settings,
        document_texts = document_texts,
        lang = lang
      )
    },
    {
      session$setInputs(`split-toggle` = "true")
      session$flushReact()

      expect_identical(texts(), document_texts())
      expect_false(split_settings()$enabled)
    }
  )
})
