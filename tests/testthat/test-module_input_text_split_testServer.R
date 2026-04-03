library(testthat)
library(shiny)
suppressWarnings(library(promises))

# Keep these tests deterministic and avoid requiring the full app wiring.

source(here::here("R", "module_input_text_split.R"), local = TRUE)

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


test_that("text_split_server: returns raw texts when toggle is off", {
  shiny::testServer(
    function(input, output, session) {
      raw_texts <- reactiveVal(c("a", "b"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        raw_texts = raw_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )
      texts <- split_result$texts
      source_texts <- split_result$source_texts

      list(
        texts = texts,
        source_texts = source_texts,
        raw_texts = raw_texts,
        lang = lang
      )
    },
    {
      expect_equal(texts(), raw_texts())
      expect_null(source_texts())

      # Explicitly set toggle to "Nee" (default) to ensure stability.
      session$setInputs(`split-toggle` = lang()$t("Nee"))
      session$flushReact()

      expect_equal(texts(), raw_texts())
      expect_null(source_texts())
    }
  )
})


test_that("text_split_server: clicking split produces split texts (sync-mocked mirai)", {
  testthat::skip_if_not_installed("ipc")
  testthat::skip_if_not_installed("mirai")

  # Monkeypatch namespaced calls (ipc::shinyQueue, mirai::mirai)
  # without requiring pkgload/devtools.
  ipc_ns <- asNamespace("ipc")
  mirai_ns <- asNamespace("mirai")

  old_shinyQueue <- get("shinyQueue", envir = ipc_ns)
  old_mirai <- get("mirai", envir = mirai_ns)

  withr::defer({
    unlockBinding("shinyQueue", ipc_ns)
    assign("shinyQueue", old_shinyQueue, envir = ipc_ns)
    lockBinding("shinyQueue", ipc_ns)

    unlockBinding("mirai", mirai_ns)
    assign("mirai", old_mirai, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  # Stub ipc queue so we don't need the real async queue internals.
  unlockBinding("shinyQueue", ipc_ns)
  assign(
    "shinyQueue",
    function() {
      list(
        consumer = list(
          start = function(millis = 50) invisible(millis),
          stop = function() invisible(NULL)
        ),
        producer = list(
          fireAssignReactive = function(...) invisible(NULL)
        )
      )
    },
    envir = ipc_ns
  )
  lockBinding("shinyQueue", ipc_ns)

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
      eval_env <- list2env(all_args, parent = parent.frame())
      value <- eval(expr_sub, envir = eval_env)
      promises::promise_resolve(value)
    },
    envir = mirai_ns
  )
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      raw_texts <- reactiveVal(c("alpha", "beta"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      split_result <- text_split_server(
        id = "split",
        raw_texts = raw_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )
      texts <- split_result$texts
      source_texts <- split_result$source_texts

      list(
        texts = texts,
        source_texts = source_texts,
        raw_texts = raw_texts,
        lang = lang
      )
    },
    {
      # Turn splitting on.
      session$setInputs(`split-toggle` = lang()$t("Ja"))
      session$flushReact()

      expect_equal(texts(), raw_texts())

      # Trigger splitting.
      session$setInputs(`split-max_tokens` = 5)
      session$flushReact()

      session$setInputs(`split-split_texts` = 1)
      session$flushReact()

      # Allow promise callbacks to run.
      later::run_now(0.25)
      session$flushReact()

      expect_true(is.character(texts()))
      expect_true(length(texts()) > length(raw_texts()))
      expect_true(all(grepl("__", texts(), fixed = TRUE)))

      # source_texts maps each chunk back to its original text
      expect_equal(length(source_texts()), length(texts()))
      expect_true(all(source_texts() %in% c("alpha", "beta")))

      # Changing raw texts resets prior split results.
      raw_texts(c("gamma"))
      session$flushReact()

      expect_equal(texts(), c("gamma"))
      expect_null(source_texts())
    }
  )
})
