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

      texts <- text_split_server(
        id = "split",
        raw_texts = raw_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )

      list(texts = texts, raw_texts = raw_texts, lang = lang)
    },
    {
      expect_equal(texts(), raw_texts())

      # Explicitly set toggle to "Nee" (default) to ensure stability.
      session$setInputs(`split-toggle` = lang()$t("Nee"))
      session$flushReact()

      expect_equal(texts(), raw_texts())
    }
  )
})


test_that("text_split_server: clicking split produces split texts (sync-mocked future)", {
  testthat::skip_if_not_installed("ipc")

  # Monkeypatch namespaced calls (ipc::shinyQueue, promises::future_promise)
  # without requiring pkgload/devtools.
  ipc_ns <- asNamespace("ipc")
  promises_ns <- asNamespace("promises")

  old_shinyQueue <- get("shinyQueue", envir = ipc_ns)
  old_future_promise <- get("future_promise", envir = promises_ns)

  withr::defer({
    unlockBinding("shinyQueue", ipc_ns)
    assign("shinyQueue", old_shinyQueue, envir = ipc_ns)
    lockBinding("shinyQueue", ipc_ns)

    unlockBinding("future_promise", promises_ns)
    assign("future_promise", old_future_promise, envir = promises_ns)
    lockBinding("future_promise", promises_ns)
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

  # Make future_promise run synchronously while still returning a promise.
  unlockBinding("future_promise", promises_ns)
  assign(
    "future_promise",
    function(
      expr = NULL,
      envir = parent.frame(),
      ...,
      substitute = TRUE,
      queue = promises::future_promise_queue()
    ) {
      dots <- list(...)
      globals <- dots$globals
      if (is.null(globals)) {
        globals <- list()
      }

      expr_sub <- if (isTRUE(substitute)) substitute(expr) else expr
      env <- list2env(globals, parent = envir)

      value <- eval(expr_sub, envir = env)
      promises::promise_resolve(value)
    },
    envir = promises_ns
  )
  lockBinding("future_promise", promises_ns)

  shiny::testServer(
    function(input, output, session) {
      raw_texts <- reactiveVal(c("alpha", "beta"))
      processing <- reactiveVal(FALSE)
      lang <- make_test_lang("nl")

      texts <- text_split_server(
        id = "split",
        raw_texts = raw_texts,
        processing = processing,
        lang = lang,
        enabled = TRUE
      )

      list(texts = texts, raw_texts = raw_texts, lang = lang)
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

      # Changing raw texts resets prior split results.
      raw_texts(c("gamma"))
      session$flushReact()

      expect_equal(texts(), c("gamma"))
    }
  )
})
