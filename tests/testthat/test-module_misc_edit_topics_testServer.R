library(testthat)
library(shiny)
library(shinyjs)
suppressWarnings(library(promises))

testthat::skip_if_not_installed("rhandsontable")
testthat::skip_if_not_installed("later")

# Stub modal side effects; we only assert state changes.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)

# Source locally so these stubs are used.
source(here::here("R", "component_modal_helpers.R"), local = TRUE)
source(here::here("R", "module_misc_edit_topics.R"), local = TRUE)

# Minimal stubs for async/LLM helpers.
send_prompt_with_retries <- function(...) {
  stop("send_prompt_with_retries should not be called in this test")
}

# Deterministic future_promise stub: run synchronously and return a promise.
future_promise <- function(
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
  eval_env <- list2env(globals, parent = envir)
  value <- eval(expr_sub, envir = eval_env)
  promises::promise_resolve(value)
}

reduce_topics <- function(...) {
  stop("reduce_topics should not be called unless explicitly stubbed")
}
get_context_window_size_in_tokens <- function(...) NULL
tiktoken_load_tokenizer <- function(...) NULL
count_tokens <- function(...) 0
async_message_printer <- function(...) function(...) invisible(NULL)
app_error <- function(...) invisible(NULL)


test_that("edit_topics_server: confirm sets edited topics and updates exclusive topics", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      topics <- reactiveVal(c("  Topic 1  ", "Topic 2", "Topic 3"))
      exclusive <- reactiveVal(c("Topic 2"))

      edited <- edit_topics_server(
        id = "edit",
        topics = topics,
        exclusive_topics = exclusive,
        research_background = reactiveVal("bg"),
        assign_multiple_categories = reactiveVal(TRUE),
        llm_provider = list(parameters = list(model = "unit-test")),
        lang = lang
      )

      list(edited = edited, exclusive = exclusive)
    },
    {
      # Let initial observer run.
      session$flushReact()

      expect_null(edited())

      # Confirm should accept initial topics (after trimming) and preserve exclusives.
      session$setInputs(`edit-confirm_topics` = 1)
      session$flushReact()

      expect_equal(edited(), c("Topic 1", "Topic 2", "Topic 3"))
      expect_equal(exclusive(), c("Topic 2"))
    }
  )
})


test_that("edit_topics_server: reduce_again applies re-reduced topics and keeps valid exclusives", {
  # Stub reduction to a stable new set including the special 'not applicable' topic.
  reduce_topics <<- function(
    updated_topics,
    research_background,
    llm_provider,
    language = "nl"
  ) {
    force(updated_topics)
    force(research_background)
    force(llm_provider)
    force(language)
    c("Topic 2", "Onbekend/niet van toepassing", "New Topic")
  }

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      topics <- reactiveVal(c("Topic 1", "Topic 2", "Topic 3"))
      exclusive <- reactiveVal(c("Topic 2"))

      edited <- edit_topics_server(
        id = "edit",
        topics = topics,
        exclusive_topics = exclusive,
        research_background = reactiveVal("bg"),
        assign_multiple_categories = reactiveVal(TRUE),
        llm_provider = list(parameters = list(model = "unit-test")),
        lang = lang
      )

      list(edited = edited, exclusive = exclusive)
    },
    {
      # Initial modal observer populates topics_table_data.
      session$flushReact()

      # Trigger re-reduce.
      session$setInputs(`edit-reduce_again` = 1)
      session$flushReact()

      # Drain the promises/later queue and flush reactives until exports update.
      # Confirm should only succeed once reduction_in_progress is FALSE.
      # We repeatedly drain the promises queue and attempt confirm until edited() is set.
      for (i in 1:20) {
        later::run_now(timeout = 0)
        session$flushReact()
        session$setInputs(`edit-confirm_topics` = i)
        session$flushReact()
        if (!is.null(edited())) break
      }

      expect_equal(
        sort(edited()),
        sort(c("Topic 2", "Onbekend/niet van toepassing", "New Topic"))
      )
      expect_equal(
        sort(exclusive()),
        sort(c("Topic 2", "Onbekend/niet van toepassing"))
      )
    }
  )
})
