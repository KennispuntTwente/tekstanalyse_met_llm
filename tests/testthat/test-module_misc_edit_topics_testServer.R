library(testthat)
library(shiny)
library(shinyjs)
suppressWarnings(library(promises))

testthat::skip_if_not_installed("rhandsontable")
testthat::skip_if_not_installed("later")

# Stub modal side effects; we only assert state changes.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
last_notification <- NULL
showNotification <- function(
  ui,
  type = c("default", "message", "warning", "error"),
  ...
) {
  last_notification <<- list(
    ui = as.character(ui),
    type = match.arg(type)
  )
  invisible(NULL)
}

# Source locally so these stubs are used.
source(here::here("R", "component_modal_helpers.R"), local = TRUE)
source(here::here("R", "utils_context_window.R"), local = TRUE)
source(here::here("R", "module_misc_edit_topics.R"), local = TRUE)

# Minimal stubs for async/LLM helpers.
send_prompt_with_retries <- function(...) {
  stop("send_prompt_with_retries should not be called in this test")
}

# Deterministic mirai stub: run synchronously and return a promise.
# We replace mirai::mirai in the mirai namespace for the test.
mirai_sync_stub <- function(
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
}

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

  env$reduce_topics <- reduce_topics
  env$get_context_window_size_in_tokens <- get_context_window_size_in_tokens
  env$tiktoken_load_tokenizer <- tiktoken_load_tokenizer
  env$count_tokens <- count_tokens
  env$async_message_printer <- async_message_printer
  env$send_prompt_with_retries <- send_prompt_with_retries
  env$app_error <- app_error

  invisible(log_context)
}

kwallm_worker_bootstrap_globals <- function(...) {
  list(kwallm_worker_bootstrap = kwallm_worker_bootstrap)
}

kwallm_worker_app_root <- function(path = here::here()) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

kwallm_worker_capture_options <- function() list()

# Patch mirai::mirai to run synchronously for all tests in this file
mirai_ns <- asNamespace("mirai")
old_mirai_fn <- get("mirai", envir = mirai_ns)

# Define setup/teardown for this file
setup({
  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", mirai_sync_stub, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)
})

teardown({
  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", old_mirai_fn, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)
})

reduce_topics <- function(...) {
  stop("reduce_topics should not be called unless explicitly stubbed")
}
get_context_window_size_in_tokens <- function(...) NULL
tiktoken_load_tokenizer <- function(...) NULL
count_tokens <- function(...) 0
async_message_printer <- function(...) function(...) invisible(NULL)
app_error <- function(...) invisible(NULL)
topic_assignment_prompt_context_window_check <- function(...) {
  list(fits = TRUE, prompt_tokens = 10L, context_window_tokens = 100L)
}


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
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
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


test_that("edit_topics_server: whitespace-only rows are ignored and single topic can be confirmed", {
  hot_ns <- asNamespace("rhandsontable")
  old_hot_to_r <- get("hot_to_r", envir = hot_ns)
  withr::defer({
    unlockBinding("hot_to_r", hot_ns)
    assign("hot_to_r", old_hot_to_r, envir = hot_ns)
    lockBinding("hot_to_r", hot_ns)
  })

  unlockBinding("hot_to_r", hot_ns)
  assign(
    "hot_to_r",
    function(...) {
      data.frame(
        topic = c("Topic 1", "   "),
        exclusive = c(FALSE, FALSE),
        stringsAsFactors = FALSE
      )
    },
    envir = hot_ns
  )
  lockBinding("hot_to_r", hot_ns)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      topics <- reactiveVal(c("Topic 1", "Topic 2"))
      exclusive <- reactiveVal(character())

      edited <- edit_topics_server(
        id = "edit",
        topics = topics,
        exclusive_topics = exclusive,
        research_background = reactiveVal("bg"),
        assign_multiple_categories = reactiveVal(TRUE),
        llm_provider = list(parameters = list(model = "unit-test")),
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
        lang = lang
      )

      list(edited = edited)
    },
    {
      session$flushReact()

      session$setInputs(`edit-topics_table` = list(dummy = TRUE))
      session$flushReact()
      session$setInputs(`edit-confirm_topics` = 1)
      session$flushReact()

      expect_identical(edited(), "Topic 1")
    }
  )
})


test_that("edit_topics_server: empty rows still fail the minimum topic count", {
  hot_ns <- asNamespace("rhandsontable")
  shiny_ns <- asNamespace("shiny")
  old_hot_to_r <- get("hot_to_r", envir = hot_ns)
  old_show_notification <- get("showNotification", envir = shiny_ns)
  withr::defer({
    unlockBinding("hot_to_r", hot_ns)
    assign("hot_to_r", old_hot_to_r, envir = hot_ns)
    lockBinding("hot_to_r", hot_ns)
    unlockBinding("showNotification", shiny_ns)
    assign("showNotification", old_show_notification, envir = shiny_ns)
    lockBinding("showNotification", shiny_ns)
  })

  unlockBinding("hot_to_r", hot_ns)
  assign(
    "hot_to_r",
    function(...) {
      data.frame(
        topic = c("   ", ""),
        exclusive = c(FALSE, FALSE),
        stringsAsFactors = FALSE
      )
    },
    envir = hot_ns
  )
  lockBinding("hot_to_r", hot_ns)

  unlockBinding("showNotification", shiny_ns)
  assign(
    "showNotification",
    function(ui, type = c("default", "message", "warning", "error"), ...) {
      last_notification <<- list(
        ui = as.character(ui),
        type = match.arg(type)
      )
      invisible(NULL)
    },
    envir = shiny_ns
  )
  lockBinding("showNotification", shiny_ns)

  last_notification <<- NULL

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      topics <- reactiveVal(c("Topic 1", "Topic 2"))
      exclusive <- reactiveVal(character())

      edited <- edit_topics_server(
        id = "edit",
        topics = topics,
        exclusive_topics = exclusive,
        research_background = reactiveVal("bg"),
        assign_multiple_categories = reactiveVal(TRUE),
        llm_provider = list(parameters = list(model = "unit-test")),
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
        lang = lang
      )

      list(edited = edited)
    },
    {
      session$flushReact()

      session$setInputs(`edit-topics_table` = list(dummy = TRUE))
      session$flushReact()
      session$setInputs(`edit-confirm_topics` = 1)
      session$flushReact()

      expect_null(edited())
      expect_false(is.null(last_notification))
      expect_identical(last_notification$type, "error")
      expect_match(last_notification$ui, "minimaal 1 onderwerp")
    }
  )
})


test_that("edit_topics_server: reduce_again ignores single-topic input", {
  reduce_topics <<- function(...) {
    testthat::fail("reduce_topics should not be called for a single topic")
  }

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      topics <- reactiveVal("Topic 1")
      exclusive <- reactiveVal(character())

      edited <- edit_topics_server(
        id = "edit",
        topics = topics,
        exclusive_topics = exclusive,
        research_background = reactiveVal("bg"),
        assign_multiple_categories = reactiveVal(TRUE),
        llm_provider = list(parameters = list(model = "unit-test")),
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
        lang = lang
      )

      list(edited = edited)
    },
    {
      session$flushReact()

      session$setInputs(`edit-reduce_again` = 1)
      session$flushReact()

      expect_null(edited())
    }
  )
})


test_that("edit_topics_server: reduce_again applies re-reduced topics and keeps valid exclusives", {
  # Stub reduction to a stable new set including the special 'not applicable' topic.
  reduce_topics <<- function(
    updated_topics,
    research_background,
    llm_provider,
    language = "nl",
    n_tokens_context_window = NULL
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
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
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


test_that("edit_topics_server: confirm blocks topics that exceed context window", {
  module_env <- environment(edit_topics_server)
  shiny_ns <- asNamespace("shiny")
  old_fit_check <- get(
    "topic_assignment_prompt_context_window_check",
    envir = module_env
  )
  old_show_notification <- get("showNotification", envir = shiny_ns)
  withr::defer({
    assign(
      "topic_assignment_prompt_context_window_check",
      old_fit_check,
      envir = module_env
    )
    unlockBinding("showNotification", shiny_ns)
    assign("showNotification", old_show_notification, envir = shiny_ns)
    lockBinding("showNotification", shiny_ns)
  })
  assign(
    "topic_assignment_prompt_context_window_check",
    function(...) {
      list(fits = FALSE, prompt_tokens = 140L, context_window_tokens = 100L)
    },
    envir = module_env
  )

  unlockBinding("showNotification", shiny_ns)
  assign(
    "showNotification",
    function(ui, type = c("default", "message", "warning", "error"), ...) {
      last_notification <<- list(
        ui = as.character(ui),
        type = match.arg(type)
      )
      invisible(NULL)
    },
    envir = shiny_ns
  )
  lockBinding("showNotification", shiny_ns)

  last_notification <<- NULL

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
        assignment_texts = reactive(c("short text")),
        assignment_llm_provider = reactive(list(
          parameters = list(model = "unit-test")
        )),
        lang = lang
      )

      list(edited = edited, exclusive = exclusive)
    },
    {
      session$flushReact()

      session$setInputs(`edit-confirm_topics` = 1)
      session$flushReact()

      expect_null(edited())
      expect_false(is.null(last_notification))
      expect_identical(last_notification$type, "error")
      expect_match(
        last_notification$ui,
        "context-window van het toekenningsmodel"
      )
    }
  )
})
