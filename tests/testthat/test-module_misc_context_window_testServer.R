library(testthat)
library(shiny)

source(here::here("R", "utils_create_text_batches.R"), local = TRUE)
source(here::here("R", "module_misc_context_window.R"), local = TRUE)

# Minimal stubs used by this module (UI helpers + validators).
disable_when_processing <- function(processing, input_ids) {
  shiny::observe({
    invisible(processing())
    invisible(input_ids)
  })
}

card_header_with_tooltip <- function(...) {
  shiny::tagList(...)
}

description_box <- function(...) {
  shiny::tagList(...)
}

tooltip <- function(...) {
  shiny::tagList(...)
}

is_valid_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

# Prompt helpers are defined in analysis files in the full app; stub them here.
prompt_category <- function(...) list(prompt = "category")
prompt_multi_category <- function(...) list(prompt = "multi_category")
prompt_score <- function(...) list(prompt = "score")
mark_text_prompt <- function(...) list(prompt = "mark")
prompt_candidate_topics <- function(...) list(prompt = "candidate_topics")

# Token counter is provided via reticulate/tiktoken in the full app; stub for determinism.
count_tokens <- function(x) {
  nchar(x)
}


test_that("context_window_server: fit flag flips based on context window size", {
  # Monkeypatch tidyprompt::construct_prompt_text without pkgload/devtools.
  tidyprompt_ns <- asNamespace("tidyprompt")
  old_construct <- get("construct_prompt_text", envir = tidyprompt_ns)
  withr::defer({
    unlockBinding("construct_prompt_text", tidyprompt_ns)
    assign("construct_prompt_text", old_construct, envir = tidyprompt_ns)
    lockBinding("construct_prompt_text", tidyprompt_ns)
  })

  unlockBinding("construct_prompt_text", tidyprompt_ns)
  assign(
    "construct_prompt_text",
    function(x, ...) {
      "PROMPTTT" # nchar = 8
    },
    envir = tidyprompt_ns
  )
  lockBinding("construct_prompt_text", tidyprompt_ns)

  old_get_cw <- get_context_window_size_in_tokens
  withr::defer({
    get_context_window_size_in_tokens <<- old_get_cw
  })
  # Start with a context window that must fail.
  get_context_window_size_in_tokens <<- function(model) 1

  shiny::testServer(
    function(input, output, session) {
      mode <- reactiveVal("Categorisatie")
      lang <- make_test_lang("nl")

      models <- reactiveValues(
        main = list(parameters = list(model = "unit-test-model-small")),
        large = NULL
      )

      categories <- list(
        texts = reactiveVal(c("CatA", "CatB")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(2),
        exclusive_texts = reactiveVal(character())
      )

      codes <- list(
        texts = reactiveVal(c("Code1")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(1)
      )

      texts <- reactiveValues(
        preprocessed = c("123456789012345678"),
        document_text = character()
      )

      rv <- context_window_server(
        id = "cw",
        mode = mode,
        models = models,
        categories = categories,
        scoring_characteristic = reactiveVal("X"),
        codes = codes,
        research_background = reactiveVal("background"),
        assign_multiple_categories = reactiveVal(FALSE),
        texts = texts,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(rv = rv, texts = texts, models = models)
    },
    {
      # Let observers settle.
      for (i in 1:10) {
        session$flushReact()
        if (
          !is.null(rv$fit_context_window_assigning) &&
            !is.null(rv$any_fit_problem)
        ) {
          break
        }
      }

      # Context window forced to 1 => must not fit.
      expect_identical(rv$fit_context_window_assigning, FALSE)
      expect_identical(rv$any_fit_problem, TRUE)

      # Now switch to a large context window by changing model (triggers the observer).
      get_context_window_size_in_tokens <<- function(model) 100
      models$main <- list(parameters = list(model = "unit-test-model-large"))
      for (i in 1:10) {
        session$flushReact()
      }

      expect_identical(rv$fit_context_window_assigning, TRUE)
      expect_identical(rv$any_fit_problem, FALSE)
    }
  )
})


test_that("context_window_server: topic mode sets batch flags and too-many-batches", {
  tidyprompt_ns <- asNamespace("tidyprompt")
  old_construct <- get("construct_prompt_text", envir = tidyprompt_ns)
  withr::defer({
    unlockBinding("construct_prompt_text", tidyprompt_ns)
    assign("construct_prompt_text", old_construct, envir = tidyprompt_ns)
    lockBinding("construct_prompt_text", tidyprompt_ns)
  })

  unlockBinding("construct_prompt_text", tidyprompt_ns)
  assign(
    "construct_prompt_text",
    function(x, ...) "PROMPT",
    envir = tidyprompt_ns
  )
  lockBinding("construct_prompt_text", tidyprompt_ns)

  old_get_cw <- get_context_window_size_in_tokens
  withr::defer({
    get_context_window_size_in_tokens <<- old_get_cw
  })
  get_context_window_size_in_tokens <<- function(model) 100

  shiny::testServer(
    function(input, output, session) {
      mode <- reactiveVal("Onderwerpextractie")
      lang <- make_test_lang("nl")

      models <- reactiveValues(
        main = list(parameters = list(model = "unit-test-model")),
        large = NULL
      )

      categories <- list(
        texts = reactiveVal(c("CatA")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(1),
        exclusive_texts = reactiveVal(character())
      )

      codes <- list(
        texts = reactiveVal(c("Code1")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(1)
      )

      texts <- reactiveValues(
        preprocessed = c("t1", "t2", "t3"),
        document_text = character()
      )

      rv <- context_window_server(
        id = "cw",
        mode = mode,
        models = models,
        categories = categories,
        scoring_characteristic = reactiveVal("X"),
        codes = codes,
        research_background = reactiveVal("background"),
        assign_multiple_categories = reactiveVal(FALSE),
        texts = texts,
        processing = reactiveVal(FALSE),
        lang = lang,
        number_of_batches_limit = 2
      )

      list(rv = rv)
    },
    {
      # Force each text into its own batch, deterministically.
      session$setInputs(`cw-batch_size` = 1)

      for (i in 1:10) {
        session$flushReact()
      }

      expect_identical(rv$fit_context_window_batches, TRUE)
      expect_identical(rv$too_many_batches, TRUE)
      expect_equal(rv$n_batches, 3)
    }
  )
})


test_that("context_window_server: multi-label uses actual exclusive_texts, not fabricated ones", {
  # Capture what prompt_multi_category receives for exclusive_categories.
  captured_exclusive <- NULL
  module_env <- environment(context_window_server)
  old_pmc <- get("prompt_multi_category", envir = module_env)
  withr::defer(assign("prompt_multi_category", old_pmc, envir = module_env))
  assign(
    "prompt_multi_category",
    function(
      text = "",
      research_background = "",
      categories = character(),
      exclusive_categories = character(),
      ...
    ) {
      captured_exclusive <<- exclusive_categories
      list(prompt = "multi_category")
    },
    envir = module_env
  )

  tidyprompt_ns <- asNamespace("tidyprompt")
  old_construct <- get("construct_prompt_text", envir = tidyprompt_ns)
  withr::defer({
    unlockBinding("construct_prompt_text", tidyprompt_ns)
    assign("construct_prompt_text", old_construct, envir = tidyprompt_ns)
    lockBinding("construct_prompt_text", tidyprompt_ns)
  })

  unlockBinding("construct_prompt_text", tidyprompt_ns)
  assign(
    "construct_prompt_text",
    function(x, ...) "PROMPTTT",
    envir = tidyprompt_ns
  )
  lockBinding("construct_prompt_text", tidyprompt_ns)

  old_get_cw <- get_context_window_size_in_tokens
  withr::defer({
    get_context_window_size_in_tokens <<- old_get_cw
  })
  get_context_window_size_in_tokens <<- function(model) 100

  shiny::testServer(
    function(input, output, session) {
      mode <- reactiveVal("Categorisatie")
      lang <- make_test_lang("nl")

      models <- reactiveValues(
        main = list(parameters = list(model = "unit-test-model")),
        large = NULL
      )

      # 4 categories, only the first is exclusive.
      # Fabricated logic (every 2nd) would yield c("CatB", "CatD") — wrong.
      categories <- list(
        texts = reactiveVal(c("CatA", "CatB", "CatC", "CatD")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(4),
        exclusive_texts = reactiveVal(c("CatA"))
      )

      codes <- list(
        texts = reactiveVal(c("Code1")),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(1)
      )

      texts <- reactiveValues(
        preprocessed = c("some text"),
        document_text = character()
      )

      rv <- context_window_server(
        id = "cw",
        mode = mode,
        models = models,
        categories = categories,
        scoring_characteristic = reactiveVal("X"),
        codes = codes,
        research_background = reactiveVal("background"),
        assign_multiple_categories = reactiveVal(TRUE),
        texts = texts,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(rv = rv)
    },
    {
      for (i in 1:10) {
        session$flushReact()
      }

      # The module must have called prompt_multi_category with the real
      # exclusive texts, not a fabricated every-second-category vector.
      expect_identical(captured_exclusive, c("CatA"))
    }
  )
})
