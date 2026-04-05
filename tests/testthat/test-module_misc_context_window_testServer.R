library(testthat)
library(shiny)

source(here::here("R", "utils_context_window.R"), local = TRUE)
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


test_that("topic_assignment_prompt_context_window_check uses the real topic list", {
  tidyprompt_ns <- asNamespace("tidyprompt")
  helper_env <- environment(topic_assignment_prompt_context_window_check)
  old_construct <- get("construct_prompt_text", envir = tidyprompt_ns)
  old_prompt_category <- get("prompt_category", envir = helper_env)
  old_prompt_multi_category <- get("prompt_multi_category", envir = helper_env)
  withr::defer({
    unlockBinding("construct_prompt_text", tidyprompt_ns)
    assign("construct_prompt_text", old_construct, envir = tidyprompt_ns)
    lockBinding("construct_prompt_text", tidyprompt_ns)
    assign("prompt_category", old_prompt_category, envir = helper_env)
    assign(
      "prompt_multi_category",
      old_prompt_multi_category,
      envir = helper_env
    )
  })

  unlockBinding("construct_prompt_text", tidyprompt_ns)
  assign(
    "construct_prompt_text",
    function(x, ...) {
      paste(c(x$text, x$categories, x$exclusive_categories), collapse = "|")
    },
    envir = tidyprompt_ns
  )
  lockBinding("construct_prompt_text", tidyprompt_ns)

  assign(
    "prompt_category",
    function(text, research_background, categories) {
      list(
        text = text,
        categories = categories,
        research_background = research_background
      )
    },
    envir = helper_env
  )
  assign(
    "prompt_multi_category",
    function(
      text,
      research_background = "",
      categories,
      exclusive_categories
    ) {
      list(
        text = text,
        categories = categories,
        research_background = research_background,
        exclusive_categories = exclusive_categories
      )
    },
    envir = helper_env
  )

  old_get_cw <- get_context_window_size_in_tokens
  withr::defer({
    get_context_window_size_in_tokens <<- old_get_cw
  })
  get_context_window_size_in_tokens <<- function(model) 20

  check <- topic_assignment_prompt_context_window_check(
    texts = c("short", "this is the longest text"),
    topics = c("Topic A", "Topic B", "Topic C"),
    research_background = "background",
    llm_provider = list(parameters = list(model = "unit-test-model")),
    assign_multiple_categories = TRUE,
    exclusive_topics = "Topic C"
  )

  expect_false(check$fits)
  expect_gt(check$prompt_tokens, check$context_window_tokens)
})


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


test_that(".kwallm_sanitize_marking_chunk_settings clamps invalid values", {
  sanitized <- .kwallm_sanitize_marking_chunk_settings(
    max_tokens = 0,
    overlap = 99
  )

  expect_identical(sanitized$max_tokens, 1)
  expect_identical(sanitized$overlap, 0)

  sanitized_ratio <- .kwallm_sanitize_marking_chunk_settings(
    max_tokens = 8,
    overlap = 0.5
  )

  expect_identical(sanitized_ratio$max_tokens, 8)
  expect_identical(sanitized_ratio$overlap, 0.5)

  sanitized_absolute <- .kwallm_sanitize_marking_chunk_settings(
    max_tokens = 4,
    overlap = 6
  )

  expect_identical(sanitized_absolute$max_tokens, 4)
  expect_identical(sanitized_absolute$overlap, 3)
})


test_that("context_window_server: marking mode sanitizes chunk settings server-side", {
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
      mode <- reactiveVal("Markeren")
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
        assign_multiple_categories = reactiveVal(FALSE),
        texts = texts,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(rv = rv)
    },
    {
      session$setInputs(`cw-max_tokens` = 4)
      session$setInputs(`cw-overlap` = 99)

      for (i in 1:10) {
        session$flushReact()
      }

      expect_identical(rv$max_tokens, 4)
      expect_identical(rv$overlap, 3)

      session$setInputs(`cw-overlap` = 0.5)
      for (i in 1:10) {
        session$flushReact()
      }

      expect_identical(rv$overlap, 0.5)
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
