library(testthat)
library(shiny)
library(shinyjs)
suppressWarnings(library(promises))

testthat::skip_if_not_installed("tidyprompt")
testthat::skip_if_not_installed("mirai")
testthat::skip_if_not_installed("later")

mirai_sync_stub <- function(
  .expr,
  ...,
  .args = list(),
  .timeout = NULL,
  .compute = NULL
) {
  args_from_dots <- list(...)
  all_args <- c(args_from_dots, .args)

  expr_sub <- substitute(.expr)
  eval_env <- list2env(all_args, parent = parent.frame())
  value <- eval(expr_sub, envir = eval_env)
  promises::promise_resolve(value)
}


stub_progress_bar_server <- function(...) {
  shiny::reactiveValues(
    set = function(...) invisible(NULL),
    set_with_total = function(...) invisible(NULL),
    show = function() invisible(NULL),
    hide = function() invisible(NULL),
    async = list(
      stop = function() invisible(NULL),
      set = function(...) invisible(NULL),
      set_with_total = function(...) invisible(NULL),
      show = function() invisible(NULL),
      hide = function() invisible(NULL)
    )
  )
}


stub_llm_streaming_server <- function(...) {
  shiny::reactiveValues(
    set = function(...) invisible(NULL),
    append = function(...) invisible(NULL),
    clear = function() invisible(NULL),
    show = function() invisible(NULL),
    hide = function() invisible(NULL),
    text = shiny::reactiveVal(""),
    async = list(
      stop = function() invisible(NULL),
      set = function(...) invisible(NULL),
      append = function(...) invisible(NULL),
      clear = function() invisible(NULL),
      show = function() invisible(NULL),
      hide = function() invisible(NULL)
    )
  )
}


test_that("processing_server: topic fit check receives a real provider object", {
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  progress_bar_server <- stub_progress_bar_server
  llm_streaming_server <- stub_llm_streaming_server

  processing_texts_under_maximum <- function(...) TRUE
  processing_split_ready <- function(...) TRUE
  processing_anonymization_ready <- function(...) TRUE
  processing_has_pending_gliner_anonymization <- function(...) FALSE

  log_action <- function(...) invisible(NULL)
  log_analysis_start <- function(...) invisible(NULL)
  log_context_capture <- function(...) list()
  log_context_apply <- function(...) invisible(NULL)
  log_async_globals <- function(...) list()
  log_info <- function(...) invisible(NULL)
  log_debug <- function(...) invisible(NULL)
  log_warn <- function(...) invisible(NULL)

  handle_detailed_error <- function(...) {
    function(err) stop(err)
  }

  app_error <- function(error, ...) stop(error)

  send_prompt_with_retries_async_globals <- function(...) list()
  analysis_async_topic_modelling_globals <- function(...) list()
  analysis_async_tokenizer_globals <- function(...) list()
  analysis_async_worker_setup_globals <- function(...) list()
  analysis_async_processing_globals <- function(...) list()

  .kwallm__prompt_execution_reset <- function(...) invisible(NULL)
  .kwallm__prompt_execution_get <- function(...) NULL

  create_candidate_topics <- function(...) c("Candidate 1", "Candidate 2")
  reduce_topics <- function(...) c("Topic A", "Topic B")

  showNotification <- function(...) invisible(NULL)

  source(here::here("R", "module_core_processing.R"), local = TRUE)

  module_env <- environment(processing_server)
  old_fit_check <- get(
    "topic_assignment_prompt_context_window_check",
    envir = module_env
  )
  old_edit_topics_server <- get("edit_topics_server", envir = module_env)

  withr::defer({
    assign(
      "topic_assignment_prompt_context_window_check",
      old_fit_check,
      envir = module_env
    )
    assign("edit_topics_server", old_edit_topics_server, envir = module_env)
  })

  captured_provider <- NULL
  edit_topics_started <- FALSE

  assign(
    "topic_assignment_prompt_context_window_check",
    function(
      texts,
      topics,
      research_background,
      llm_provider,
      assign_multiple_categories,
      exclusive_topics
    ) {
      captured_provider <<- llm_provider
      list(fits = FALSE, prompt_tokens = 140L, context_window_tokens = 100L)
    },
    envir = module_env
  )

  assign(
    "edit_topics_server",
    function(...) {
      edit_topics_started <<- TRUE
      shiny::reactiveVal(NULL)
    },
    envir = module_env
  )

  mirai_ns <- asNamespace("mirai")
  old_mirai_fn <- get("mirai", envir = mirai_ns)
  withr::defer({
    if (bindingIsLocked("mirai", mirai_ns)) {
      unlockBinding("mirai", mirai_ns)
    }
    assign("mirai", old_mirai_fn, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", mirai_sync_stub, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      texts <- shiny::reactiveValues(
        preprocessed = c("first text", "second text"),
        analysis_units = data.frame(analysis_unit_id = c(1L, 2L)),
        df = data.frame(
          document_text = c("first text", "second text"),
          stringsAsFactors = FALSE
        )
      )

      models <- shiny::reactiveValues(
        main = kwallm_test_llm_provider("kwallm-fake-main-1024"),
        large = kwallm_test_llm_provider("kwallm-fake-reducer-320")
      )

      categories <- list(
        texts = shiny::reactiveVal(character()),
        exclusive_texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      codes <- list(
        texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      context_window <- shiny::reactiveValues(
        any_fit_problem = FALSE,
        too_many_batches = FALSE,
        text_batches = list(c("first text", "second text"))
      )

      processing_server(
        id = "processing",
        mode = shiny::reactiveVal("Onderwerpextractie"),
        interrater_reliability_toggle = shiny::reactiveVal(FALSE),
        texts = texts,
        llm_provider_rv = shiny::reactiveValues(),
        models = models,
        categories = categories,
        scoring_characteristic = shiny::reactiveVal(""),
        codes = codes,
        research_background = shiny::reactiveVal("Background"),
        style_prompt = shiny::reactiveVal(""),
        human_in_the_loop = shiny::reactiveVal(FALSE),
        assign_multiple_categories = shiny::reactiveVal(TRUE),
        write_paragraphs = shiny::reactiveVal(FALSE),
        context_window = context_window,
        lang = lang
      )

      list(models = models)
    },
    {
      session$setInputs(`processing-process` = 1)

      for (i in seq_len(20)) {
        later::run_now(timeout = 0)
        session$flushReact()

        if (!is.null(captured_provider) && isTRUE(edit_topics_started)) {
          break
        }
      }

      expect_true(edit_topics_started)
      expect_true(inherits(captured_provider, "LlmProvider"))
      expect_identical(
        captured_provider$parameters$model,
        "kwallm-fake-main-1024"
      )
    }
  )
})


test_that("processing_server: auto-confirm drops blank reduced topics", {
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  progress_bar_server <- stub_progress_bar_server
  llm_streaming_server <- stub_llm_streaming_server

  processing_texts_under_maximum <- function(...) TRUE
  processing_split_ready <- function(...) TRUE
  processing_anonymization_ready <- function(...) TRUE
  processing_has_pending_gliner_anonymization <- function(...) FALSE

  log_action <- function(...) invisible(NULL)
  log_analysis_start <- function(...) invisible(NULL)
  log_context_capture <- function(...) list()
  log_context_apply <- function(...) invisible(NULL)
  log_async_globals <- function(...) list()
  log_info <- function(...) invisible(NULL)
  log_debug <- function(...) invisible(NULL)
  log_warn <- function(...) invisible(NULL)

  handle_detailed_error <- function(...) {
    function(err) stop(err)
  }

  app_error <- function(error, ...) stop(error)

  send_prompt_with_retries_async_globals <- function(...) list()
  analysis_async_topic_modelling_globals <- function(...) list()
  analysis_async_tokenizer_globals <- function(...) list()
  analysis_async_worker_setup_globals <- function(...) list()
  analysis_async_processing_globals <- function(...) list()

  .kwallm__prompt_execution_reset <- function(...) invisible(NULL)
  .kwallm__prompt_execution_get <- function(...) NULL

  create_candidate_topics <- function(...) c("Candidate 1", "Candidate 2")
  reduce_topics <- function(...) c("Topic A", "", " Topic B ")

  captured_fit_topics <- NULL
  captured_assignment_topics <- NULL
  edit_topics_started <- FALSE

  topic_assignment_prompt_context_window_check <- function(
    texts,
    topics,
    research_background,
    llm_provider,
    assign_multiple_categories,
    exclusive_topics
  ) {
    force(texts)
    force(research_background)
    force(llm_provider)
    force(assign_multiple_categories)
    force(exclusive_topics)
    captured_fit_topics <<- topics
    list(fits = TRUE, prompt_tokens = 10L, context_window_tokens = 100L)
  }

  edit_topics_server <- function(...) {
    edit_topics_started <<- TRUE
    shiny::reactiveVal(NULL)
  }

  prepare_async_analysis_worker <- function(...) invisible(NULL)
  count_tokens <- function(x) {
    if (length(x) > 1) {
      return(rep(1L, length(x)))
    }

    1L
  }
  get_context_window_size_in_tokens <- function(...) 1000L

  assign_topics <- function(
    texts,
    analysis_unit_ids,
    topics,
    research_background,
    llm_provider,
    assign_multiple_categories,
    exclusive_topics,
    ...
  ) {
    force(research_background)
    force(llm_provider)
    force(assign_multiple_categories)
    force(exclusive_topics)
    captured_assignment_topics <<- topics

    data.frame(
      analysis_unit_id = analysis_unit_ids,
      text = texts,
      result = rep(topics[[1]], length(texts)),
      stringsAsFactors = FALSE
    )
  }

  join_processing_results <- function(texts_df, results_table_pre) {
    force(texts_df)
    results_table_pre
  }
  processing_results_have_invalid_na <- function(...) FALSE
  interrater_server <- function(...) {
    list(
      start = function() invisible(NULL),
      done = shiny::reactiveVal(FALSE),
      result = NULL,
      sample = NULL
    )
  }

  showNotification <- function(...) invisible(NULL)

  source(here::here("R", "module_core_processing.R"), local = TRUE)

  mirai_ns <- asNamespace("mirai")
  old_mirai_fn <- get("mirai", envir = mirai_ns)
  withr::defer({
    if (bindingIsLocked("mirai", mirai_ns)) {
      unlockBinding("mirai", mirai_ns)
    }
    assign("mirai", old_mirai_fn, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", mirai_sync_stub, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      texts <- shiny::reactiveValues(
        preprocessed = c("first text", "second text"),
        analysis_units = data.frame(analysis_unit_id = c(1L, 2L)),
        df = data.frame(
          document_text = c("first text", "second text"),
          stringsAsFactors = FALSE
        )
      )

      models <- shiny::reactiveValues(
        main = kwallm_test_llm_provider("kwallm-fake-main-1024"),
        large = kwallm_test_llm_provider("kwallm-fake-reducer-320")
      )

      categories <- list(
        texts = shiny::reactiveVal(character()),
        exclusive_texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      codes <- list(
        texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      context_window <- shiny::reactiveValues(
        any_fit_problem = FALSE,
        too_many_batches = FALSE,
        text_batches = list(c("first text", "second text"))
      )

      processing_server(
        id = "processing",
        mode = shiny::reactiveVal("Onderwerpextractie"),
        interrater_reliability_toggle = shiny::reactiveVal(TRUE),
        texts = texts,
        llm_provider_rv = shiny::reactiveValues(),
        models = models,
        categories = categories,
        scoring_characteristic = shiny::reactiveVal(""),
        codes = codes,
        research_background = shiny::reactiveVal("Background"),
        style_prompt = shiny::reactiveVal(""),
        human_in_the_loop = shiny::reactiveVal(FALSE),
        assign_multiple_categories = shiny::reactiveVal(TRUE),
        write_paragraphs = shiny::reactiveVal(FALSE),
        context_window = context_window,
        lang = lang
      )

      NULL
    },
    {
      session$setInputs(`processing-process` = 1)

      for (i in seq_len(20)) {
        later::run_now(timeout = 0)
        session$flushReact()

        if (!is.null(captured_assignment_topics)) {
          break
        }
      }

      expect_false(edit_topics_started)
      expect_identical(captured_fit_topics, c("Topic A", "Topic B"))
      expect_identical(captured_assignment_topics, c("Topic A", "Topic B"))
    }
  )
})


test_that("processing_server: reduced topics keep reduction_summary for result building", {
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  progress_bar_server <- stub_progress_bar_server
  llm_streaming_server <- stub_llm_streaming_server

  processing_texts_under_maximum <- function(...) TRUE
  processing_split_ready <- function(...) TRUE
  processing_anonymization_ready <- function(...) TRUE
  processing_has_pending_gliner_anonymization <- function(...) FALSE
  processing_results_have_invalid_na <- function(...) FALSE

  log_action <- function(...) invisible(NULL)
  log_analysis_start <- function(...) invisible(NULL)
  log_context_capture <- function(...) list()
  log_context_apply <- function(...) invisible(NULL)
  log_async_globals <- function(...) list()
  log_info <- function(...) invisible(NULL)
  log_debug <- function(...) invisible(NULL)
  log_warn <- function(...) invisible(NULL)

  handle_detailed_error <- function(...) {
    function(err) stop(err)
  }

  app_error <- function(error, ...) stop(error)

  send_prompt_with_retries_async_globals <- function(...) list()
  analysis_async_topic_modelling_globals <- function(...) list()
  analysis_async_tokenizer_globals <- function(...) list()
  analysis_async_worker_setup_globals <- function(...) list()
  analysis_async_processing_globals <- function(...) list()
  analysis_result_async_globals <- function(...) list()

  .kwallm__prompt_execution_reset <- function(...) invisible(NULL)
  .kwallm__prompt_execution_get <- function(...) NULL

  create_candidate_topics <- function(...) c("Candidate 1", "Candidate 2")
  reduce_topics <- function(...) {
    reduced_topics <- c("Topic A", "Unknown/not applicable")
    attr(reduced_topics, "reduction_summary") <- list(
      not_applicable_requested = TRUE,
      auto_added_not_applicable = FALSE,
      not_applicable_check_performed = TRUE,
      reduction_iterations = 1L
    )
    reduced_topics
  }
  assign_topics <- function(...) {
    data.frame(
      analysis_unit_id = c(1L, 2L),
      text = c("first text", "second text"),
      check.names = FALSE,
      stringsAsFactors = FALSE,
      "Topic A" = c(TRUE, FALSE),
      "Unknown/not applicable" = c(FALSE, TRUE)
    )
  }

  topic_assignment_prompt_context_window_check <- function(...) {
    list(fits = TRUE, prompt_tokens = 10L, context_window_tokens = 100L)
  }

  prepare_async_analysis_worker <- function(...) invisible(NULL)
  count_tokens <- function(x) {
    if (length(x) > 1) {
      return(rep(1L, length(x)))
    }

    1L
  }

  captured_reduction_summary <- NULL
  captured_topics_were_edited <- NULL

  build_analysis_result <- function(..., reduced_topics, topics_were_edited) {
    captured_reduction_summary <<- attr(
      reduced_topics,
      "reduction_summary",
      exact = TRUE
    )
    captured_topics_were_edited <<- topics_were_edited

    structure(list(), class = "MockAnalysisResult")
  }

  analysis_result_expected_paragraph_subject_count <- function(...) 0L
  .kwallm_report_results_df <- function(...) {
    data.frame(stringsAsFactors = FALSE)
  }

  create_analysis_result_download_bundle <- function(...) {
    path <- tempfile(fileext = ".zip")
    file.create(path)
    path
  }

  source(here::here("R", "module_core_processing.R"), local = TRUE)

  mirai_ns <- asNamespace("mirai")
  old_mirai_fn <- get("mirai", envir = mirai_ns)
  withr::defer({
    if (bindingIsLocked("mirai", mirai_ns)) {
      unlockBinding("mirai", mirai_ns)
    }
    assign("mirai", old_mirai_fn, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", mirai_sync_stub, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")

      texts <- shiny::reactiveValues(
        preprocessed = c("first text", "second text"),
        analysis_units = data.frame(analysis_unit_id = c(1L, 2L)),
        df = data.frame(
          analysis_unit_id = c(1L, 2L),
          document_text = c("first text", "second text"),
          preprocessed = c("first text", "second text"),
          stringsAsFactors = FALSE
        )
      )

      models <- shiny::reactiveValues(
        main = kwallm_test_llm_provider("kwallm-fake-main-1024"),
        large = kwallm_test_llm_provider("kwallm-fake-reducer-320")
      )

      categories <- list(
        texts = shiny::reactiveVal(character()),
        exclusive_texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      codes <- list(
        texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      context_window <- shiny::reactiveValues(
        any_fit_problem = FALSE,
        too_many_batches = FALSE,
        text_batches = list(c("first text", "second text"))
      )

      processing_server(
        id = "processing",
        mode = shiny::reactiveVal("Onderwerpextractie"),
        interrater_reliability_toggle = shiny::reactiveVal(FALSE),
        texts = texts,
        llm_provider_rv = shiny::reactiveValues(),
        models = models,
        categories = categories,
        scoring_characteristic = shiny::reactiveVal(""),
        codes = codes,
        research_background = shiny::reactiveVal("Background"),
        style_prompt = shiny::reactiveVal(""),
        human_in_the_loop = shiny::reactiveVal(FALSE),
        assign_multiple_categories = shiny::reactiveVal(TRUE),
        write_paragraphs = shiny::reactiveVal(FALSE),
        context_window = context_window,
        lang = lang
      )

      NULL
    },
    {
      session$setInputs(`processing-process` = 1)

      for (i in seq_len(50)) {
        later::run_now(timeout = 0)
        session$flushReact()

        if (!is.null(captured_reduction_summary)) {
          break
        }
      }

      expect_identical(
        captured_reduction_summary,
        list(
          not_applicable_requested = TRUE,
          auto_added_not_applicable = FALSE,
          not_applicable_check_performed = TRUE,
          reduction_iterations = 1L
        )
      )
      expect_false(isTRUE(captured_topics_were_edited))
    }
  )
})


# 4. Overflow re-validation gate -----------------------------------------------

test_that("processing_server: topics_definitive gate blocks assignment when topics still overflow", {
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  progress_bar_server <- stub_progress_bar_server
  llm_streaming_server <- stub_llm_streaming_server

  processing_texts_under_maximum <- function(...) TRUE
  processing_split_ready <- function(...) TRUE
  processing_anonymization_ready <- function(...) TRUE
  processing_has_pending_gliner_anonymization <- function(...) FALSE

  log_action <- function(...) invisible(NULL)
  log_analysis_start <- function(...) invisible(NULL)
  log_context_capture <- function(...) list()
  log_context_apply <- function(...) invisible(NULL)
  log_async_globals <- function(...) list()
  log_info <- function(...) invisible(NULL)
  log_debug <- function(...) invisible(NULL)
  log_warn <- function(...) invisible(NULL)
  log_error <- function(...) invisible(NULL)

  handle_detailed_error <- function(...) {
    function(err) stop(err)
  }

  app_error <- function(error, ...) stop(error)

  send_prompt_with_retries_async_globals <- function(...) list()
  analysis_async_topic_modelling_globals <- function(...) list()
  analysis_async_tokenizer_globals <- function(...) list()
  analysis_async_worker_setup_globals <- function(...) list()
  analysis_async_processing_globals <- function(...) list()

  .kwallm__prompt_execution_reset <- function(...) invisible(NULL)
  .kwallm__prompt_execution_get <- function(...) NULL

  create_candidate_topics <- function(...) c("Candidate 1", "Candidate 2")
  reduce_topics <- function(...) c("Topic A", "Topic B")

  assignment_started <- FALSE
  editor_returned_topics <- shiny::reactiveVal(NULL)

  # Fit check always returns overflow
  topic_assignment_prompt_context_window_check <- function(...) {
    list(fits = FALSE, prompt_tokens = 500L, context_window_tokens = 100L)
  }

  # Editor stub: immediately returns whatever was set in editor_returned_topics
  edit_topics_server <- function(...) {
    editor_returned_topics
  }

  assign_topics <- function(...) {
    assignment_started <<- TRUE
    data.frame(
      analysis_unit_id = 1:2,
      text = c("first text", "second text"),
      result = c("Topic A", "Topic B"),
      stringsAsFactors = FALSE
    )
  }

  showNotification <- function(...) invisible(NULL)

  source(here::here("R", "module_core_processing.R"), local = TRUE)

  mirai_ns <- asNamespace("mirai")
  old_mirai_fn <- get("mirai", envir = mirai_ns)
  withr::defer({
    if (bindingIsLocked("mirai", mirai_ns)) {
      unlockBinding("mirai", mirai_ns)
    }
    assign("mirai", old_mirai_fn, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  if (bindingIsLocked("mirai", mirai_ns)) {
    unlockBinding("mirai", mirai_ns)
  }
  assign("mirai", mirai_sync_stub, envir = mirai_ns)
  lockBinding("mirai", mirai_ns)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      texts <- shiny::reactiveValues(
        preprocessed = c("first text", "second text"),
        analysis_units = data.frame(analysis_unit_id = c(1L, 2L)),
        df = data.frame(
          document_text = c("first text", "second text"),
          stringsAsFactors = FALSE
        )
      )

      models <- shiny::reactiveValues(
        main = kwallm_test_llm_provider("kwallm-fake-main-1024"),
        large = kwallm_test_llm_provider("kwallm-fake-reducer-320")
      )

      categories <- list(
        texts = shiny::reactiveVal(character()),
        exclusive_texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      codes <- list(
        texts = shiny::reactiveVal(character()),
        editing = shiny::reactiveVal(FALSE),
        unique_non_empty_count = shiny::reactiveVal(0)
      )

      context_window <- shiny::reactiveValues(
        any_fit_problem = FALSE,
        too_many_batches = FALSE,
        text_batches = list(c("first text", "second text"))
      )

      processing_server(
        id = "processing",
        mode = shiny::reactiveVal("Onderwerpextractie"),
        interrater_reliability_toggle = shiny::reactiveVal(FALSE),
        texts = texts,
        llm_provider_rv = shiny::reactiveValues(),
        models = models,
        categories = categories,
        scoring_characteristic = shiny::reactiveVal(""),
        codes = codes,
        research_background = shiny::reactiveVal("Background"),
        style_prompt = shiny::reactiveVal(""),
        human_in_the_loop = shiny::reactiveVal(FALSE),
        assign_multiple_categories = shiny::reactiveVal(TRUE),
        write_paragraphs = shiny::reactiveVal(FALSE),
        context_window = context_window,
        lang = lang
      )

      list(editor_returned_topics = editor_returned_topics)
    },
    {
      # Launch processing: topic generation will succeed, but topics will
      # overflow the assignment context window → editor opens
      session$setInputs(`processing-process` = 1)

      for (i in seq_len(20)) {
        later::run_now(timeout = 0)
        session$flushReact()
      }

      # Simulate editor returning still-overflowing topics
      editor_returned_topics(c("Topic A", "Topic B"))

      for (i in seq_len(20)) {
        later::run_now(timeout = 0)
        session$flushReact()
      }

      # The re-validation gate in topics_definitive should have blocked
      # start_topic_assignment() because the fit check still returns
      # fits = FALSE
      expect_false(assignment_started)
    }
  )
})
