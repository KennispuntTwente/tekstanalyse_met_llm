library(testthat)


count_pattern_occurrences <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]

  if (length(matches) == 1L && identical(matches[[1]], -1L)) {
    return(0L)
  }

  length(matches)
}


test_that("kwallm_worker_bootstrap loads app functions and worker options in a real mirai worker", {
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)

  old_opts <- options(
    app__mode = "test",
    kwallm.test_fake_llm = TRUE,
    send_prompt_with_retries__max_tries = 7L,
    topic_modelling__always_add_not_applicable = FALSE,
    topic_modelling__reduction_max_prompt_batches = 24L,
    topic_modelling__reduction_max_iterations = 3L,
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)

  can_start_daemons <- TRUE
  tryCatch(
    {
      mirai::daemons(1)
      on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
    },
    error = function(e) {
      can_start_daemons <<- FALSE
    }
  )
  if (!isTRUE(can_start_daemons)) {
    skip("mirai daemons not available in this environment")
  }

  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "bootstrap_smoke",
        app_root = app_root,
        worker_options = worker_options
      )

      list(
        function_presence = vapply(
          required_functions,
          exists,
          logical(1),
          envir = environment(),
          inherits = TRUE
        ),
        option_values = stats::setNames(
          lapply(option_names, getOption),
          option_names
        ),
        working_directory = normalizePath(
          getwd(),
          winslash = "/",
          mustWork = TRUE
        )
      )
    },
    .args = c(
      list(
        app_root = kwallm_worker_app_root(),
        worker_options = kwallm_worker_capture_options(),
        required_functions = c(
          "categorize_texts",
          "score_texts",
          "reduce_topics",
          "assign_topics",
          "mark_texts",
          "split_texts_with_semchunk",
          "generate_codes_by_reading_texts",
          "gliner_load_model",
          "create_analysis_result_download_bundle",
          "run_model_provider_test",
          "send_prompt_with_retries",
          "log_context_apply"
        ),
        option_names = c(
          "app__mode",
          "kwallm.test_fake_llm",
          "send_prompt_with_retries__max_tries",
          "topic_modelling__always_add_not_applicable",
          "topic_modelling__reduction_max_prompt_batches",
          "topic_modelling__reduction_max_iterations"
        )
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]

  if (mirai::is_error_value(result)) {
    fail(paste("mirai worker bootstrap error:", as.character(result)))
  }

  expect_true(all(result$function_presence))
  expect_identical(result$option_values$app__mode, "test")
  expect_identical(result$option_values$kwallm.test_fake_llm, TRUE)
  expect_identical(result$option_values$send_prompt_with_retries__max_tries, 7L)
  expect_identical(
    result$option_values$topic_modelling__always_add_not_applicable,
    FALSE
  )
  expect_identical(
    result$option_values$topic_modelling__reduction_max_prompt_batches,
    24L
  )
  expect_identical(
    result$option_values$topic_modelling__reduction_max_iterations,
    3L
  )
  expect_identical(result$working_directory, kwallm_worker_app_root())
})


test_that("production mirai call sites all use kwallm worker bootstrap", {
  worker_files <- c(
    "R/module_core_processing.R",
    "R/module_input_text_split.R",
    "R/module_input_marking_codes.R",
    "R/module_misc_edit_topics.R",
    "R/module_misc_gliner_anonymization.R",
    "R/module_config_model.R",
    "R/module_config_llm_provider.R"
  )

  for (path in worker_files) {
    text <- paste(readLines(here::here(path), warn = FALSE), collapse = "\n")
    mirai_count <- count_pattern_occurrences(text, "mirai::mirai\\(")
    bootstrap_call_count <- count_pattern_occurrences(
      text,
      "kwallm_worker_bootstrap\\("
    )
    bootstrap_globals_count <- count_pattern_occurrences(
      text,
      "kwallm_worker_bootstrap_globals\\("
    )

    expect_true(
      mirai_count > 0L,
      info = sprintf("Expected at least one mirai call in %s", path)
    )
    expect_identical(
      bootstrap_call_count,
      mirai_count,
      info = sprintf("Every mirai call in %s should bootstrap the worker", path)
    )
    expect_identical(
      bootstrap_globals_count,
      mirai_count,
      info = sprintf(
        "Every mirai call in %s should export the bootstrap helper",
        path
      )
    )
  }
})
