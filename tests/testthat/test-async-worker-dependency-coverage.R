library(testthat)

async_worker_task_entry_points <- list(
  text_split = c("split_texts_with_semchunk", "semchunk_load_chunker"),
  gliner = c("gliner_load_model", "initialize_python_environment"),
  categorization = c(
    "categorize_texts",
    "write_grouped_paragraphs",
    "send_prompt_with_retries"
  ),
  scoring = c("score_texts", "send_prompt_with_retries"),
  topic_generation = c(
    "create_candidate_topics",
    "reduce_topics",
    "send_prompt_with_retries"
  ),
  topic_reduction = c("reduce_topics", "send_prompt_with_retries"),
  topic_assignment = c(
    "assign_topics",
    "write_grouped_paragraphs",
    "send_prompt_with_retries"
  ),
  code_generation = c(
    "generate_codes_by_reading_texts",
    "create_text_batches",
    "send_prompt_with_retries"
  ),
  marking = c(
    "mark_texts",
    ".kwallm_marking_matches_from_find_matches",
    "send_prompt_with_retries"
  ),
  download_bundle = c("create_analysis_result_download_bundle"),
  model_provider_test = c(
    "run_model_provider_test",
    "send_prompt_with_retries",
    "log_context_apply"
  ),
  llm_provider_models_fetch = c("log_context_apply")
)

test_that("bootstrap exposes task entry points in a real mirai worker", {
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  kwallm_test_start_mirai_daemons(n = 1L)

  for (task in names(async_worker_task_entry_points)) {
    worker <- mirai::mirai(
      {
        kwallm_worker_bootstrap(
          task = task,
          app_root = app_root,
          worker_options = worker_options
        )

        list(
          task = getOption("kwallm__worker_task"),
          functions_present = vapply(
            required_functions,
            exists,
            logical(1),
            envir = environment(),
            inherits = TRUE
          )
        )
      },
      .args = c(
        list(
          task = task,
          required_functions = async_worker_task_entry_points[[task]],
          app_root = kwallm_worker_app_root(),
          worker_options = kwallm_worker_capture_options()
        ),
        kwallm_worker_bootstrap_globals()
      )
    )

    result <- worker[]

    if (mirai::is_error_value(result)) {
      fail(paste("mirai worker error:", as.character(result)))
    }

    expect_identical(result$task, task)
    expect_true(
      all(result$functions_present),
      info = sprintf(
        "Task '%s' is missing [%s] after bootstrap",
        task,
        paste(
          names(result$functions_present)[!result$functions_present],
          collapse = ", "
        )
      )
    )
  }
})

test_that("marking helper empty-match path works in a bootstrap worker", {
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  kwallm_test_start_mirai_daemons(n = 1L)

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "marking",
        app_root = app_root,
        worker_options = worker_options
      )

      .kwallm_marking_matches_from_find_matches(
        tibble::tibble(
          needle = character(),
          match = character(),
          start = integer(),
          end = integer(),
          distance = integer()
        ),
        response_status = "matched_all"
      )
    },
    .args = c(
      list(
        app_root = kwallm_worker_app_root(),
        worker_options = kwallm_worker_capture_options()
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]

  if (mirai::is_error_value(result)) {
    fail(paste("mirai worker error:", as.character(result)))
  }

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 1L)
  expect_identical(result$response_status[[1]], "matched_all")
  expect_true(all(is.na(result$marked_text)))
})

test_that("marking worker errors preserve chunk, code, and provider cause", {
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  kwallm_test_start_mirai_daemons(n = 1L)

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "marking",
        app_root = app_root,
        worker_options = worker_options
      )

      semchunk_load_chunker <- function(...) {
        function(text, ...) text
      }
      count_tokens <- function(x) nchar(x)
      get_context_window_size_in_tokens <- function(...) 100000L
      log_info <- function(...) invisible(NULL)
      provider_error <- paste0(
        "Invalid parameter: 'response_format' of type 'json_schema' ",
        "is not supported with this model."
      )
      send_prompt_with_retries <- function(...) {
        stop(provider_error)
      }

      mark_texts(
        texts = "An interview excerpt",
        analysis_unit_ids = 17L,
        codes = "Interview",
        text_size_tokens = 987L,
        overlap_size_tokens = 0L,
        llm_provider = list(parameters = list(model = "test-model")),
        write_paragraphs = FALSE,
        lang = NULL
      )
    },
    .args = c(
      list(
        app_root = kwallm_worker_app_root(),
        worker_options = kwallm_worker_capture_options()
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]

  expect_true(mirai::is_error_value(result))
  expect_match(
    as.character(result),
    paste0(
      "Marking failed for analysis_unit_id=17, chunk_id=1, ",
      "chunk_index=1, code='Interview'."
    ),
    fixed = TRUE
  )
  expect_match(
    as.character(result),
    paste0(
      "Provider error: Invalid parameter: 'response_format' of type ",
      "'json_schema' is not supported with this model."
    ),
    fixed = TRUE
  )
})

test_that("topic reduction worker preserves batch context and provider cause", {
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  kwallm_test_start_mirai_daemons(n = 1L)

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "topic_reduction",
        app_root = app_root,
        worker_options = worker_options
      )

      count_tokens <- function(...) 1L
      get_context_window_size_in_tokens <- function(...) 100000L
      log_info <- function(...) invisible(NULL)
      send_prompt_with_retries <- function(...) {
        stop("PROVIDER_ERROR_SENTINEL", call. = FALSE)
      }

      reduce_topics(
        candidate_topics = c("Topic A", "Topic B"),
        llm_provider = list(parameters = list(model = "test-model")),
        language = "en",
        always_add_not_applicable = FALSE
      )
    },
    .args = c(
      list(
        app_root = kwallm_worker_app_root(),
        worker_options = kwallm_worker_capture_options()
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]

  expect_true(mirai::is_error_value(result))
  expect_match(
    as.character(result),
    "Topic reduction failed for iteration=1, batch_index=1.",
    fixed = TRUE
  )
  expect_match(
    as.character(result),
    "Cause: PROVIDER_ERROR_SENTINEL",
    fixed = TRUE
  )
  expect_false(grepl("purrr_error_indexed", as.character(result), fixed = TRUE))
})
