test_that("async_inject_dependencies binds worker dependencies in a child env", {
  worker_env <- new.env(parent = emptyenv())
  worker_env$dep_a <- function() "a"
  worker_env$dep_b <- function() "b"
  worker_env$target <- local(
    {
      function() paste(dep_a(), dep_b(), sep = "-")
    },
    envir = new.env(parent = baseenv())
  )

  original_env <- environment(worker_env$target)

  async_inject_dependencies(
    bindings = list(target = c("dep_a", "dep_b")),
    env = worker_env
  )

  expect_identical(worker_env$target(), "a-b")
  expect_false(identical(environment(worker_env$target), original_env))
  expect_identical(parent.env(environment(worker_env$target)), original_env)
  expect_true(exists("dep_a", envir = environment(worker_env$target)))
  expect_true(exists("dep_b", envir = environment(worker_env$target)))
})


test_that("prepare_async_analysis_worker injects the expected dependencies", {
  categorization_env <- new.env(parent = emptyenv())

  categorization_env$async_message_printer <- function(...) NULL
  categorization_env$send_prompt_with_retries <- function(...) NULL
  categorization_env$get_context_window_size_in_tokens <- function(...) 1024
  categorization_env$count_tokens <- function(...) 1
  categorization_env$tiktoken_load_tokenizer <- function(...) NULL
  categorization_env$prompt_category <- function(...) NULL
  categorization_env$prompt_multi_category <- function(...) NULL
  categorization_env$write_paragraph <- function(...) NULL
  categorization_env$categorize_texts <- function(...) NULL

  prepare_async_analysis_worker(
    task = "categorization",
    env = categorization_env
  )

  expect_true(exists(
    "async_message_printer",
    envir = environment(categorization_env$tiktoken_load_tokenizer)
  ))
  expect_true(exists(
    "tiktoken_load_tokenizer",
    envir = environment(categorization_env$count_tokens)
  ))
  expect_true(exists(
    "send_prompt_with_retries",
    envir = environment(categorization_env$write_paragraph)
  ))
  expect_true(exists(
    "prompt_category",
    envir = environment(categorization_env$categorize_texts)
  ))

  scoring_env <- new.env(parent = emptyenv())
  scoring_env$send_prompt_with_retries <- function(...) NULL
  scoring_env$prompt_score <- function(...) NULL
  scoring_env$score_texts <- function(...) NULL

  prepare_async_analysis_worker(
    task = "scoring",
    env = scoring_env
  )

  expect_true(exists(
    "prompt_score",
    envir = environment(scoring_env$score_texts)
  ))

  marking_env <- new.env(parent = emptyenv())
  marking_env$async_message_printer <- function(...) NULL
  marking_env$log_info <- function(...) NULL
  marking_env$send_prompt_with_retries <- function(...) NULL
  marking_env$get_context_window_size_in_tokens <- function(...) 1024
  marking_env$count_tokens <- function(...) 1
  marking_env$tiktoken_load_tokenizer <- function(...) NULL
  marking_env$semchunk_load_chunker <- function(...) NULL
  marking_env$mark_text_prompt <- function(...) NULL
  marking_env$write_paragraph <- function(...) NULL
  marking_env$.kwallm_empty_marking_matches <- function(...) NULL
  marking_env$.kwallm_marking_status_row <- function(...) NULL
  marking_env$.kwallm_marking_matches_from_find_matches <- function(...) NULL
  marking_env$.kwallm_normalize_marking_matches <- function(...) NULL
  marking_env$.kwallm_marking_find_absolute_span <- function(...) NULL
  marking_env$.kwallm_marking_clean_results <- function(...) NULL
  marking_env$.kwallm_marking_build_highlighted_excerpt <- function(...) NULL
  marking_env$.kwallm_marking_collect_paragraph_inputs <- function(...) NULL
  marking_env$find_matches <- function(...) NULL
  marking_env$normalize_with_map <- function(...) NULL
  marking_env$best_literal_substring <- function(...) NULL
  marking_env$fuzzy_threshold <- function(...) NULL
  marking_env$normalize_for_dist <- function(...) NULL
  marking_env$mark_texts <- function(...) NULL

  prepare_async_analysis_worker(
    task = "marking",
    env = marking_env
  )

  expect_true(exists(
    "async_message_printer",
    envir = environment(marking_env$semchunk_load_chunker)
  ))
  expect_true(exists(
    "semchunk_load_chunker",
    envir = environment(marking_env$mark_texts)
  ))
  expect_true(exists(
    "log_info",
    envir = environment(marking_env$mark_texts)
  ))
  expect_true(exists(
    "find_matches",
    envir = environment(marking_env$mark_text_prompt)
  ))
  expect_true(exists(
    ".kwallm_marking_matches_from_find_matches",
    envir = environment(marking_env$mark_text_prompt)
  ))
  expect_true(exists(
    "best_literal_substring",
    envir = environment(marking_env$find_matches)
  ))
})


test_that("analysis async globals helpers expose the expected names", {
  helper_env <- environment(analysis_async_tokenizer_globals)
  stub_names <- c(
    "get_context_window_size_in_tokens",
    "tiktoken_load_tokenizer",
    "count_tokens",
    "async_message_printer",
    "categorize_texts",
    "prompt_category",
    "prompt_multi_category",
    "score_texts",
    "prompt_score",
    "collect_grouped_texts",
    "collect_grouped_paragraph_inputs",
    "write_grouped_paragraphs",
    "mark_texts",
    "mark_text_prompt",
    "semchunk_load_chunker",
    "write_paragraph",
    ".kwallm_empty_marking_matches",
    ".kwallm_marking_status_row",
    ".kwallm_marking_matches_from_find_matches",
    ".kwallm_normalize_marking_matches",
    ".kwallm_marking_find_absolute_span",
    ".kwallm_marking_clean_results",
    ".kwallm_marking_build_highlighted_excerpt",
    ".kwallm_marking_collect_paragraph_inputs",
    "find_matches",
    "normalize_with_map",
    "best_literal_substring",
    "fuzzy_threshold",
    "normalize_for_dist"
  )
  old_bindings <- mget(
    stub_names,
    envir = helper_env,
    inherits = FALSE,
    ifnotfound = vector("list", length(stub_names))
  )

  for (name in stub_names) {
    assign(name, function(...) NULL, envir = helper_env)
  }
  withr::defer({
    for (i in seq_along(stub_names)) {
      name <- stub_names[[i]]
      value <- old_bindings[[i]]
      if (is.null(value)) {
        rm(list = name, envir = helper_env)
      } else {
        assign(name, value, envir = helper_env)
      }
    }
  })

  expect_named(
    analysis_async_tokenizer_globals(),
    c(
      "get_context_window_size_in_tokens",
      "tiktoken_load_tokenizer",
      "count_tokens",
      "async_message_printer"
    )
  )

  expect_named(
    analysis_async_processing_globals(),
    c(
      "collect_grouped_texts",
      "collect_grouped_paragraph_inputs",
      "write_grouped_paragraphs",
      "write_paragraph"
    )
  )

  expect_named(
    analysis_async_categorization_globals(),
    c(
      "categorize_texts",
      "prompt_category",
      "prompt_multi_category",
      "write_paragraph"
    )
  )

  expect_named(
    analysis_async_scoring_globals(),
    c(
      "score_texts",
      "prompt_score"
    )
  )

  expect_named(
    analysis_async_marking_globals(),
    c(
      "mark_texts",
      "mark_text_prompt",
      "semchunk_load_chunker",
      "write_paragraph",
      ".kwallm_empty_marking_matches",
      ".kwallm_marking_status_row",
      ".kwallm_marking_matches_from_find_matches",
      ".kwallm_normalize_marking_matches",
      ".kwallm_marking_find_absolute_span",
      ".kwallm_marking_clean_results",
      ".kwallm_marking_build_highlighted_excerpt",
      ".kwallm_marking_collect_paragraph_inputs",
      "find_matches",
      "normalize_with_map",
      "best_literal_substring",
      "fuzzy_threshold",
      "normalize_for_dist"
    )
  )

  worker_setup_globals <- analysis_async_worker_setup_globals()
  expect_named(worker_setup_globals, "prepare_async_analysis_worker")
  expect_true(exists(
    "async_inject_dependencies",
    envir = environment(worker_setup_globals$prepare_async_analysis_worker),
    inherits = TRUE
  ))
  expect_true(exists(
    ".analysis_async_dependency_map",
    envir = environment(worker_setup_globals$prepare_async_analysis_worker),
    inherits = TRUE
  ))
})
