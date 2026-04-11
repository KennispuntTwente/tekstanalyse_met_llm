# Tests for the marking__max_combinations safety guard in analysis_marking.R

library(testthat)

source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

test_that("mark_texts stops when chunk x code combinations exceed the limit", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  # Stub out Python-dependent semchunk: each text becomes a single chunk

  semchunk_load_chunker <- function(...) {
    function(text, ...) text
  }

  count_tokens <- function(x) rep(1L, length(x))
  get_context_window_size_in_tokens <- function(...) 100000L
  log_info <- function(...) invisible(NULL)

  provider <- kwallm_test_llm_provider("fake-model")

  # 3 texts x 4 codes = 12 combinations. set limit to 10 → should error.
  withr::with_options(
    list(marking__max_combinations = 10L),
    {
      expect_error(
        mark_texts(
          texts = c("first text", "second text", "third text"),
          analysis_unit_ids = 1:3,
          codes = c("A", "B", "C", "D"),
          llm_provider = provider,
          write_paragraphs = FALSE,
          text_size_tokens = 512,
          overlap_size_tokens = 0
        ),
        regexp = "exceeds the safety limit of 10",
        fixed = TRUE
      )
    }
  )
})

test_that("mark_texts proceeds when combinations are within the limit", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  semchunk_load_chunker <- function(...) {
    function(text, ...) text
  }

  count_tokens <- function(x) rep(1L, length(x))
  get_context_window_size_in_tokens <- function(...) 100000L
  log_info <- function(...) invisible(NULL)

  # Stub send_prompt_with_retries to avoid real LLM calls.
  # Return NULL to simulate "no match".
  send_prompt_with_retries <- function(...) NULL

  provider <- kwallm_test_llm_provider("fake-model")

  # 2 texts x 1 code = 2 combinations; limit is 10 → should not error.
  withr::with_options(
    list(marking__max_combinations = 10L),
    {
      result <- mark_texts(
        texts = c("The sky is blue", "The grass is green"),
        analysis_unit_ids = 1:2,
        codes = "nature",
        llm_provider = provider,
        write_paragraphs = FALSE,
        text_size_tokens = 512,
        overlap_size_tokens = 0
      )

      expect_s3_class(result, "data.frame")
      expect_true("code" %in% names(result))
      expect_true("marked_text" %in% names(result))
      expect_true(nrow(result) >= 2)
    }
  )
})

test_that("mark_texts uses default limit of 50000 when option is not set", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  semchunk_load_chunker <- function(...) {
    function(text, ...) text
  }

  count_tokens <- function(x) rep(1L, length(x))
  get_context_window_size_in_tokens <- function(...) 100000L
  log_info <- function(...) invisible(NULL)
  send_prompt_with_retries <- function(...) NULL

  provider <- kwallm_test_llm_provider("fake-model")

  # 2 texts x 2 codes = 4 combinations; default limit 50000 → should work.
  withr::with_options(
    list(marking__max_combinations = NULL),
    {
      result <- mark_texts(
        texts = c("text one", "text two"),
        analysis_unit_ids = 1:2,
        codes = c("A", "B"),
        llm_provider = provider,
        write_paragraphs = FALSE,
        text_size_tokens = 512,
        overlap_size_tokens = 0
      )

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 4)
    }
  )
})
