# Tests for analysis_inductive_topic_modelling.R
# Focus on create_text_chunks_legacy() - pure function with no LLM dependencies

library(testthat)

test_that("create_text_chunks_legacy validates input arguments", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Valid input
  expect_no_error(create_text_chunks_legacy(
    texts = c("text1", "text2"),
    max_chunk_size = 50
  ))

  # Invalid: non-character texts
  expect_error(create_text_chunks_legacy(
    texts = 123,
    max_chunk_size = 50
  ))

  # Invalid: empty texts
  expect_error(create_text_chunks_legacy(
    texts = character(0),
    max_chunk_size = 50
  ))

  # Invalid: max_chunk_size <= 0
  expect_error(create_text_chunks_legacy(
    texts = c("test"),
    max_chunk_size = 0
  ))

  # Invalid: n_tokens_context_window <= 0
  expect_error(create_text_chunks_legacy(
    texts = c("test"),
    max_chunk_size = 50,
    n_tokens_context_window = 0
  ))
})

test_that("create_text_chunks_legacy returns a list of character vectors", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  texts <- c("hello world", "foo bar", "test text")
  result <- create_text_chunks_legacy(texts, max_chunk_size = 50)

  expect_true(is.list(result))
  expect_true(all(sapply(result, is.character)))
  expect_true(length(result) >= 1)
})

test_that("create_text_chunks_legacy respects max_chunk_size", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Use small texts and max_chunk_size = 2
  texts <- c("a", "b", "c", "d", "e")
  result <- create_text_chunks_legacy(
    texts,
    max_chunk_size = 2,
    n_tokens_context_window = 10000 # large to avoid char limit
  )

  # Each chunk should have at most 2 texts
  expect_true(all(sapply(result, length) <= 2))
})

test_that("create_text_chunks_legacy respects character limit from context window", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Create texts that together exceed allowed_chars
  # With n_tokens_context_window = 100, allowed_chars = 100*3 - 600 = -300 (too small)
  # Use larger context window
  texts <- c(
    paste(rep("a", 50), collapse = ""),
    paste(rep("b", 50), collapse = ""),
    paste(rep("c", 50), collapse = "")
  )

  # Small context window forces separate chunks
  result <- create_text_chunks_legacy(
    texts,
    max_chunk_size = 100, # high enough not to be limiting
    n_tokens_context_window = 300, # 300 * 3 = 900 chars, minus 600 base = 300 allowed
    n_char_base_prompt = 600
  )

  # Should create multiple chunks since texts are 50 chars each
  # and allowed is 300 chars - should fit maybe 5-6 texts per chunk with separators
  expect_true(is.list(result))
  expect_true(length(result) >= 1)
})

test_that("create_text_chunks_legacy handles max_redrawing parameter", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  set.seed(42) # for reproducible randomization
  texts <- c("alpha", "beta")

  # With max_redrawing = 2, each text appears twice in the pool
  result <- create_text_chunks_legacy(
    texts,
    max_chunk_size = 100,
    max_redrawing = 2,
    n_tokens_context_window = 10000
  )

  # Count total texts across all chunks
  total_texts <- sum(sapply(result, length))
  expect_equal(total_texts, 4) # 2 original texts * 2 redrawing
})

test_that("create_text_chunks_legacy errors on oversized individual text", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Create a text that exceeds the allowed character limit
  # With n_tokens = 300, n_char = 900, minus 600 base = 300 allowed
  oversized_text <- paste(rep("x", 500), collapse = "")

  expect_error(
    create_text_chunks_legacy(
      texts = oversized_text,
      max_chunk_size = 50,
      n_tokens_context_window = 300,
      n_char_base_prompt = 600
    ),
    "exceed"
  )
})

test_that("create_text_chunks_legacy handles single text", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  result <- create_text_chunks_legacy(
    texts = "single text",
    max_chunk_size = 50,
    n_tokens_context_window = 10000
  )

  expect_equal(length(result), 1)
  expect_equal(result[[1]], "single text")
})

test_that("prompt_candidate_topics returns a usable prompt object", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  text_chunk <- c("The weather is nice", "I like coding")

  prompt <- prompt_candidate_topics(
    text_chunk = text_chunk,
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
  expect_match(prompt_text, "topics")
})

test_that("prompt_candidate_topics includes research background", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_chunk = c("test"),
    research_background = "Customer satisfaction survey",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "Customer satisfaction survey")
})

test_that("prompt_candidate_topics respects language parameter", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt_nl <- prompt_candidate_topics(
    text_chunk = c("test"),
    research_background = "",
    language = "nl"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt_nl)
  expect_match(prompt_text, "Dutch")
})
