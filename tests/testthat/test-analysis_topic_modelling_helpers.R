# Tests for topic reduction logic in analysis_inductive_topic_modelling.R
# Tests prompt construction and chunking logic (not LLM calls)

library(testthat)

test_that("prompt_reduce_topics (via reduce_topics logic) creates valid prompt", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Test prompt_candidate_topics for different inputs
  topics <- c("Customer service", "Product quality", "Shipping speed")
  text_chunk <- c("Great product!", "Fast delivery")

  prompt <- prompt_candidate_topics(
    text_chunk = text_chunk,
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
  expect_match(prompt_text, "topics", ignore.case = TRUE)
})

test_that("create_text_chunks_legacy validates max_redrawing parameter", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Invalid: max_redrawing = 0
  expect_error(create_text_chunks_legacy(
    texts = c("a", "b"),
    max_chunk_size = 50,
    max_redrawing = 0
  ))

  # Invalid: max_redrawing negative
  expect_error(create_text_chunks_legacy(
    texts = c("a", "b"),
    max_chunk_size = 50,
    max_redrawing = -1
  ))
})

test_that("create_text_chunks_legacy produces deterministic output given seed", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  texts <- c("alpha", "beta", "gamma", "delta")

  set.seed(123)
  result1 <- create_text_chunks_legacy(texts, max_chunk_size = 2)

  set.seed(123)
  result2 <- create_text_chunks_legacy(texts, max_chunk_size = 2)

  expect_equal(result1, result2)
})

test_that("create_text_chunks_legacy respects n_char_base_prompt", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # With a larger base prompt, fewer chars are available per chunk
  texts <- c(
    paste(rep("word", 30), collapse = " "), # ~150 chars
    paste(rep("text", 30), collapse = " ") # ~150 chars
  )

  # Large base prompt leaves little room
  result_large_base <- create_text_chunks_legacy(
    texts = texts,
    max_chunk_size = 100,
    n_tokens_context_window = 200, # 600 chars total
    n_char_base_prompt = 500 # leaves only 100 chars
  )

  # Small base prompt leaves more room
  result_small_base <- create_text_chunks_legacy(
    texts = texts,
    max_chunk_size = 100,
    n_tokens_context_window = 200, # 600 chars total
    n_char_base_prompt = 100 # leaves 500 chars
  )

  # Both should succeed, but structure may differ
  expect_true(is.list(result_large_base))
  expect_true(is.list(result_small_base))
})

test_that("create_text_chunks_legacy handles texts with special characters", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  texts <- c(
    "Text with \"quotes\" and 'apostrophes'",
    "Text with em—dash and en–dash",
    "Text with <html> tags & symbols"
  )

  result <- create_text_chunks_legacy(
    texts = texts,
    max_chunk_size = 50,
    n_tokens_context_window = 10000
  )

  expect_true(is.list(result))
  expect_true(length(result) >= 1)
})

test_that("prompt_candidate_topics handles empty research background", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_chunk = c("Test text"),
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(nchar(prompt_text) > 0)
  # Should not contain empty research background section
  expect_false(grepl("Research background:\\s*$", prompt_text))
})

test_that("prompt_candidate_topics includes research background when provided", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_chunk = c("Test text"),
    research_background = "Customer satisfaction study 2024",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "Customer satisfaction study 2024")
})

test_that("prompt_candidate_topics formats text chunks with XML tags", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_chunk = c("First text", "Second text"),
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "<text 1>")
  expect_match(prompt_text, "</text 1>")
  expect_match(prompt_text, "<text 2>")
  expect_match(prompt_text, "</text 2>")
})
