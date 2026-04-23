# Tests for topic reduction logic in analysis_inductive_topic_modelling.R
# Tests prompt construction and batching logic (not LLM calls)

library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

test_that("prompt_reduce_topics (via reduce_topics logic) creates valid prompt", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  # Test prompt_candidate_topics for different inputs
  topics <- c("Customer service", "Product quality", "Shipping speed")
  text_batch <- c("Great product!", "Fast delivery")

  prompt <- prompt_candidate_topics(
    text_batch = text_batch,
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
  expect_match(prompt_text, "topics", ignore.case = TRUE)
})


test_that("prompt_candidate_topics handles empty research background", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_batch = c("Test text"),
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
    text_batch = c("Test text"),
    research_background = "Customer satisfaction study 2024",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "Customer satisfaction study 2024")
})

test_that("prompt_candidate_topics formats text batches with XML tags", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_batch = c("First text", "Second text"),
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "<text 1>")
  expect_match(prompt_text, "</text 1>")
  expect_match(prompt_text, "<text 2>")
  expect_match(prompt_text, "</text 2>")
})
