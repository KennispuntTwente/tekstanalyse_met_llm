# Tests for analysis_inductive_topic_modelling.R

library(testthat)

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
