# Tests for mark_text_prompt() from analysis_marking.R
# Testing the prompt construction (not LLM calls)
# Note: find_matches is already well-tested in test-find_matches.R

library(testthat)

test_that("mark_text_prompt returns a usable prompt object", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "The weather is sunny and warm today.",
    code = "weather"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
})

test_that("mark_text_prompt includes code and text in prompt", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "Customer was very satisfied with the product.",
    code = "customer satisfaction"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "customer satisfaction", ignore.case = TRUE)
  expect_match(prompt_text, "Customer was very satisfied")
})

test_that("mark_text_prompt includes research background when provided", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "Sample text here.",
    code = "test code",
    research_background = "This is interview data from healthcare workers"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "healthcare workers")
  expect_match(prompt_text, "research", ignore.case = TRUE)
})

test_that("mark_text_prompt works without research background", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Empty string
  prompt1 <- mark_text_prompt(
    text = "Some text.",
    code = "some code",
    research_background = ""
  )
  expect_true(nchar(tidyprompt::construct_prompt_text(prompt1)) > 0)

  # NULL
  prompt2 <- mark_text_prompt(
    text = "Some text.",
    code = "some code",
    research_background = NULL
  )
  expect_true(nchar(tidyprompt::construct_prompt_text(prompt2)) > 0)
})

test_that("normalize_with_map maps indices correctly", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Test whitespace normalization
  result <- normalize_with_map("hello   world")
  expect_equal(result$norm, "hello world")
  expect_true(length(result$start_idx) == nchar(result$norm))
  expect_true(length(result$end_idx) == nchar(result$norm))

  # The space in normalized maps back to original positions 6-8 (the triple space)
  space_idx <- which(strsplit(result$norm, "")[[1]] == " ")
  expect_true(result$start_idx[space_idx] == 6)
  expect_true(result$end_idx[space_idx] >= 6)
})

test_that("normalize_with_map handles quote normalization", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Curly quotes should be normalized
  result <- normalize_with_map("\u2018hello\u2019") # 'hello'
  expect_equal(result$norm, "'hello'")

  # Double curly quotes
  result2 <- normalize_with_map("\u201Chello\u201D") # "hello"
  expect_equal(result2$norm, "\"hello\"")
})

test_that("normalize_with_map handles empty and NA input", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Empty string
  result_empty <- normalize_with_map("")
  expect_equal(result_empty$norm, "")
  expect_equal(length(result_empty$start_idx), 0)

  # NA
  result_na <- normalize_with_map(NA)
  expect_equal(result_na$norm, "")
  expect_equal(length(result_na$start_idx), 0)
})

test_that("normalize_for_dist is consistent with normalize_with_map", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  test_strings <- c(
    "Hello World",
    "It's  \"OK\"—really",
    "  Spaces   Everywhere  "
  )

  for (s in test_strings) {
    expect_equal(
      normalize_for_dist(s),
      normalize_with_map(s)$norm
    )
  }
})
