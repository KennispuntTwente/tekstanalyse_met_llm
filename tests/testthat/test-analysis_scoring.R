# Tests for analysis_deductive_scoring_characteristic.R
# Tests the prompt_score() function for scoring texts on characteristics

library(testthat)

test_that("prompt_score validates input arguments", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  # Valid input
  expect_no_error(prompt_score(
    text = "This is a sample text",
    research_background = "Customer feedback survey",
    scoring_characteristic = "positive sentiment"
  ))

  # Invalid: non-character text
  expect_error(prompt_score(
    text = 123,
    research_background = "bg",
    scoring_characteristic = "sentiment"
  ))

  # Invalid: multiple texts
  expect_error(prompt_score(
    text = c("text1", "text2"),
    research_background = "bg",
    scoring_characteristic = "sentiment"
  ))

  # Invalid: non-character scoring_characteristic
  expect_error(prompt_score(
    text = "test",
    research_background = "bg",
    scoring_characteristic = 123
  ))

  # Invalid: multiple scoring_characteristics
  expect_error(prompt_score(
    text = "test",
    research_background = "bg",
    scoring_characteristic = c("a", "b")
  ))
})

test_that("prompt_score returns a usable prompt object", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  prompt <- prompt_score(
    text = "I absolutely love this product!",
    research_background = "Customer reviews",
    scoring_characteristic = "positive sentiment"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
})

test_that("prompt_score includes all parameters in prompt text", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  prompt <- prompt_score(
    text = "The weather is nice today",
    research_background = "Daily mood survey",
    scoring_characteristic = "happiness level"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)

  # Check that all elements are present

  expect_match(prompt_text, "weather is nice", fixed = TRUE)
  expect_match(prompt_text, "Daily mood survey", fixed = TRUE)
  expect_match(prompt_text, "happiness level", fixed = TRUE)
  expect_match(prompt_text, "0-100", fixed = TRUE)
})

test_that("prompt_score works with empty research background", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  prompt <- prompt_score(
    text = "Sample text",
    research_background = "",
    scoring_characteristic = "clarity"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(nchar(prompt_text) > 0)
  expect_match(prompt_text, "clarity", fixed = TRUE)
})
