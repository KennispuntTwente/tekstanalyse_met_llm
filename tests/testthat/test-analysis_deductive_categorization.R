# Tests for analysis_deductive_categorization.R
# These test the prompt-building functions and their input validation
# The extraction logic is tested indirectly via end-to-end shinytest2 tests

library(testthat)

test_that("prompt_category validates input arguments", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  # Valid input
  expect_no_error(prompt_category(
    text = "test text",
    research_background = "background",
    categories = c("cat1", "cat2")
  ))

  # Invalid: non-character text
  expect_error(prompt_category(
    text = 123,
    research_background = "bg",
    categories = c("a", "b")
  ))

  # Invalid: empty categories
  expect_error(prompt_category(
    text = "test",
    research_background = "bg",
    categories = character(0)
  ))

  # Invalid: duplicate categories
  expect_error(prompt_category(
    text = "test",
    research_background = "bg",
    categories = c("cat1", "cat1")
  ))

  # Invalid: multiple texts
  expect_error(prompt_category(
    text = c("text1", "text2"),
    research_background = "",
    categories = c("a", "b")
  ))
})

test_that("prompt_category returns a usable prompt object", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  categories <- c("positive", "negative", "neutral")
  prompt <- prompt_category(
    text = "I love this product!",
    research_background = "",
    categories = categories
  )

  # Verify it returns something we can construct prompt text from
  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
})

test_that("prompt_multi_category validates input and exclusive categories", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  # Valid input with exclusive category
  expect_no_error(prompt_multi_category(
    text = "test",
    categories = c("a", "b", "unclear"),
    exclusive_categories = c("unclear")
  ))

  # Invalid: exclusive category not in categories
  expect_error(prompt_multi_category(
    text = "test",
    categories = c("a", "b"),
    exclusive_categories = c("not_present")
  ))

  # Invalid: duplicate categories
  expect_error(prompt_multi_category(
    text = "test",
    categories = c("a", "a", "b"),
    exclusive_categories = character(0)
  ))
})

test_that("prompt_multi_category returns a usable prompt object", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  categories <- c("positive", "negative", "unclear")
  prompt <- prompt_multi_category(
    text = "test",
    research_background = "",
    categories = categories,
    exclusive_categories = c("unclear")
  )

  # Verify it returns something we can construct prompt text from
  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
})

test_that("prompt_category includes research background when provided", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  prompt_with_bg <- prompt_category(
    text = "test",
    research_background = "This is customer feedback research",
    categories = c("a", "b")
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt_with_bg)
  expect_match(prompt_text, "Research background")
  expect_match(prompt_text, "customer feedback research")
})

test_that("prompt_multi_category includes exclusive category annotation", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  prompt <- prompt_multi_category(
    text = "test",
    categories = c("positive", "negative", "unclear"),
    exclusive_categories = c("unclear")
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "\\[exclusive\\]")
  expect_match(prompt_text, "unclear")
})
