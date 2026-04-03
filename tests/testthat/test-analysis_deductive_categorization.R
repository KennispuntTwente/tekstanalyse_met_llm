# Tests for analysis_deductive_categorization.R
# These test the prompt-building functions and their input validation
# The extraction logic is tested indirectly via end-to-end shinytest2 tests

library(testthat)

create_test_provider <- function(model = "test-model") {
  provider <- list(parameters = list(model = model))
  provider$clone <- function() provider
  provider
}

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

test_that("categorize_texts returns binary columns for multi-label output", {
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  send_prompt_with_retries <- function(prompt, llm_provider) {
    force(prompt)
    force(llm_provider)

    if (
      grepl("text a", tidyprompt::construct_prompt_text(prompt), fixed = TRUE)
    ) {
      return("cat1")
    }

    c("cat1", "cat2")
  }

  result <- categorize_texts(
    texts = c("text a", "text b"),
    categories = c("cat1", "cat2"),
    llm_provider = create_test_provider(),
    assign_multiple_categories = TRUE
  )

  expect_false("result" %in% names(result))
  expect_identical(result$text, c("text a", "text b"))
  expect_identical(result$cat1, c(TRUE, TRUE))
  expect_identical(result$cat2, c(FALSE, TRUE))
})

test_that("categorize_texts supports progress, interruption, and early NA", {
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)

  call_count <- 0
  interrupt_count <- 0
  progress_events <- list()

  send_prompt_with_retries <- function(prompt, llm_provider) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("cat1")
    }

    NA_character_
  }

  interrupter <- list(
    execInterrupts = function() {
      interrupt_count <<- interrupt_count + 1
    }
  )

  result <- categorize_texts(
    texts = c("text a", "text b", "text c"),
    categories = c("cat1", "cat2"),
    llm_provider = create_test_provider(),
    on_progress = function(i, n, text) {
      progress_events[[length(progress_events) + 1]] <<- list(
        i = i,
        n = n,
        text = text
      )
    },
    interrupter = interrupter
  )

  expect_equal(result$text, c("text a", "text b", "text c"))
  expect_true(all(is.na(result$result)))
  expect_equal(call_count, 2)
  expect_equal(interrupt_count, 2)
  expect_length(progress_events, 2)
  expect_equal(progress_events[[1]], list(i = 1, n = 3, text = "text a"))
  expect_equal(progress_events[[2]], list(i = 2, n = 3, text = "text b"))
})
