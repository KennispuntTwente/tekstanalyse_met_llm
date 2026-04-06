# Tests for analysis_deductive_scoring_characteristic.R
# Tests the prompt_score() function for scoring texts on characteristics

library(testthat)

create_test_provider <- function(model = "test-model") {
  provider <- list(parameters = list(model = model))
  provider$clone <- function() provider
  provider
}

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
  expect_match(prompt_text, "<text>", fixed = TRUE)
  expect_match(prompt_text, "<scoring_characteristic>", fixed = TRUE)
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
  expect_match(prompt_text, "<research_background>", fixed = TRUE)
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

test_that("score_texts preserves completed scores, reports progress, and stops on early NA", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  analysis_unit_ids <- c(10L, 20L, 30L)
  call_count <- 0
  interrupt_count <- 0
  progress_events <- list()

  send_prompt_with_retries <- function(prompt, llm_provider, ...) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return(42)
    }

    NA_real_
  }

  interrupter <- list(
    execInterrupts = function() {
      interrupt_count <<- interrupt_count + 1
    }
  )

  result <- score_texts(
    texts = c("text a", "text b", "text c"),
    analysis_unit_ids = analysis_unit_ids,
    scoring_characteristic = "clarity",
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

  expect_identical(result$analysis_unit_id, analysis_unit_ids)
  expect_equal(result$text, c("text a", "text b", "text c"))
  # Row 1 succeeded, row 2 failed (NA), row 3 was never processed (NA).
  expect_equal(result$result, c(42, NA, NA))
  expect_equal(call_count, 2)
  expect_equal(interrupt_count, 2)
  expect_length(progress_events, 2)
  expect_equal(progress_events[[1]], list(i = 1, n = 3, text = "text a"))
  expect_equal(progress_events[[2]], list(i = 2, n = 3, text = "text b"))
})
