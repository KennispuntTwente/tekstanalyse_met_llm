# Tests for analysis_deductive_scoring_characteristic.R

library(testthat)

create_test_provider <- function(model = "test-model") {
  provider <- list(parameters = list(model = model))
  provider$clone <- function() provider
  provider
}

test_that("score_texts preserves successful rows on partial failure", {
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  call_count <- 0
  send_prompt_with_retries <- function(prompt, llm_provider, ...) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return(75)
    }
    NA
  }

  result <- score_texts(
    texts = c("text a", "text b", "text c"),
    analysis_unit_ids = c(1L, 2L, 3L),
    scoring_characteristic = "quality",
    llm_provider = create_test_provider()
  )

  expect_identical(result$analysis_unit_id, c(1L, 2L, 3L))
  expect_equal(result$text, c("text a", "text b", "text c"))
  # Row 1 succeeded, row 2 failed (NA), row 3 was never processed (NA)
  expect_equal(result$result[1], 75)
  expect_true(is.na(result$result[2]))
  expect_true(is.na(result$result[3]))
  expect_equal(call_count, 2)
})
