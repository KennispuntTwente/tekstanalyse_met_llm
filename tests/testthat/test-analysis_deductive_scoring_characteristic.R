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

  expect_identical(result$analysis_unit_id, 1L)
  expect_equal(result$text, "text a")
  expect_equal(result$result, 75)
  expect_equal(call_count, 2)
})

test_that("score_texts can return a decision payload and resume after skip", {
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  call_count <- 0
  send_prompt_with_retries <- function(prompt, llm_provider, ...) {
    force(prompt)
    force(llm_provider)

    call_count <<- call_count + 1
    if (call_count == 1) {
      return(25)
    }
    if (call_count == 2) {
      return(NA_real_)
    }

    90
  }

  decision <- score_texts(
    texts = c("text a", "text b", "text c"),
    analysis_unit_ids = c(1L, 2L, 3L),
    scoring_characteristic = "clarity",
    llm_provider = create_test_provider(),
    failure_action = "return_decision"
  )

  expect_identical(decision$status, "decision_required")
  expect_identical(decision$failed_index, 2L)
  expect_identical(decision$failed_analysis_unit_id, 2L)
  expect_identical(decision$results$result, 25)
  expect_identical(decision$results$response_status, "completed")
  expect_identical(decision$skip_row$response_status, "skipped")
  expect_true(is.na(decision$skip_row$result))

  decision$skip_row$response_status <- "skipped_after_user_confirmation"

  resumed <- score_texts(
    texts = c("text a", "text b", "text c"),
    analysis_unit_ids = c(1L, 2L, 3L),
    scoring_characteristic = "clarity",
    llm_provider = create_test_provider(),
    existing_results = rbind(decision$results, decision$skip_row),
    start_index = decision$failed_index + 1L,
    failure_action = "return_decision"
  )

  expect_identical(resumed$status, "completed")
  expect_identical(resumed$results$result, c(25, NA, 90))
  expect_identical(
    resumed$results$response_status,
    c("completed", "skipped_after_user_confirmation", "completed")
  )
})
