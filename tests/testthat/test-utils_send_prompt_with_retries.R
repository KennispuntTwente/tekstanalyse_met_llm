# Tests for utils_send_prompt_with_retries.R
# Using testthat's local_mocked_bindings to mock tidyprompt::send_prompt

library(testthat)

# Helper to create a mock result structure
create_mock_result <- function(response = "test response") {
  list(
    response = response,
    chat_history = data.frame(role = "assistant", content = response)
  )
}

# Helper to create a mock llm_provider object
create_mock_llm_provider <- function(model = "test-model") {
  list(
    parameters = list(model = model),
    clone = function() create_mock_llm_provider(model)
  )
}

test_that("send_prompt_with_retries returns response on successful first try", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  # Mock send_prompt to return success immediately
  local_mocked_bindings(
    send_prompt = function(...) create_mock_result("success"),
    .package = "tidyprompt"
  )

  result <- send_prompt_with_retries(
    prompt = "test prompt",
    llm_provider = create_mock_llm_provider(),
    max_tries = 3,
    retry_delay_seconds = 0
  )

  expect_equal(result, "success")
})

test_that("send_prompt_with_retries retries on error and eventually succeeds", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  # Track call count
  call_count <- 0

  # Mock that fails twice, then succeeds
  local_mocked_bindings(
    send_prompt = function(...) {
      call_count <<- call_count + 1
      if (call_count < 3) {
        stop("Temporary error")
      }
      create_mock_result("success after retries")
    },
    .package = "tidyprompt"
  )

  result <- send_prompt_with_retries(
    prompt = "test prompt",
    llm_provider = create_mock_llm_provider(),
    max_tries = 5,
    retry_delay_seconds = 0 # No delay for tests
  )

  expect_equal(result, "success after retries")
  expect_equal(call_count, 3)
})

test_that("send_prompt_with_retries stops after max_tries with error", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  # Mock that always fails
  local_mocked_bindings(
    send_prompt = function(...) {
      stop("Persistent error")
    },
    .package = "tidyprompt"
  )

  expect_error(
    send_prompt_with_retries(
      prompt = "test prompt",
      llm_provider = create_mock_llm_provider(),
      max_tries = 3,
      retry_delay_seconds = 0
    ),
    "3 attempts"
  )
})

test_that("send_prompt_with_retries errors when response is NULL", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  # Mock returning result with NULL response
  local_mocked_bindings(
    send_prompt = function(...) {
      list(response = NULL, chat_history = NULL)
    },
    .package = "tidyprompt"
  )

  expect_error(
    send_prompt_with_retries(
      prompt = "test prompt",
      llm_provider = create_mock_llm_provider(),
      max_tries = 1,
      retry_delay_seconds = 0
    ),
    "failed to get a valid reply"
  )
})

test_that("send_prompt_with_retries respects max_interactions parameter", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  captured_args <- NULL

  local_mocked_bindings(
    send_prompt = function(
      prompt,
      provider,
      return_mode,
      max_interactions,
      ...
    ) {
      captured_args <<- list(max_interactions = max_interactions)
      create_mock_result("captured")
    },
    .package = "tidyprompt"
  )

  send_prompt_with_retries(
    prompt = "test",
    llm_provider = create_mock_llm_provider(),
    max_tries = 1,
    max_interactions = 42,
    retry_delay_seconds = 0
  )

  expect_equal(captured_args$max_interactions, 42)
})

test_that("send_prompt_with_retries uses default options", {
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)

  # Verify function signature accepts options with defaults
  fn_formals <- formals(send_prompt_with_retries)

  expect_true("max_tries" %in% names(fn_formals))
  expect_true("retry_delay_seconds" %in% names(fn_formals))
  expect_true("max_interactions" %in% names(fn_formals))
  expect_true("stream_callback" %in% names(fn_formals))
  expect_true("llm_provider" %in% names(fn_formals))
})
