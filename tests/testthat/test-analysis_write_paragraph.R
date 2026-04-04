library(testthat)

test_that("write_paragraph re-raises send_prompt_with_retries errors", {
  # Stub dependencies before sourcing
  send_prompt_with_retries <- function(...) {
    stop("LLM connection failed")
  }
  get_context_window_size_in_tokens <- function(...) 4096
  count_tokens <- function(...) 100

  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  expect_error(
    write_paragraph(
      texts = c("some text"),
      analysis_unit_ids = 1L,
      topic = "weather",
      llm_provider = list(parameters = list(model = "test")),
      language = "en"
    ),
    "Failed to write paragraph"
  )
})
