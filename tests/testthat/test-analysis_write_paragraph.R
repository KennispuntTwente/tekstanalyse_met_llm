library(testthat)

test_that("write_paragraph returns a warning record when the prompt overflows", {
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  count_tokens <- function(x) {
    nchar(x)
  }
  get_context_window_size_in_tokens <- function(model) {
    force(model)
    10
  }
  send_prompt_with_retries <- function(...) {
    testthat::fail("send_prompt_with_retries should not be called on overflow")
  }

  result <- write_paragraph(
    texts = c("This text is intentionally long enough to overflow."),
    analysis_unit_ids = 1L,
    topic = "Code A",
    llm_provider = list(parameters = list(model = "unit-test-model")),
    language = "en"
  )

  expect_identical(result$paragraph, "")
  expect_false(result$prompt_fits)
  expect_identical(result$analysis_unit_ids, 1L)
  expect_identical(result$topic, "Code A")
})

test_that("prompt_write_paragraph builds structured tagged prompt", {
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  prompt <- prompt_write_paragraph(
    texts = c("First text", "Second text"),
    topic = "weather",
    research_background = "Survey context",
    style_prompt = "Keep it concise.",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)

  expect_match(prompt_text, "<research_background>", fixed = TRUE)
  expect_match(prompt_text, "<topic>", fixed = TRUE)
  expect_match(prompt_text, "<texts>", fixed = TRUE)
  expect_match(prompt_text, "<text 1>", fixed = TRUE)
  expect_match(prompt_text, "<style_instructions>", fixed = TRUE)
})


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


test_that("write_paragraph checks prompt fit before sending", {
  send_call_count <- 0
  send_prompt_with_retries <- function(...) {
    send_call_count <<- send_call_count + 1
    "unused"
  }
  get_context_window_size_in_tokens <- function(...) 10
  count_tokens <- function(...) 999

  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  result <- write_paragraph(
    texts = c("some text"),
    analysis_unit_ids = 1L,
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_identical(result$paragraph, "")
  expect_false(result$prompt_fits)
  expect_identical(send_call_count, 0)
})
