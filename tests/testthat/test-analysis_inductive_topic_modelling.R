# Tests for analysis_inductive_topic_modelling.R

library(testthat)

create_test_provider <- function(model = "test-model") {
  provider <- list(parameters = list(model = model))
  provider$clone <- function() provider
  provider
}

test_that("prompt_candidate_topics returns a usable prompt object", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  text_chunk <- c("The weather is nice", "I like coding")

  prompt <- prompt_candidate_topics(
    text_chunk = text_chunk,
    research_background = "",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
  expect_match(prompt_text, "topics")
})

test_that("prompt_candidate_topics includes research background", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt <- prompt_candidate_topics(
    text_chunk = c("test"),
    research_background = "Customer satisfaction survey",
    language = "en"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "Customer satisfaction survey")
})

test_that("prompt_candidate_topics respects language parameter", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt_nl <- prompt_candidate_topics(
    text_chunk = c("test"),
    research_background = "",
    language = "nl"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt_nl)
  expect_match(prompt_text, "Dutch")
})

test_that("create_candidate_topics supports progress and interruption", {
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  prompt_candidate_topics <- function(
    text_chunk,
    research_background = "",
    language = c("nl", "en")
  ) {
    force(text_chunk)
    force(research_background)
    match.arg(language)
    list(text_chunk = text_chunk)
  }

  interrupt_count <- 0
  progress_events <- list()

  send_prompt_with_retries <- function(prompt, llm_provider) {
    force(llm_provider)
    list(topics = paste0("topic:", prompt$text_chunk))
  }

  interrupter <- list(
    execInterrupts = function() {
      interrupt_count <<- interrupt_count + 1
    }
  )

  result <- create_candidate_topics(
    text_chunks = list(c("alpha"), c("beta", "gamma")),
    research_background = "background",
    llm_provider = create_test_provider(),
    on_progress = function(i, n, chunk, chunk_result) {
      progress_events[[length(progress_events) + 1]] <<- list(
        i = i,
        n = n,
        chunk = chunk,
        result = chunk_result
      )
    },
    interrupter = interrupter
  )

  expect_equal(result, c("topic:alpha", "topic:beta", "topic:gamma"))
  expect_equal(interrupt_count, 2)
  expect_length(progress_events, 2)
  expect_equal(
    progress_events[[1]],
    list(
      i = 1,
      n = 2,
      chunk = c("alpha"),
      result = c("topic:alpha")
    )
  )
  expect_equal(
    progress_events[[2]],
    list(
      i = 2,
      n = 2,
      chunk = c("beta", "gamma"),
      result = c("topic:beta", "topic:gamma")
    )
  )
})

test_that("assign_topics supports progress, interruption, and early NA", {
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  call_count <- 0
  interrupt_count <- 0
  progress_events <- list()

  send_prompt_with_retries <- function(prompt, llm_provider) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return("Topic A")
    }

    NA_character_
  }

  interrupter <- list(
    execInterrupts = function() {
      interrupt_count <<- interrupt_count + 1
    }
  )

  result <- assign_topics(
    texts = c("text a", "text b", "text c"),
    topics = c("Topic A", "Topic B"),
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
