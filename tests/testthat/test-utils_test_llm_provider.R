library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

log_debug <- function(...) NULL
log_info <- function(...) NULL
log_warning <- function(...) NULL
log_error <- function(...) NULL


test_that("fake LLM recognizes the current production prompt builders", {
  withr::local_dir(here::here())

  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  prompt_texts <- c(
    tidyprompt::construct_prompt_text(prompt_category(
      text = "lovely product!",
      research_background = "",
      categories = c("Positive", "Negative")
    )),
    tidyprompt::construct_prompt_text(prompt_multi_category(
      text = "bad product!",
      research_background = "",
      categories = c("Positive", "Negative", "Unknown/not applicable"),
      exclusive_categories = "Unknown/not applicable"
    )),
    tidyprompt::construct_prompt_text(prompt_score(
      text = "lovely product!",
      research_background = "",
      scoring_characteristic = "Positive sentiment"
    )),
    tidyprompt::construct_prompt_text(prompt_candidate_topics(
      text_batch = c("Document 0001: Great delivery experience"),
      research_background = "",
      language = "en"
    )),
    tidyprompt::construct_prompt_text(prompt_reduce_topics(
      candidate_topics = c("Delivery speed", "Fast delivery"),
      research_background = "",
      language = "en"
    )),
    tidyprompt::construct_prompt_text(prompt_write_paragraph(
      texts = c("Great product quality and great support."),
      topic = "product quality",
      language = "en"
    )),
    tidyprompt::construct_prompt_text(mark_text_prompt(
      text = "lovely product!",
      code = "Product feedback"
    ))
  )

  expect_true(all(vapply(
    prompt_texts,
    kwallm_test_llm_recognizes_prompt,
    logical(1)
  )))
})


test_that("fake LLM replies remain parseable for supported workflow prompts", {
  withr::local_dir(here::here())

  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  score_reply <- kwallm_test_llm_reply(tidyprompt::construct_prompt_text(
    prompt_score(
      text = "lovely product!",
      research_background = "",
      scoring_characteristic = "Positive sentiment"
    )
  ))
  expect_identical(score_reply, "68")

  candidate_topics_reply <- kwallm_test_llm_reply(tidyprompt::construct_prompt_text(
    prompt_candidate_topics(
      text_batch = c("Document 0001: Fast delivery and helpful support"),
      research_background = "",
      language = "en"
    )
  ))
  candidate_topics <- jsonlite::fromJSON(candidate_topics_reply)
  expect_true("topics" %in% names(candidate_topics))
  expect_true(length(candidate_topics$topics) >= 1)

  reduced_topics_reply <- kwallm_test_llm_reply(tidyprompt::construct_prompt_text(
    prompt_reduce_topics(
      candidate_topics = c("Fast delivery", "Delivery speed"),
      research_background = "",
      language = "en"
    )
  ))
  reduced_topics <- jsonlite::fromJSON(reduced_topics_reply)
  expect_true("topics" %in% names(reduced_topics))
  expect_true(length(reduced_topics$topics) >= 2)

  marking_reply <- kwallm_test_llm_reply(tidyprompt::construct_prompt_text(
    mark_text_prompt(
      text = "lovely product!",
      code = "Product feedback"
    )
  ))
  marking_json <- jsonlite::fromJSON(marking_reply)
  expect_true("text_parts" %in% names(marking_json))
  expect_true(length(marking_json$text_parts) >= 1)
})


test_that("fake LLM provider works through send_prompt_with_retries and streams", {
  withr::local_dir(here::here())

  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")

  score <- send_prompt_with_retries(
    prompt = prompt_score(
      text = "lovely product!",
      research_background = "",
      scoring_characteristic = "Positive sentiment"
    ),
    llm_provider = provider
  )
  expect_identical(score, 68)

  streamed_tokens <- character()
  partials <- character()

  paragraph <- send_prompt_with_retries(
    prompt = prompt_write_paragraph(
      texts = c("Great product quality and helpful support."),
      topic = "product quality",
      language = "en"
    ),
    llm_provider = provider,
    stream_callback = function(token, meta) {
      streamed_tokens <<- c(streamed_tokens, token)
      partials <<- c(partials, meta$partial_response %||% "")
    }
  )

  expect_true(length(streamed_tokens) > 1)
  expect_true(nchar(paragraph) > 0)
  expect_identical(utils::tail(partials, 1), paragraph)
})
