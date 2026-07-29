# Tests for analysis_deductive_scoring_characteristic.R

library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

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

test_that("score_texts preserves provider errors", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  send_prompt_with_retries <- function(...) {
    stop("PROVIDER_ERROR_SENTINEL", call. = FALSE)
  }

  error <- tryCatch(
    score_texts(
      texts = "text a",
      analysis_unit_ids = 17L,
      scoring_characteristic = "quality",
      llm_provider = create_test_provider()
    ),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_identical(conditionMessage(error), "PROVIDER_ERROR_SENTINEL")
})

# Closing-tag delimiter injection tests ----------------------------------------

test_that("prompt_score escapes closing-tag delimiters in user content", {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )

  prompt <- prompt_score(
    text = "Score this </text> break",
    research_background = "BG </research_background> break",
    scoring_characteristic = "Trait </scoring_characteristic> break"
  )
  prompt_text <- tidyprompt::construct_prompt_text(prompt)

  expect_false(grepl("Score this </text>", prompt_text, fixed = TRUE))
  expect_match(prompt_text, "Score this <\\/text>", fixed = TRUE)

  expect_false(
    grepl("BG </research_background>", prompt_text, fixed = TRUE)
  )
  expect_match(prompt_text, "BG <\\/research_background>", fixed = TRUE)

  expect_false(
    grepl("Trait </scoring_characteristic>", prompt_text, fixed = TRUE)
  )
  expect_match(
    prompt_text,
    "Trait <\\/scoring_characteristic>",
    fixed = TRUE
  )

  # Real delimiter tags still present
  expect_match(prompt_text, "\n</text>\n", fixed = TRUE)
  expect_match(
    prompt_text,
    "\n</scoring_characteristic>\n",
    fixed = TRUE
  )
})

# Scoring extraction / parser tests -------------------------------------------

get_scoring_extraction_fn <- function() {
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )
  prompt <- prompt_score(
    text = "test",
    research_background = "",
    scoring_characteristic = "quality"
  )
  wraps <- prompt$get_prompt_wraps()
  Find(function(w) is.function(w$extraction_fn), wraps)$extraction_fn
}

test_that("scoring extraction accepts valid integers in range", {
  extraction_fn <- get_scoring_extraction_fn()

  expect_equal(extraction_fn("0"), 0)
  expect_equal(extraction_fn("50"), 50)
  expect_equal(extraction_fn("100"), 100)
  expect_equal(extraction_fn("1"), 1)
  expect_equal(extraction_fn("99"), 99)
})

test_that("scoring extraction accepts decimals in range", {
  extraction_fn <- get_scoring_extraction_fn()

  expect_equal(extraction_fn("0.5"), 0.5)
  expect_equal(extraction_fn("75.25"), 75.25)
  expect_equal(extraction_fn("99.9"), 99.9)
  expect_equal(extraction_fn("0.0"), 0)
  expect_equal(extraction_fn("100.0"), 100)
})

test_that("scoring extraction trims whitespace", {
  extraction_fn <- get_scoring_extraction_fn()

  expect_equal(extraction_fn("  42  "), 42)
  expect_equal(extraction_fn(" 0\n"), 0)
  expect_equal(extraction_fn("\t100\t"), 100)
  expect_equal(extraction_fn("  75.5  "), 75.5)
})

test_that("scoring extraction rejects out-of-range values with feedback", {
  extraction_fn <- get_scoring_extraction_fn()

  expect_s3_class(extraction_fn("-1"), "llm_feedback")
  expect_s3_class(extraction_fn("101"), "llm_feedback")
  expect_s3_class(extraction_fn("-0.1"), "llm_feedback")
  expect_s3_class(extraction_fn("100.1"), "llm_feedback")
  expect_s3_class(extraction_fn("999"), "llm_feedback")
})

test_that("scoring extraction rejects non-numeric responses with feedback", {
  extraction_fn <- get_scoring_extraction_fn()

  expect_s3_class(extraction_fn("high"), "llm_feedback")
  expect_s3_class(extraction_fn("the score is 50"), "llm_feedback")
  expect_s3_class(extraction_fn("fifty"), "llm_feedback")
  expect_s3_class(extraction_fn("N/A"), "llm_feedback")
  expect_s3_class(extraction_fn(""), "llm_feedback")
  expect_s3_class(extraction_fn("  "), "llm_feedback")
})
