# Tests for mark_text_prompt() from analysis_marking.R
# Testing the prompt construction (not LLM calls)
# Note: find_matches is already well-tested in test-find_matches.R

library(testthat)

test_that("mark_text_prompt returns a usable prompt object", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "The weather is sunny and warm today.",
    code = "weather"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_true(is.character(prompt_text))
  expect_true(nchar(prompt_text) > 0)
})

test_that("mark_text_prompt includes code and text in prompt", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "Customer was very satisfied with the product.",
    code = "customer satisfaction"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "customer satisfaction", ignore.case = TRUE)
  expect_match(prompt_text, "Customer was very satisfied")
})

test_that("mark_text_prompt includes research background when provided", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  prompt <- mark_text_prompt(
    text = "Sample text here.",
    code = "test code",
    research_background = "This is interview data from healthcare workers"
  )

  prompt_text <- tidyprompt::construct_prompt_text(prompt)
  expect_match(prompt_text, "healthcare workers")
  expect_match(prompt_text, "research", ignore.case = TRUE)
})

test_that("mark_text_prompt works without research background", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Empty string
  prompt1 <- mark_text_prompt(
    text = "Some text.",
    code = "some code",
    research_background = ""
  )
  expect_true(nchar(tidyprompt::construct_prompt_text(prompt1)) > 0)

  # NULL
  prompt2 <- mark_text_prompt(
    text = "Some text.",
    code = "some code",
    research_background = NULL
  )
  expect_true(nchar(tidyprompt::construct_prompt_text(prompt2)) > 0)
})

test_that("normalize_with_map maps indices correctly", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Test whitespace normalization
  result <- normalize_with_map("hello   world")
  expect_equal(result$norm, "hello world")
  expect_true(length(result$start_idx) == nchar(result$norm))
  expect_true(length(result$end_idx) == nchar(result$norm))

  # The space in normalized maps back to original positions 6-8 (the triple space)
  space_idx <- which(strsplit(result$norm, "")[[1]] == " ")
  expect_true(result$start_idx[space_idx] == 6)
  expect_true(result$end_idx[space_idx] >= 6)
})

test_that("normalize_with_map handles quote normalization", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Curly quotes should be normalized
  result <- normalize_with_map("\u2018hello\u2019") # 'hello'
  expect_equal(result$norm, "'hello'")

  # Double curly quotes
  result2 <- normalize_with_map("\u201Chello\u201D") # "hello"
  expect_equal(result2$norm, "\"hello\"")
})

test_that("normalize_with_map handles empty and NA input", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Empty string
  result_empty <- normalize_with_map("")
  expect_equal(result_empty$norm, "")
  expect_equal(length(result_empty$start_idx), 0)

  # NA
  result_na <- normalize_with_map(NA)
  expect_equal(result_na$norm, "")
  expect_equal(length(result_na$start_idx), 0)
})

test_that("normalize_for_dist is consistent with normalize_with_map", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  test_strings <- c(
    "Hello World",
    "It's  \"OK\"—really",
    "  Spaces   Everywhere  "
  )

  for (s in test_strings) {
    expect_equal(
      normalize_for_dist(s),
      normalize_with_map(s)$norm
    )
  }
})

test_that("mark_texts works with lang = NULL", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  analysis_unit_ids <- 91L

  withr::defer({
    rm(
      list = grep("^semchunker_", ls(envir = .GlobalEnv), value = TRUE),
      envir = .GlobalEnv
    )
  })

  log_info <- function(...) invisible(NULL)
  semchunk_load_chunker <- function(chunk_size) {
    force(chunk_size)
    function(text, overlap = 0) {
      force(overlap)
      text
    }
  }
  get_context_window_size_in_tokens <- function(model) {
    force(model)
    2048
  }
  count_tokens <- function(x) {
    nchar(x)
  }
  send_prompt_with_retries <- function(
    prompt,
    llm_provider,
    max_interactions = 10
  ) {
    force(prompt)
    force(llm_provider)
    force(max_interactions)
    "short text"
  }

  progress_messages <- character()
  progress_stub <- list(
    set_with_total = function(i, total, txt) {
      expect_type(txt, "character")
      expect_length(txt, 1)
      progress_messages <<- c(progress_messages, txt)
      invisible(NULL)
    },
    show = function() invisible(NULL),
    hide = function() invisible(NULL)
  )

  result <- mark_texts(
    texts = "A short text",
    analysis_unit_ids = analysis_unit_ids,
    codes = "Code A",
    llm_provider = list(parameters = list(model = "unit-test-model")),
    progress_primary = progress_stub,
    progress_secondary = progress_stub,
    lang = NULL,
    write_paragraphs = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(
      "analysis_unit_id",
      "chunk_id",
      "chunk_index",
      "chunk_text",
      "code",
      "marked_text"
    ) %in%
      names(result)
  ))
  expect_identical(unique(result$analysis_unit_id), analysis_unit_ids)
  expect_true(length(progress_messages) >= 3)
})

test_that("mark_texts includes research background in live marking prompts", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  withr::defer({
    rm(
      list = grep("^semchunker_", ls(envir = .GlobalEnv), value = TRUE),
      envir = .GlobalEnv
    )
  })

  log_info <- function(...) invisible(NULL)
  semchunk_load_chunker <- function(chunk_size) {
    force(chunk_size)
    function(text, overlap = 0) {
      force(overlap)
      text
    }
  }
  get_context_window_size_in_tokens <- function(model) {
    force(model)
    2048
  }
  count_tokens <- function(x) {
    nchar(x)
  }

  prompt_texts <- character()
  send_prompt_with_retries <- function(
    prompt,
    llm_provider,
    max_interactions = 10
  ) {
    force(llm_provider)
    force(max_interactions)
    prompt_texts <<- c(prompt_texts, tidyprompt::construct_prompt_text(prompt))
    tibble::tibble(
      source_marked_text = "Sample text",
      marked_text = "Sample text",
      match_start = 1L,
      match_end = 11L,
      match_distance = 0L,
      match_method = "exact",
      response_status = "matched_all"
    )
  }

  result <- mark_texts(
    texts = "Sample text",
    analysis_unit_ids = 1L,
    codes = "Code A",
    research_background = "BACKGROUND_SENTINEL",
    llm_provider = list(parameters = list(model = "unit-test-model")),
    lang = NULL,
    write_paragraphs = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_true(any(grepl("BACKGROUND_SENTINEL", prompt_texts, fixed = TRUE)))
})

test_that("mark_texts preserves chunk-code rows when nothing matches", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  withr::defer({
    rm(
      list = grep("^semchunker_", ls(envir = .GlobalEnv), value = TRUE),
      envir = .GlobalEnv
    )
  })

  log_info <- function(...) invisible(NULL)
  semchunk_load_chunker <- function(chunk_size) {
    force(chunk_size)
    function(text, overlap = 0) {
      force(overlap)
      text
    }
  }
  get_context_window_size_in_tokens <- function(model) {
    force(model)
    2048
  }
  count_tokens <- function(x) {
    nchar(x)
  }
  send_prompt_with_retries <- function(
    prompt,
    llm_provider,
    max_interactions = 10
  ) {
    force(prompt)
    force(llm_provider)
    force(max_interactions)
    .kwallm_empty_marking_matches()
  }

  result <- mark_texts(
    texts = "A short text",
    analysis_unit_ids = 91L,
    codes = "Code A",
    llm_provider = list(parameters = list(model = "unit-test-model")),
    lang = NULL,
    write_paragraphs = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_identical(result$analysis_unit_id, 91L)
  expect_identical(result$chunk_text, "A short text")
  expect_identical(result$code, "Code A")
  expect_true(is.na(result$marked_text))
})

test_that(".kwallm_marking_matches_from_find_matches keeps partial status without spans", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  matches <- tibble::tibble(
    needle = character(),
    match = character(),
    distance = integer(),
    start = integer(),
    end = integer()
  )

  result <- .kwallm_marking_matches_from_find_matches(
    matches,
    response_status = "partial_after_max_interactions"
  )

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$marked_text[[1]]))
  expect_equal(
    result$response_status[[1]],
    "partial_after_max_interactions"
  )
})

test_that("mark_text_prompt allows one correction turn before partial fallback", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  find_match_calls <- 0L
  find_matches <- function(
    haystack,
    needles,
    rel = 0.12,
    abs = 2,
    step_div = 5L
  ) {
    force(rel)
    force(abs)
    force(step_div)

    find_match_calls <<- find_match_calls + 1L

    if (find_match_calls == 1L) {
      return(tibble::tibble(
        needle = needles,
        match = NA_character_,
        distance = NA_integer_,
        start = NA_integer_,
        end = NA_integer_
      ))
    }

    tibble::tibble(
      needle = needles,
      match = haystack,
      distance = 0L,
      start = 1L,
      end = nchar(haystack)
    )
  }

  prompt <- mark_text_prompt(
    text = "literal text",
    code = "Code A",
    max_interactions = 2
  )
  extraction_fn <- prompt$get_prompt_wraps()[[3]]$extraction_fn

  first_result <- extraction_fn(list(text_parts = "wrong text"))
  second_result <- extraction_fn(list(text_parts = "literal text"))

  expect_false(is.data.frame(first_result))
  expect_s3_class(second_result, "tbl_df")
  expect_identical(second_result$marked_text[[1]], "literal text")
  expect_identical(second_result$response_status[[1]], "matched_all")
})

test_that("mark_texts and mark_text_prompt respect send_prompt_with_retries__max_interactions option", {
  source(here::here("R", "analysis_marking.R"), local = TRUE)

  # Set the canonical (plural) option to a non-default value
  withr::local_options(send_prompt_with_retries__max_interactions = 3)

  # mark_texts default should pick up the option
  mark_texts_defaults <- formals(mark_texts)
  resolved <- eval(mark_texts_defaults$max_interactions)
  expect_equal(resolved, 3)

  # mark_text_prompt default should pick up the option
  mark_text_prompt_defaults <- formals(mark_text_prompt)
  resolved2 <- eval(mark_text_prompt_defaults$max_interactions)
  expect_equal(resolved2, 3)
})
