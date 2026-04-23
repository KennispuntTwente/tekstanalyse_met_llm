library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

# Shared helper ---------------------------------------------------------------
# Source analysis_code_generation.R into an isolated env with stubs so the
# function finds our fakes for create_text_batches, count_tokens, etc.
make_codegen_env <- function(
  context_window = 20,
  create_text_batches_fn = function(
    texts,
    batch_size,
    draws,
    n_tokens_context_window,
    base_prompt_text,
    text_formatter = NULL,
    ...
  ) {
    NULL
  }
) {
  env <- new.env(parent = globalenv())

  env$semchunk_load_chunker <- function(chunk_size) {
    function(texts, overlap = 0) as.list(texts)
  }
  env$get_context_window_size_in_tokens <- function(model) context_window
  env$count_tokens <- function(x) nchar(x)
  env$escape_prompt_delimiters <- escape_prompt_delimiters
  env$prompt_candidate_topics <- function(
    text_batch,
    research_background,
    language
  ) {
    tidyprompt::tidyprompt("")
  }
  env$create_text_batches <- create_text_batches_fn

  source(here::here("R", "analysis_code_generation.R"), local = env)
  env
}

fake_provider <- list(parameters = list(model = "test-model"))

# 1. create_text_batches returns NULL -> error ---------------------------------
test_that("generate_codes_by_reading_texts stops when batches are NULL", {
  env <- make_codegen_env(
    create_text_batches_fn = function(
      texts,
      batch_size,
      draws,
      n_tokens_context_window,
      base_prompt_text,
      text_formatter = NULL,
      ...
    ) {
      NULL
    }
  )

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("some very long text that exceeds the tiny context window"),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    ),
    "Cannot generate codes"
  )
})

# 2. create_text_batches returns empty list -> error ---------------------------
test_that("generate_codes_by_reading_texts stops when batches are empty list", {
  env <- make_codegen_env(
    create_text_batches_fn = function(
      texts,
      batch_size,
      draws,
      n_tokens_context_window,
      base_prompt_text,
      text_formatter = NULL,
      ...
    ) {
      list()
    }
  )

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    ),
    "Cannot generate codes"
  )
})

# 3. Error message includes context window size --------------------------------
test_that("generate_codes_by_reading_texts error includes token count", {
  env <- make_codegen_env(
    context_window = 20,
    create_text_batches_fn = function(
      texts,
      batch_size,
      draws,
      n_tokens_context_window,
      base_prompt_text,
      text_formatter = NULL,
      ...
    ) {
      NULL
    }
  )

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    ),
    "20 tokens available"
  )
})

# 4. Input validation: empty texts ---------------------------------------------
test_that("generate_codes_by_reading_texts rejects empty texts", {
  env <- make_codegen_env()

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = character(0),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    )
  )
})

# 5. Input validation: non-character texts -------------------------------------
test_that("generate_codes_by_reading_texts rejects non-character texts", {
  env <- make_codegen_env()

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = 42,
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    )
  )
})

# 6. Input validation: empty string in texts -----------------------------------
test_that("generate_codes_by_reading_texts rejects empty strings", {
  env <- make_codegen_env()

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("valid", ""),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    )
  )
})

# 7. Input validation: bad text_size_tokens ------------------------------------
test_that("generate_codes_by_reading_texts rejects invalid text_size_tokens", {
  env <- make_codegen_env()

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      text_size_tokens = -1,
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    )
  )

  expect_error(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      text_size_tokens = "abc",
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    )
  )
})

# 8. NULL context window defaults to 2048 --------------------------------------
test_that("generate_codes_by_reading_texts defaults to 2048 for unknown model", {
  received_window <- NULL
  env <- make_codegen_env(
    context_window = NULL,
    create_text_batches_fn = function(
      texts,
      batch_size,
      draws,
      n_tokens_context_window,
      base_prompt_text,
      text_formatter = NULL,
      ...
    ) {
      received_window <<- n_tokens_context_window
      NULL
    }
  )

  try(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    ),
    silent = TRUE
  )

  expect_equal(received_window, 2048)
})

# Closing-tag delimiter injection tests ----------------------------------------

test_that("text_formatter in generate_codes escapes closing-tag delimiters", {
  # The text_formatter callback inside generate_codes_by_reading_texts
  # should escape closing tags. We mock create_text_batches to capture it.
  captured_formatter <- NULL

  env <- make_codegen_env(
    context_window = 100000,
    create_text_batches_fn = function(
      texts,
      batch_size,
      draws,
      n_tokens_context_window,
      base_prompt_text,
      text_formatter = NULL,
      ...
    ) {
      captured_formatter <<- text_formatter
      NULL
    }
  )

  try(
    env$generate_codes_by_reading_texts(
      texts = c("text"),
      research_background = "",
      llm_provider = fake_provider,
      language = "en"
    ),
    silent = TRUE
  )

  expect_false(is.null(captured_formatter))

  # Test that the formatter escapes closing tags
  result <- captured_formatter("Content </text 1> break", 1)
  expect_false(grepl("Content </text 1>", result, fixed = TRUE))
  expect_match(result, "Content <\\/text 1>", fixed = TRUE)
})
