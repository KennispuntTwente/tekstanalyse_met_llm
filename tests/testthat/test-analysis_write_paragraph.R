library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)
source(here::here("R", "utils_create_text_batches.R"), local = TRUE)

# Keep context-window tests independent of the Python tokenizer.
count_tokens <- function(x) nchar(x)

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

test_that("write_paragraph clears stale streaming output on unsent overflow", {
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  get_context_window_size_in_tokens <- function(...) 10
  count_tokens <- function(...) 999
  send_prompt_with_retries <- function(...) {
    testthat::fail("No prompt should be sent when no text fits")
  }
  reset_count <- 0L

  result <- write_paragraph(
    texts = "an oversized text",
    analysis_unit_ids = 1L,
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en",
    stream_callback = function(...) invisible(NULL),
    stream_reset_callback = function() {
      reset_count <<- reset_count + 1L
      invisible(NULL)
    }
  )

  expect_false(result$prompt_fits)
  expect_identical(reset_count, 1L)
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

test_that("prompt_write_paragraph escapes closing-tag delimiters", {
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  prompt <- prompt_write_paragraph(
    texts = c("Text </text 1> break", "Normal"),
    topic = "Topic </topic> break",
    research_background = "BG </research_background> break",
    style_prompt = "Style </style_instructions> break",
    language = "en"
  )
  prompt_text <- tidyprompt::construct_prompt_text(prompt)

  expect_false(grepl("Text </text 1>", prompt_text, fixed = TRUE))
  expect_match(prompt_text, "Text <\\/text 1>", fixed = TRUE)

  expect_false(grepl("Topic </topic>", prompt_text, fixed = TRUE))
  expect_match(prompt_text, "Topic <\\/topic>", fixed = TRUE)

  expect_false(
    grepl("BG </research_background>", prompt_text, fixed = TRUE)
  )
  expect_match(prompt_text, "BG <\\/research_background>", fixed = TRUE)

  expect_false(
    grepl("Style </style_instructions>", prompt_text, fixed = TRUE)
  )
  expect_match(
    prompt_text,
    "Style <\\/style_instructions>",
    fixed = TRUE
  )

  # Real delimiter tags still present
  expect_match(prompt_text, "\n</texts>\n", fixed = TRUE)
  expect_match(prompt_text, "\n</topic>\n", fixed = TRUE)
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


test_that("batch strategy summarizes all texts and recursively reduces batches", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:6, "-", strrep(letters[1:6], 180))
  two_text_prompt <- prompt_write_paragraph(
    texts = texts[1:2],
    topic = "weather",
    language = "en"
  )
  context_window <- count_tokens(
    tidyprompt::construct_prompt_text(two_text_prompt)
  )
  get_context_window_size_in_tokens <- function(...) context_window

  sent_prompts <- character()
  send_prompt_with_retries <- function(prompt, ...) {
    sent_prompts <<- c(
      sent_prompts,
      tidyprompt::construct_prompt_text(prompt)
    )
    paste("Partial summary", length(sent_prompts))
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 8L
  )
  withr::defer(options(old), testthat::teardown_env())
  set.seed(42)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  expect_identical(result$texts, texts)
  expect_identical(result$analysis_unit_ids, seq_along(texts))
  expect_gt(length(sent_prompts), 1L)
  expect_true(any(grepl("<summaries>", sent_prompts, fixed = TRUE)))
})


test_that("batch strategy records batch and reduction scope for every call", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:6, "-", strrep("x", 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[1:2], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window
  scopes <- list()
  send_prompt_with_retries <- function(prompt, execution_scope, ...) {
    force(prompt)
    scopes[[length(scopes) + 1L]] <<- execution_scope
    "short partial summary"
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 8L
  )
  withr::defer(options(old), testthat::teardown_env())
  set.seed(42)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  expect_gt(length(scopes), 1L)
  iterations <- vapply(scopes, `[[`, integer(1), "reduction_iteration")
  expect_identical(sort(unique(iterations)), seq_len(max(iterations)))
  for (iteration in unique(iterations)) {
    iteration_scopes <- scopes[iterations == iteration]
    expect_identical(
      vapply(iteration_scopes, `[[`, integer(1), "batch_index"),
      seq_along(iteration_scopes)
    )
  }
})


test_that("default strategy sends one random context-sized subset", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:6, "-", strrep(letters[1:6], 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[1:2], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window

  calls <- list()
  send_prompt_with_retries <- function(prompt, execution_scope, ...) {
    calls[[length(calls) + 1L]] <<- list(
      prompt = tidyprompt::construct_prompt_text(prompt),
      ids = execution_scope$analysis_unit_ids
    )
    "Sample summary"
  }
  old <- options(paragraph_summary_strategy = NULL)
  withr::defer(options(old), testthat::teardown_env())
  set.seed(7)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  expect_length(calls, 1L)
  expect_gt(length(result$texts), 0L)
  expect_lt(length(result$texts), length(texts))
  expect_identical(result$analysis_unit_ids, calls[[1]]$ids)
  expect_identical(result$texts, texts[result$analysis_unit_ids])
  expect_identical(result$source_coverage, "sampled")
})


test_that("sample strategy repacks when additive token estimates undercount", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- c("first short source", "second short source")
  combined_prompt <- tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts, "weather", language = "en")
  )
  context_window <- nchar(combined_prompt)
  count_tokens <- function(x) {
    item_count <- lengths(regmatches(
      x,
      gregexpr("<text [0-9]+>", x, perl = TRUE)
    ))
    nchar(x) + ifelse(item_count > 1L, 100, 0)
  }
  get_context_window_size_in_tokens <- function(...) context_window
  calls <- list()
  send_prompt_with_retries <- function(prompt, execution_scope, ...) {
    calls[[length(calls) + 1L]] <<- list(
      prompt = tidyprompt::construct_prompt_text(prompt),
      ids = execution_scope$analysis_unit_ids
    )
    "Sample summary"
  }
  old <- options(paragraph_summary_strategy = "sample")
  withr::defer(options(old), testthat::teardown_env())
  set.seed(3)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  expect_length(calls, 1L)
  expect_length(result$texts, 1L)
  expect_identical(result$analysis_unit_ids, calls[[1]]$ids)
  expect_lte(count_tokens(calls[[1]]$prompt), context_window)
})


test_that("sample strategy skips individually oversized texts", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- c(strrep("X", 5000), "small alpha", "small beta")
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[2:3], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window
  sent_ids <- NULL
  send_prompt_with_retries <- function(prompt, execution_scope, ...) {
    force(prompt)
    sent_ids <<- execution_scope$analysis_unit_ids
    "Sample summary"
  }
  old <- options(paragraph_summary_strategy = "sample")
  withr::defer(options(old), testthat::teardown_env())
  set.seed(11)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  expect_false(1L %in% result$analysis_unit_ids)
  expect_identical(result$analysis_unit_ids, sent_ids)
  expect_false(any(result$texts == texts[[1]]))
  expect_identical(result$source_coverage, "sampled")
})


test_that("batch strategy recursively summarizes summaries over multiple levels", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:16, "-", strrep("x", 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[1:2], "weather", language = "en")
  ))
  summary_lengths <- seq(20L, 400L, by = 5L)
  two_fit <- vapply(
    summary_lengths,
    function(n) {
      count_tokens(tidyprompt::construct_prompt_text(prompt_write_paragraph(
        rep(strrep("s", n), 2L),
        "weather",
        language = "en",
        texts_are_summaries = TRUE
      ))) <=
        context_window
    },
    logical(1)
  )
  three_fit <- vapply(
    summary_lengths,
    function(n) {
      count_tokens(tidyprompt::construct_prompt_text(prompt_write_paragraph(
        rep(strrep("s", n), 3L),
        "weather",
        language = "en",
        texts_are_summaries = TRUE
      ))) <=
        context_window
    },
    logical(1)
  )
  candidates <- summary_lengths[two_fit & !three_fit]
  expect_gt(length(candidates), 0L)
  response <- strrep("s", candidates[[1]])

  get_context_window_size_in_tokens <- function(...) context_window
  calls <- list()
  send_prompt_with_retries <- function(prompt, execution_scope, ...) {
    calls[[length(calls) + 1L]] <<- list(
      is_reduction = grepl(
        "<summaries>",
        tidyprompt::construct_prompt_text(prompt),
        fixed = TRUE
      ),
      n_source_ids = length(execution_scope$analysis_unit_ids)
    )
    response
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 8L
  )
  withr::defer(options(old), testthat::teardown_env())
  set.seed(23)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_true(result$prompt_fits)
  reduction_calls <- calls[vapply(calls, `[[`, logical(1), "is_reduction")]
  expect_gt(length(reduction_calls), 1L)
  expect_true(any(
    vapply(
      reduction_calls,
      `[[`,
      integer(1),
      "n_source_ids"
    ) >
      2L
  ))
})


test_that("batch strategy stops safely when a reduction makes no progress", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:3, "-", strrep("x", 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[[1]], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window
  send_count <- 0L
  send_prompt_with_retries <- function(...) {
    send_count <<- send_count + 1L
    strrep("s", 180)
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 8L
  )
  withr::defer(options(old), testthat::teardown_env())

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_false(result$prompt_fits)
  expect_identical(result$paragraph, "")
  expect_identical(send_count, 3L)
})


test_that("batch strategy honors the recursive reduction iteration cap", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:4, "-", strrep("x", 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[1:2], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window
  send_count <- 0L
  send_prompt_with_retries <- function(...) {
    send_count <<- send_count + 1L
    "short partial summary"
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 1L
  )
  withr::defer(options(old), testthat::teardown_env())

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en"
  )

  expect_false(result$prompt_fits)
  expect_identical(send_count, 2L)
})


test_that("batch strategy resets and streams every intermediate synthesis", {
  count_tokens <- function(x) nchar(x)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)

  texts <- paste0("text-", 1:6, "-", strrep("x", 180))
  context_window <- count_tokens(tidyprompt::construct_prompt_text(
    prompt_write_paragraph(texts[1:2], "weather", language = "en")
  ))
  get_context_window_size_in_tokens <- function(...) context_window

  callback_attached <- logical()
  streamed_values <- character()
  stream_events <- character()
  send_prompt_with_retries <- function(prompt, stream_callback = NULL, ...) {
    force(prompt)
    callback_attached <<- c(callback_attached, !is.null(stream_callback))
    if (!is.null(stream_callback)) {
      call_index <- length(callback_attached)
      stream_callback(
        "token",
        list(partial_response = paste("partial response", call_index))
      )
    }
    "short partial summary"
  }
  ui_callback <- function(token, meta) {
    force(token)
    streamed_values <<- c(streamed_values, meta$partial_response)
    stream_events <<- c(stream_events, paste("set", meta$partial_response))
  }
  reset_callback <- function() {
    stream_events <<- c(stream_events, "clear")
  }
  old <- options(
    paragraph_summary_strategy = "batch",
    paragraph_summary_max_reduction_iterations = 8L
  )
  withr::defer(options(old), testthat::teardown_env())
  set.seed(31)

  result <- write_paragraph(
    texts = texts,
    analysis_unit_ids = seq_along(texts),
    topic = "weather",
    llm_provider = list(parameters = list(model = "test")),
    language = "en",
    stream_callback = ui_callback,
    stream_reset_callback = reset_callback
  )

  expect_true(result$prompt_fits)
  expect_gt(length(callback_attached), 1L)
  expect_true(all(callback_attached))
  expect_length(streamed_values, length(callback_attached))
  expect_identical(
    stream_events[seq(1L, length(stream_events), by = 2L)],
    rep("clear", length(callback_attached))
  )
  expect_true(all(grepl(
    "^set partial response",
    stream_events[seq(2L, length(stream_events), by = 2L)]
  )))
})
