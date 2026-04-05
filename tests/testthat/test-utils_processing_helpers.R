source(here::here("R", "utils_processing_helpers.R"), local = TRUE)

test_that("collect_grouped_texts groups by result or binary columns", {
  single_results <- data.frame(
    text = c("a", "b", "c"),
    result = c("x", "y", "x"),
    stringsAsFactors = FALSE
  )
  multi_results <- data.frame(
    text = c("a", "b", "c"),
    x = c(TRUE, FALSE, TRUE),
    y = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  expect_identical(
    collect_grouped_texts(single_results, c("x", "y"), FALSE),
    list(x = c("a", "c"), y = "b")
  )
  expect_identical(
    collect_grouped_texts(multi_results, c("x", "y"), TRUE),
    list(x = c("a", "c"), y = "b")
  )
})


test_that("collect_grouped_paragraph_inputs keeps analysis units aligned", {
  single_results <- data.frame(
    analysis_unit_id = c(10L, 20L, 30L),
    text = c("a", "b", "c"),
    result = c("x", "y", "x"),
    stringsAsFactors = FALSE
  )
  multi_results <- data.frame(
    analysis_unit_id = c(10L, 20L, 30L),
    text = c("a", "b", "c"),
    x = c(TRUE, FALSE, TRUE),
    y = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  expect_identical(
    collect_grouped_paragraph_inputs(single_results, c("x", "y"), FALSE),
    list(
      x = list(texts = c("a", "c"), analysis_unit_ids = c(10L, 30L)),
      y = list(texts = "b", analysis_unit_ids = 20L)
    )
  )
  expect_identical(
    collect_grouped_paragraph_inputs(multi_results, c("x", "y"), TRUE),
    list(
      x = list(texts = c("a", "c"), analysis_unit_ids = c(10L, 30L)),
      y = list(texts = "b", analysis_unit_ids = 20L)
    )
  )
})

test_that("processing_texts_under_maximum validates count and notifies", {
  lang <- list(t = function(x) x)
  notification <- NULL

  notify_fn <- function(message, type = NULL) {
    notification <<- list(message = message, type = type)
    invisible(NULL)
  }

  expect_true(processing_texts_under_maximum(
    preprocessed_texts = c("a", "b"),
    lang = lang,
    maximum = 2,
    notify_fn = notify_fn
  ))
  expect_null(notification)

  expect_false(processing_texts_under_maximum(
    preprocessed_texts = c("a", "b", "c"),
    lang = lang,
    maximum = 2,
    notify_fn = notify_fn
  ))
  expect_identical(notification$type, "error")
  expect_match(notification$message, "Je mag maximaal 2 teksten analyseren\\.")
})


test_that("processing_has_pending_gliner_anonymization detects incomplete GLiNER state", {
  expect_false(processing_has_pending_gliner_anonymization(NULL))

  texts <- list(
    anonymization_requested_mode = "gliner",
    anonymization_completed = FALSE
  )
  expect_true(processing_has_pending_gliner_anonymization(texts))

  texts$anonymization_completed <- TRUE
  expect_false(processing_has_pending_gliner_anonymization(texts))

  texts$anonymization_requested_mode <- "regex"
  texts$anonymization_completed <- FALSE
  expect_false(processing_has_pending_gliner_anonymization(texts))
})


test_that("processing_anonymization_ready blocks incomplete GLiNER anonymization", {
  lang <- list(t = function(x) paste0("tr:", x))
  notification <- NULL

  notify_fn <- function(message, type = NULL) {
    notification <<- list(message = message, type = type)
    invisible(NULL)
  }

  expect_false(processing_anonymization_ready(
    texts = list(
      anonymization_requested_mode = "gliner",
      anonymization_completed = FALSE
    ),
    lang = lang,
    notify_fn = notify_fn
  ))
  expect_identical(notification$type, "error")
  expect_identical(
    notification$message,
    "tr:GLiNER-anonimisering nog niet voltooid..."
  )

  notification <- NULL
  expect_true(processing_anonymization_ready(
    texts = list(
      anonymization_requested_mode = "gliner",
      anonymization_completed = TRUE
    ),
    lang = lang,
    notify_fn = notify_fn
  ))
  expect_null(notification)
})


test_that("processing_split_ready blocks launches while splitting is active", {
  lang <- list(t = function(x) paste0("tr:", x))
  notification <- NULL

  notify_fn <- function(message, type = NULL) {
    notification <<- list(message = message, type = type)
    invisible(NULL)
  }

  expect_false(processing_split_ready(
    split_in_progress = TRUE,
    lang = lang,
    notify_fn = notify_fn
  ))
  expect_identical(notification$type, "error")
  expect_identical(notification$message, "tr:Teksten worden nog gesplitst...")

  notification <- NULL
  expect_true(processing_split_ready(
    split_in_progress = FALSE,
    lang = lang,
    notify_fn = notify_fn
  ))
  expect_null(notification)
})


test_that("join_processing_results restores document texts without paragraph side channel", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 2L),
    document_text = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  results_table_pre <- data.frame(
    analysis_unit_id = c(1L, 2L),
    text = c("prep-1", "prep-2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  joined <- join_processing_results(texts_df, results_table_pre)

  expect_true(is.data.frame(joined))
  expect_identical(joined$text, c("Raw 1", "Raw 2"))
  expect_identical(joined$result, c("A", "B"))
  expect_false("preprocessed" %in% names(joined))
  expect_null(attr(joined, "paragraphs", exact = TRUE))
})


test_that("join_processing_results fans out shared analysis units per document row", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 1L, 2L),
    document_text = c("Raw 1", "Raw 2", "Raw 3"),
    preprocessed = c("prep-shared", "prep-shared", "prep-3"),
    stringsAsFactors = FALSE
  )
  results_table_pre <- data.frame(
    analysis_unit_id = c(1L, 2L),
    text = c("prep-shared", "prep-3"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )

  joined <- join_processing_results(texts_df, results_table_pre)

  expect_identical(joined$analysis_unit_id, c(1L, 1L, 2L))
  expect_identical(joined$text, c("Raw 1", "Raw 2", "Raw 3"))
  expect_identical(joined$result, c("A", "A", "B"))
})


test_that("processing_results_have_invalid_na is mode-aware", {
  scoring_results <- data.frame(
    text = c("a", "b"),
    result = c(10, NA),
    stringsAsFactors = FALSE
  )
  multi_label_results <- data.frame(
    text = c("a", "b"),
    topic_a = c(TRUE, NA),
    topic_b = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  invalid_marking_results <- data.frame(
    text = c("a", "b"),
    chunk_text = c("a", NA),
    code = c("x", NA),
    marked_text = c("a", NA),
    stringsAsFactors = FALSE
  )
  valid_marking_results <- data.frame(
    text = c("a", "b"),
    chunk_text = c("a", "b"),
    code = c("x", "x"),
    marked_text = c("a", NA),
    stringsAsFactors = FALSE
  )

  expect_true(processing_results_have_invalid_na(scoring_results, "Scoren"))
  expect_true(
    processing_results_have_invalid_na(
      multi_label_results,
      "Onderwerpextractie"
    )
  )
  expect_true(
    processing_results_have_invalid_na(invalid_marking_results, "Markeren")
  )
  expect_false(
    processing_results_have_invalid_na(valid_marking_results, "Markeren")
  )
})
