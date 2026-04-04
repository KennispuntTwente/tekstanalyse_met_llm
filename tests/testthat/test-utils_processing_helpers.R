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


test_that("join_processing_results keeps paragraphs on the joined results table", {
  texts_df <- data.frame(
    raw = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  results_table_pre <- data.frame(
    text = c("prep-1", "prep-2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  attr(results_table_pre, "paragraphs") <- list(list(
    topic = "Topic A",
    paragraph = "paragraph",
    texts = c("Raw 1"),
    prompt_fits = TRUE
  ))

  joined <- join_processing_results(texts_df, results_table_pre)

  expect_true(is.data.frame(joined))
  expect_identical(joined$text, c("Raw 1", "Raw 2"))
  expect_identical(joined$result, c("A", "B"))
  expect_false("preprocessed" %in% names(joined))
  expect_identical(attr(joined, "paragraphs")[[1]]$paragraph, "paragraph")
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
  marking_results <- data.frame(
    text = c("a", "b"),
    sub_text = c("a", NA),
    code = c("x", NA),
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
  expect_false(processing_results_have_invalid_na(marking_results, "Markeren"))
})
