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


test_that("join_processing_results restores raw texts and paragraph attribute", {
  texts_df <- data.frame(
    raw = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  worker_results <- data.frame(
    text = c("prep-1", "prep-2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  attr(worker_results, "paragraphs") <- list(p1 = "paragraph")

  joined <- join_processing_results(texts_df, worker_results)

  expect_identical(joined$text, c("Raw 1", "Raw 2"))
  expect_identical(joined$result, c("A", "B"))
  expect_false("preprocessed" %in% names(joined))
  expect_identical(attr(joined, "paragraphs"), list(p1 = "paragraph"))
})


test_that("processing_mode_supports_report matches supported modes", {
  expect_true(processing_mode_supports_report("Categorisatie"))
  expect_true(processing_mode_supports_report("Markeren"))
  expect_false(processing_mode_supports_report("Unknown"))
})
