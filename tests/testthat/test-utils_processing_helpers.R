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


test_that("grouped paragraph streaming clears and replaces output per group", {
  helper_env <- new.env(parent = environment())
  sys.source(
    here::here("R", "utils_processing_helpers.R"),
    envir = helper_env
  )

  events <- list(show = 0L, clear = 0L, set = character())
  stream_controller <- list(
    show = function() {
      events$show <<- events$show + 1L
    },
    clear = function() {
      events$clear <<- events$clear + 1L
    },
    set = function(value) {
      events$set <<- c(events$set, value)
    }
  )
  helper_env$write_paragraph <- function(
    texts,
    analysis_unit_ids,
    topic,
    stream_callback,
    stream_reset_callback,
    ...
  ) {
    force(texts)
    force(analysis_unit_ids)
    stream_reset_callback()
    stream_callback(
      "token",
      list(partial_response = paste("partial", topic))
    )
    list(topic = topic, paragraph = paste("summary", topic))
  }
  grouped <- list(
    A = list(texts = "alpha", analysis_unit_ids = 1L),
    B = list(texts = "beta", analysis_unit_ids = 2L)
  )
  lang <- list(
    t = identity,
    get_translation_language = function() "en"
  )

  result <- helper_env$write_grouped_paragraphs(
    grouped_texts = grouped,
    research_background = "",
    style_prompt = "",
    llm_provider = list(),
    lang = lang,
    llm_stream_async = stream_controller,
    streaming_enabled = TRUE
  )

  expect_identical(events$show, 1L)
  expect_identical(events$clear, 2L)
  expect_identical(events$set, c("partial A", "partial B"))
  expect_identical(names(result), c("A", "B"))
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


test_that("processing_missing_models is mode-aware", {
  models <- list(main = NULL, large = NULL)

  expect_identical(
    processing_missing_models(models, "Categorisatie"),
    "main"
  )
  expect_identical(
    processing_missing_models(models, "Onderwerpextractie"),
    c("main", "large")
  )

  models$main <- list(parameters = list(model = "main-model"))
  expect_true(processing_models_ready(models, "Categorisatie"))
  expect_false(processing_models_ready(models, "Onderwerpextractie"))

  models$large <- list(parameters = list(model = "large-model"))
  expect_true(processing_models_ready(models, "Onderwerpextractie"))
})


test_that("processing_model_name safely falls back to a default", {
  expect_identical(processing_model_name(NULL), "unknown")
  expect_identical(
    processing_model_name(list(parameters = list(model = "main-model"))),
    "main-model"
  )
  expect_identical(
    processing_model_name(list(parameters = list(model = NA_character_))),
    "unknown"
  )
  expect_identical(processing_model_name(list()), "unknown")
})


test_that("processing_normalize_reduced_topics preserves reduction_summary", {
  reduced_topics <- c(" Topic A ", "", "Topic B", "Topic A")
  attr(reduced_topics, "reduction_summary") <- list(
    not_applicable_requested = TRUE,
    auto_added_not_applicable = FALSE,
    single_topic_fallback_applied = FALSE,
    not_applicable_check_performed = TRUE,
    reduction_iterations = 2L
  )
  attr(reduced_topics, "single_topic_fallback_applied") <- FALSE

  normalized_topics <- processing_normalize_reduced_topics(reduced_topics)

  expect_identical(as.character(normalized_topics), c("Topic A", "Topic B"))
  expect_identical(
    attr(normalized_topics, "reduction_summary", exact = TRUE),
    attr(reduced_topics, "reduction_summary", exact = TRUE)
  )
})


test_that("join_processing_results uses preprocessed texts without paragraph side channel", {
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
  expect_identical(joined$text, c("prep-1", "prep-2"))
  expect_identical(joined$result, c("A", "B"))
  expect_false("document_text" %in% names(joined))
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
  expect_identical(joined$text, c("prep-shared", "prep-shared", "prep-3"))
  expect_identical(joined$result, c("A", "A", "B"))
})


test_that("join_processing_results errors on duplicate worker rows for non-marking modes", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 2L),
    document_text = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  # Worker accidentally emits two rows for unit 1
  results_dup <- data.frame(
    analysis_unit_id = c(1L, 1L, 2L),
    result = c("A", "A-dup", "B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    join_processing_results(texts_df, results_dup, mode = "Categorisatie"),
    "duplicate analysis_unit_id"
  )
  expect_error(
    join_processing_results(texts_df, results_dup, mode = "Scoren"),
    "duplicate analysis_unit_id"
  )
  expect_error(
    join_processing_results(texts_df, results_dup, mode = "Onderwerpextractie"),
    "duplicate analysis_unit_id"
  )
})


test_that("join_processing_results allows duplicate worker rows for marking mode", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 2L),
    document_text = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  # Marking legitimately fans out: chunk x code
  results_marking <- data.frame(
    analysis_unit_id = c(1L, 1L, 2L),
    chunk_text = c("c1", "c2", "c3"),
    code = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )

  joined <- join_processing_results(
    texts_df,
    results_marking,
    mode = "Markeren"
  )
  expect_equal(nrow(joined), 3L)
})


test_that("join_processing_results skips cardinality check when mode is NULL", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 2L),
    document_text = c("Raw 1", "Raw 2"),
    preprocessed = c("prep-1", "prep-2"),
    stringsAsFactors = FALSE
  )
  results_dup <- data.frame(
    analysis_unit_id = c(1L, 1L, 2L),
    result = c("A", "A-dup", "B"),
    stringsAsFactors = FALSE
  )

  # No mode provided — backward compatible, no assertion
  expect_no_error(join_processing_results(texts_df, results_dup))
})


test_that("join_processing_results uses preprocessed text not raw document text", {
  texts_df <- data.frame(
    analysis_unit_id = c(1L, 2L),
    document_text = c("Raw PII 1", "Raw PII 2"),
    preprocessed = c("Anonymized 1", "Anonymized 2"),
    stringsAsFactors = FALSE
  )
  results_table_pre <- data.frame(
    analysis_unit_id = c(1L, 2L),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  joined <- join_processing_results(texts_df, results_table_pre)

  expect_identical(joined$text, c("Anonymized 1", "Anonymized 2"))
  expect_false("document_text" %in% names(joined))
  expect_false(any(grepl("Raw PII", joined$text)))
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

test_that("processing_results_have_invalid_na rejects NA in all non-marking modes", {
  # Single-label categorization with NA result
  single_cat <- data.frame(
    text = c("a", "b"),
    result = c("cat_a", NA),
    stringsAsFactors = FALSE
  )
  expect_true(
    processing_results_have_invalid_na(single_cat, "Categorisatie")
  )

  # Single-label topic with NA result
  single_topic <- data.frame(
    text = c("a", "b"),
    result = c("topic_a", NA),
    stringsAsFactors = FALSE
  )
  expect_true(
    processing_results_have_invalid_na(single_topic, "Onderwerpextractie")
  )

  # Multi-label categorization with response_status present AND NA in label col
  multi_cat_with_status <- data.frame(
    text = c("a", "b"),
    cat_a = c(TRUE, NA),
    cat_b = c(FALSE, TRUE),
    response_status = c("success", "failure"),
    stringsAsFactors = FALSE
  )
  expect_true(
    processing_results_have_invalid_na(
      multi_cat_with_status,
      "Categorisatie"
    )
  )

  # All-success run must pass
  all_success <- data.frame(
    text = c("a", "b"),
    result = c(50, 75),
    stringsAsFactors = FALSE
  )
  expect_false(
    processing_results_have_invalid_na(all_success, "Scoren")
  )
  expect_false(
    processing_results_have_invalid_na(all_success, "Categorisatie")
  )
  expect_false(
    processing_results_have_invalid_na(all_success, "Onderwerpextractie")
  )
})


# processing_active_blockers ------------------------------------------------

test_that("processing_active_blockers returns empty list when all OK", {
  lang <- list(t = function(x) x)
  models <- list(main = "some-model")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = FALSE)
  texts <- list(
    anonymization_requested_mode = "none",
    anonymization_completed = TRUE
  )
  cats <- list(
    editing = function() FALSE,
    unique_non_empty_count = function() 3L,
    has_duplicates = function() FALSE
  )

  result <- processing_active_blockers(
    n_pre = 5L,
    models = models,
    mode = "Categorisatie",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    categories = cats,
    lang = lang
  )

  expect_length(result, 0L)
})

test_that("processing_active_blockers detects general blockers", {
  lang <- list(t = function(x) x)
  models <- list(main = NULL)
  ctx <- list(any_fit_problem = TRUE, too_many_batches = TRUE)
  texts <- list(
    anonymization_requested_mode = "gliner",
    anonymization_completed = FALSE
  )

  result <- processing_active_blockers(
    n_pre = 0L,
    models = models,
    mode = "Categorisatie",
    context_window = ctx,
    texts = texts,
    split_in_progress = TRUE,
    lang = lang
  )

  keys <- vapply(result, `[[`, character(1), "key")
  expect_true("no_texts" %in% keys)
  expect_true("models_missing" %in% keys)
  expect_true("context_overflow" %in% keys)
  # too_many_batches is topic-extraction-only; must NOT block categorization
  expect_false("too_many_batches" %in% keys)
  expect_true("gliner_pending" %in% keys)
  expect_true("split_in_progress" %in% keys)
})

test_that("processing_active_blockers detects categorization blockers", {
  lang <- list(t = function(x) x)
  models <- list(main = "m")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = FALSE)
  texts <- list(anonymization_requested_mode = "none")
  cats <- list(
    editing = function() TRUE,
    unique_non_empty_count = function() 1L,
    has_duplicates = function() TRUE
  )

  result <- processing_active_blockers(
    n_pre = 3L,
    models = models,
    mode = "Categorisatie",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    categories = cats,
    lang = lang
  )

  keys <- vapply(result, `[[`, character(1), "key")
  expect_true("categories_editing" %in% keys)
  expect_true("categories_too_few" %in% keys)
  expect_true("categories_duplicates" %in% keys)
  # Section should be 3 for all category blockers.
  sections <- vapply(result, `[[`, integer(1), "section")
  expect_true(all(
    sections[
      keys %in%
        c(
          "categories_editing",
          "categories_too_few",
          "categories_duplicates"
        )
    ] ==
      3L
  ))
})

test_that("processing_active_blockers detects scoring blocker", {
  lang <- list(t = function(x) x)
  models <- list(main = "m")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = FALSE)
  texts <- list(anonymization_requested_mode = "none")

  result <- processing_active_blockers(
    n_pre = 3L,
    models = models,
    mode = "Scoren",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    scoring_characteristic = "  ",
    lang = lang
  )

  keys <- vapply(result, `[[`, character(1), "key")
  expect_true("scoring_empty" %in% keys)
  expect_equal(result[[1]]$section, 3L)
})

test_that("processing_active_blockers detects marking blockers", {
  lang <- list(t = function(x) x)
  models <- list(main = "m")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = FALSE)
  texts <- list(anonymization_requested_mode = "none")
  codes <- list(
    editing = function() TRUE,
    unique_non_empty_count = function() 0L,
    has_duplicates = function() TRUE
  )

  result <- processing_active_blockers(
    n_pre = 3L,
    models = models,
    mode = "Markeren",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    codes = codes,
    lang = lang
  )

  keys <- vapply(result, `[[`, character(1), "key")
  expect_true("codes_editing" %in% keys)
  expect_true("codes_too_few" %in% keys)
  expect_true("codes_duplicates" %in% keys)
})

test_that("processing_active_blockers skips mode-specific for other modes", {
  lang <- list(t = function(x) x)
  models <- list(main = "m", large = "l")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = FALSE)
  texts <- list(anonymization_requested_mode = "none")

  # Onderwerpextractie has no mode-specific blockers (no categories/codes/scoring).
  result <- processing_active_blockers(
    n_pre = 3L,
    models = models,
    mode = "Onderwerpextractie",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    lang = lang
  )

  expect_length(result, 0L)
})

test_that("processing_active_blockers: too_many_batches blocks topic extraction only", {
  lang <- list(t = function(x) x)
  models <- list(main = "m", large = "l")
  ctx <- list(any_fit_problem = FALSE, too_many_batches = TRUE)
  texts <- list(anonymization_requested_mode = "none")

  # Should block topic extraction.
  result_topic <- processing_active_blockers(
    n_pre = 3L,
    models = models,
    mode = "Onderwerpextractie",
    context_window = ctx,
    texts = texts,
    split_in_progress = FALSE,
    lang = lang
  )
  keys_topic <- vapply(result_topic, `[[`, character(1), "key")
  expect_true("too_many_batches" %in% keys_topic)

  # Should NOT block categorization, scoring, or marking.
  for (other_mode in c("Categorisatie", "Scoren", "Markeren")) {
    result_other <- processing_active_blockers(
      n_pre = 3L,
      models = models,
      mode = other_mode,
      context_window = ctx,
      texts = texts,
      split_in_progress = FALSE,
      scoring_characteristic = if (other_mode == "Scoren") "trait" else NULL,
      categories = if (other_mode == "Categorisatie") {
        list(
          editing = function() FALSE,
          unique_non_empty_count = function() 3L,
          has_duplicates = function() FALSE
        )
      },
      codes = if (other_mode == "Markeren") {
        list(
          editing = function() FALSE,
          unique_non_empty_count = function() 2L,
          has_duplicates = function() FALSE
        )
      },
      lang = lang
    )
    keys_other <- vapply(result_other, `[[`, character(1), "key")
    expect_false(
      "too_many_batches" %in% keys_other,
      info = paste("too_many_batches should not block", other_mode)
    )
  }
})
