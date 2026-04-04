library(testthat)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

source(here::here("R", "result_model.R"), local = TRUE)
source(here::here("R", "result_builders.R"), local = TRUE)
source(here::here("R", "result_serializers.R"), local = TRUE)
source(here::here("R", "utils_processing_helpers.R"), local = TRUE)

.test_models <- function() {
  list(
    main = list(
      parameters = list(model = "main-model"),
      url = "https://api.example.com/v1/chat/completions"
    ),
    large = list(
      parameters = list(model = "large-model"),
      url = "https://api.example.com/v1/chat/completions"
    )
  )
}

.make_result_texts_df <- function(
  document_text,
  preprocessed = document_text,
  source_document_id = seq_along(document_text),
  source_document_text = document_text,
  analysis_unit_id = match(preprocessed, unique(preprocessed))
) {
  data.frame(
    source_document_id = as.integer(source_document_id),
    document_id = seq_along(document_text),
    source_document_text = as.character(source_document_text),
    document_text = as.character(document_text),
    preprocessed = as.character(preprocessed),
    analysis_unit_id = as.integer(analysis_unit_id),
    stringsAsFactors = FALSE
  )
}

test_that("build_analysis_result preserves split lineage and group fan-out", {
  texts_df <- .make_result_texts_df(
    document_text = c("Chunk A", "Chunk B"),
    source_document_id = c(1L, 1L),
    source_document_text = c("Original text", "Original text")
  )

  results_table <- data.frame(
    text = c("Chunk A", "Chunk B"),
    result = c("Theme 1", "Theme 2"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-1",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = "group",
    by_column_lookup = data.frame(
      source_document_id = 1L,
      by_value = "G1",
      stringsAsFactors = FALSE
    ),
    models = .test_models(),
    categories = c("Theme 1", "Theme 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = TRUE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt"),
    input_info = list(file_type = "csv", text_column = "text")
  )

  report_df <- .kwallm_report_results_df(analysis_result)
  group_lookup <- .kwallm_report_group_lookup(analysis_result)

  expect_s3_class(report_df, "data.frame")
  expect_true("document_id" %in% names(report_df))
  expect_equal(nrow(analysis_result@text_lineage@source_documents), 1)
  expect_equal(nrow(analysis_result@text_lineage@documents), 2)
  expect_equal(analysis_result@text_lineage@document_groups$group_value, "G1")
  expect_equal(nrow(group_lookup), 2)
  expect_equal(group_lookup$document_id, c(1L, 2L))
  expect_equal(report_df$result, c("Theme 1", "Theme 2"))

  # stage_models captures api_url
  expect_true("api_url" %in% names(analysis_result@stage_models))
  expect_equal(
    analysis_result@stage_models$api_url[[1]],
    "https://api.example.com/v1/chat/completions"
  )
})

test_that("build_analysis_result preserves groups from chunk-keyed lookup", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
    source_document_id = c(1L, 1L, 2L),
    source_document_text = c("Text 1", "Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
    result = c("Theme 1", "Theme 2", "Theme 1"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-1b",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = "group",
    by_column_lookup = data.frame(
      document_id = c(1L, 2L, 3L),
      by_value = c("G1", "G1", "G2"),
      stringsAsFactors = FALSE
    ),
    models = .test_models(),
    categories = c("Theme 1", "Theme 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = TRUE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  group_lookup <- .kwallm_report_group_lookup(analysis_result)

  expect_equal(
    analysis_result@text_lineage@document_groups$group_value,
    c("G1", "G2")
  )
  expect_equal(nrow(group_lookup), 3)
  expect_true("document_id" %in% names(group_lookup))
  expect_equal(group_lookup$document_id, c(1L, 2L, 3L))
  expect_equal(sort(group_lookup$by_value), c("G1", "G1", "G2"))
})

test_that("metadata and export sheets include text counts", {
  texts_df <- .make_result_texts_df(
    document_text = c("Doc 1 chunk A", "Doc 1 chunk B", "Doc 2"),
    preprocessed = c("shared prep", "shared prep", "Doc 2"),
    source_document_id = c(1L, 1L, 2L),
    source_document_text = c("Doc 1", "Doc 1", "Doc 2"),
    analysis_unit_id = c(1L, 1L, 2L)
  )

  results_table <- data.frame(
    text = c("Doc 1 chunk A", "Doc 1 chunk B", "Doc 2"),
    result = c("Theme 1", "Theme 1", "Theme 2"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-counts",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("Theme 1", "Theme 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)
  metadata_values <- stats::setNames(
    sheets$metadata$value,
    sheets$metadata$field
  )

  expect_equal(
    metadata$text_counts,
    list(
      source_documents = 2L,
      documents = 3L,
      analysis_units = 2L,
      reused_analyses = 1L
    )
  )
  expect_equal(metadata_values[["source_documents"]], "2")
  expect_equal(metadata_values[["documents"]], "3")
  expect_equal(metadata_values[["analysis_units"]], "2")
  expect_equal(metadata_values[["reused_analyses"]], "1")
})

test_that("marking paragraphs retain supporting excerpts in report helpers", {
  texts_df <- .make_result_texts_df(
    document_text = "Text about dogs"
  )

  results_table <- data.frame(
    analysis_unit_id = 1L,
    chunk_id = 1L,
    chunk_index = 1L,
    text = "Text about dogs",
    chunk_text = "Text about dogs",
    code = "Code 1",
    marked_text = "dogs",
    source_marked_text = "dogs?",
    match_start = 12L,
    match_end = 15L,
    match_distance = 1L,
    match_method = "fuzzy",
    response_status = "matched_all",
    stringsAsFactors = FALSE
  )

  paragraph_entries <- list(list(
    topic = "Code 1",
    paragraph = "Summary paragraph.",
    texts = c("Text about **dogs**"),
    analysis_unit_ids = 1L,
    prompt_fits = TRUE
  ))

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-2",
    mode = "Markeren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    codes = "Code 1",
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(marking = "prompt")
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  paragraph_row <- analysis_result@paragraphs@paragraphs[1, , drop = FALSE]
  subject_lookup <- .kwallm_paragraph_subject_lookup(analysis_result)
  supporting_texts <- .kwallm_paragraph_supporting_texts(
    analysis_result,
    paragraph_row$paragraph_id[[1]]
  )

  expect_equal(nrow(analysis_result@paragraphs@paragraphs), 1)
  expect_equal(
    unname(subject_lookup[[as.character(paragraph_row$subject_id[[1]])]]),
    "Code 1"
  )
  expect_equal(supporting_texts, c("Text about **dogs**"))
  expect_equal(metadata$results$markings[[1]]$source_marked_text, "dogs?")
  expect_equal(metadata$results$markings[[1]]$match_start, 12L)
  expect_equal(metadata$results$markings[[1]]$match_method, "fuzzy")
})

test_that("marking results deduplicate shared analysis-unit rows before report fan-out", {
  texts_df <- .make_result_texts_df(
    document_text = c("Shared text", "Shared text"),
    preprocessed = c("shared prep", "shared prep"),
    analysis_unit_id = c(1L, 1L)
  )

  results_table <- data.frame(
    analysis_unit_id = c(1L, 1L),
    chunk_id = c(1L, 1L),
    chunk_index = c(1L, 1L),
    text = c("Shared text", "Shared text"),
    chunk_text = c("Shared text", "Shared text"),
    code = c("Code 1", "Code 1"),
    source_marked_text = c("Shared", "Shared"),
    marked_text = c("Shared", "Shared"),
    match_start = c(1L, 1L),
    match_end = c(6L, 6L),
    match_distance = c(0L, 0L),
    match_method = c("exact", "exact"),
    response_status = c("matched_all", "matched_all"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-marking-dedup",
    mode = "Markeren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    codes = "Code 1",
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(marking = "prompt")
  )

  report_df <- .kwallm_report_results_df(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)

  expect_equal(nrow(analysis_result@results@chunks), 1)
  expect_equal(nrow(analysis_result@results@markings), 1)
  expect_equal(nrow(report_df), 2)
  expect_true("document_id" %in% names(report_df))
  expect_identical(report_df$document_id, c(1L, 2L))
  expect_true(all(report_df$marked_text == "Shared"))
  expect_true("document_id" %in% names(sheets$results))
  expect_identical(sheets$results$document_id, c(1L, 2L))
})

test_that("marking results default source_marked_text per row when omitted", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text about cats", "Text about dogs")
  )

  results_table <- data.frame(
    analysis_unit_id = c(1L, 2L),
    chunk_id = c(1L, 2L),
    chunk_index = c(1L, 1L),
    text = c("Text about cats", "Text about dogs"),
    chunk_text = c("Text about cats", "Text about dogs"),
    code = c("Code 1", "Code 1"),
    marked_text = c("cats", "dogs"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-marking-default-source-text",
    mode = "Markeren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    codes = "Code 1",
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(marking = "prompt")
  )

  markings <- analysis_result@results@markings

  expect_equal(nrow(markings), 2)
  expect_identical(markings$source_marked_text, markings$marked_text)
  expect_true(all(markings$response_status == "matched_all"))
})

test_that("single-label assignments use explicit analysis unit ids when shuffled", {
  texts_df <- .make_result_texts_df(
    document_text = c("Doc 1", "Doc 2"),
    preprocessed = c("prep 1", "prep 2"),
    analysis_unit_id = c(10L, 20L)
  )

  results_table <- data.frame(
    analysis_unit_id = c(20L, 10L),
    text = c("Doc 2", "Doc 1"),
    result = c("Theme 2", "Theme 1"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-shuffled-single-label",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("Theme 1", "Theme 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  labels_lookup <- .kwallm_labels_lookup(analysis_result@results@labels)
  assignments <- analysis_result@results@assignments
  assigned_labels <- labels_lookup[as.character(assignments$label_id)]

  expect_identical(
    unname(assigned_labels[match(c(10L, 20L), assignments$analysis_unit_id)]),
    c("Theme 1", "Theme 2")
  )
})

test_that("multi-label assignments use explicit analysis unit ids when shuffled", {
  texts_df <- .make_result_texts_df(
    document_text = c("Doc 1", "Doc 2"),
    preprocessed = c("prep 1", "prep 2"),
    analysis_unit_id = c(10L, 20L)
  )

  results_table <- data.frame(
    analysis_unit_id = c(20L, 10L),
    text = c("Doc 2", "Doc 1"),
    Positive = c(FALSE, TRUE),
    Negative = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-shuffled-multi-label",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("Positive", "Negative"),
    exclusive_categories = character(),
    assign_multiple_categories = TRUE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  labels_lookup <- .kwallm_labels_lookup(analysis_result@results@labels)
  assignments <- analysis_result@results@assignments
  assigned_labels <- split(
    labels_lookup[as.character(assignments$label_id)],
    assignments$analysis_unit_id
  )

  expect_identical(unname(assigned_labels[["10"]]), "Positive")
  expect_identical(unname(assigned_labels[["20"]]), "Negative")
})

test_that("scoring results use explicit analysis unit ids when shuffled", {
  texts_df <- .make_result_texts_df(
    document_text = c("Doc 1", "Doc 2"),
    preprocessed = c("prep 1", "prep 2"),
    analysis_unit_id = c(10L, 20L)
  )

  results_table <- data.frame(
    analysis_unit_id = c(20L, 10L),
    text = c("Doc 2", "Doc 1"),
    result = c(90, 10),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-shuffled-scores",
    mode = "Scoren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(scoring = "prompt")
  )

  scores <- analysis_result@results@scores
  expect_identical(
    scores$score[match(c(10L, 20L), scores$analysis_unit_id)],
    c(10, 90)
  )
})

test_that("paragraph provenance uses analysis unit ids when raw text differs", {
  texts_df <- .make_result_texts_df(
    document_text = "John Smith called",
    preprocessed = "[PERSON] called"
  )

  results_table <- data.frame(
    text = "John Smith called",
    result = "Theme 1",
    stringsAsFactors = FALSE
  )

  paragraph_entries <- list(list(
    topic = "Theme 1",
    paragraph = "Summary paragraph.",
    texts = "[PERSON] called",
    analysis_unit_ids = 1L,
    prompt_fits = TRUE
  ))

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-2b",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = "Theme 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  expect_equal(nrow(analysis_result@paragraphs@paragraph_sources), 1)
  expect_equal(
    .kwallm_paragraph_supporting_texts(
      analysis_result,
      analysis_result@paragraphs@paragraphs$paragraph_id[[1]]
    ),
    "[PERSON] called"
  )
})


test_that("build_analysis_result rejects paragraph entries without analysis unit ids", {
  texts_df <- .make_result_texts_df(document_text = "Text 1")

  results_table <- data.frame(
    text = "Text 1",
    result = "Theme 1",
    stringsAsFactors = FALSE
  )

  paragraph_entries <- list(list(
    topic = "Theme 1",
    paragraph = "Summary paragraph.",
    texts = "Text 1",
    prompt_fits = TRUE
  ))

  expect_error(
    build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      paragraph_entries = paragraph_entries,
      uuid = "run-missing-paragraph-ids",
      mode = "Categorisatie",
      research_background = "background",
      style_prompt = NULL,
      irr_result = NULL,
      language = "en",
      by_column_name = NULL,
      by_column_lookup = NULL,
      models = .test_models(),
      categories = "Theme 1",
      exclusive_categories = character(),
      assign_multiple_categories = FALSE,
      human_in_the_loop = FALSE,
      write_paragraphs = TRUE,
      stage_prompt_previews = list(categorization = "prompt")
    ),
    "paragraph entries must contain analysis_unit_ids"
  )
})


test_that("paragraph provenance uses explicit analysis unit ids when available", {
  texts_df <- .make_result_texts_df(
    document_text = "John Smith called",
    preprocessed = "[PERSON] called"
  )

  results_table <- data.frame(
    text = "John Smith called",
    result = "Theme 1",
    stringsAsFactors = FALSE
  )

  paragraph_entries <- list(list(
    topic = "Theme 1",
    paragraph = "Summary paragraph.",
    texts = "excerpt carried through paragraph generation",
    analysis_unit_ids = 1L,
    prompt_fits = TRUE
  ))

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-2c",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = "Theme 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  expect_equal(nrow(analysis_result@paragraphs@paragraph_sources), 1)
  expect_identical(
    analysis_result@paragraphs@paragraph_sources$document_id,
    1L
  )
  expect_equal(
    .kwallm_paragraph_supporting_texts(
      analysis_result,
      analysis_result@paragraphs@paragraphs$paragraph_id[[1]]
    ),
    "excerpt carried through paragraph generation"
  )
})

test_that("analysis_result_expected_paragraph_subject_count is result-aware", {
  marking_result <- build_analysis_result(
    texts_df = .make_result_texts_df(document_text = "Text about cats"),
    results_table = data.frame(
      analysis_unit_id = 1L,
      chunk_id = NA_integer_,
      chunk_index = NA_integer_,
      text = "Text about cats",
      chunk_text = NA_character_,
      code = NA_character_,
      marked_text = NA_character_,
      stringsAsFactors = FALSE
    ),
    uuid = "run-empty-marking",
    mode = "Markeren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    codes = "Code 1",
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE
  )

  categorization_result <- build_analysis_result(
    texts_df = .make_result_texts_df(
      document_text = c("Text 1", "Text 2"),
      preprocessed = c("Text 1", "Text 2")
    ),
    results_table = data.frame(
      text = c("Text 1", "Text 2"),
      result = c("Theme 1", "Theme 2"),
      stringsAsFactors = FALSE
    ),
    uuid = "run-categorization-count",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("Theme 1", "Theme 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE
  )

  expect_identical(
    analysis_result_expected_paragraph_subject_count(marking_result),
    0L
  )
  expect_identical(
    analysis_result_expected_paragraph_subject_count(categorization_result),
    2L
  )
})

test_that("topic metadata includes candidate and reduced topics", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("Topic A", "Topic B"),
    stringsAsFactors = FALSE
  )
  reduced_topics <- c("Topic A", "Topic B")
  attr(reduced_topics, "reduction_summary") <- list(
    not_applicable_requested = TRUE,
    auto_added_not_applicable = TRUE,
    not_applicable_check_performed = TRUE,
    reduction_iterations = 2L
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-3",
    mode = "Onderwerpextractie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    topics = c("Topic A", "Topic B"),
    exclusive_topics = "Topic B",
    assign_multiple_categories = FALSE,
    human_in_the_loop = TRUE,
    write_paragraphs = FALSE,
    context_window = list(
      batch_size = 5,
      draws = 2,
      n_batches = 3,
      n_tokens_context_window = 1000
    ),
    stage_prompt_previews = list(
      topic_candidate_generation = "candidate prompt",
      topic_reduction = "reduction prompt",
      topic_not_applicable_check = "not applicable prompt",
      topic_assignment = "assignment prompt"
    ),
    candidate_topics = c("Topic A", "Topic B", "Topic C"),
    reduced_topics = reduced_topics,
    topics_were_edited = TRUE
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  reduction_model <- .kwallm_get_stage_model_id(
    analysis_result,
    c("topic_reduction", "topic_not_applicable_check")
  )

  expect_equal(
    metadata$results$topic_provenance$candidate_topics,
    c("Topic A", "Topic B", "Topic C")
  )
  expect_equal(
    metadata$results$topic_provenance$reduced_topics,
    c("Topic A", "Topic B")
  )
  expect_true(isTRUE(metadata$results$topic_provenance$human_edited))
  expect_true(isTRUE(
    metadata$results$topic_provenance$not_applicable_requested
  ))
  expect_true(isTRUE(
    metadata$results$topic_provenance$auto_added_not_applicable
  ))
  expect_true(isTRUE(
    metadata$results$topic_provenance$not_applicable_check_performed
  ))
  expect_equal(metadata$results$topic_provenance$reduction_iterations, 2L)
  expect_true(any(vapply(
    metadata$stage_models,
    function(x) identical(x$stage_id, "topic_not_applicable_check"),
    logical(1)
  )))
  expect_equal(reduction_model, "large-model")
})

test_that("input provenance and irr sample are serialized", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c(10, 20),
    stringsAsFactors = FALSE
  )

  irr_summary <- list(
    subjects = 1,
    estimate = 0,
    statistic = 0,
    p.value = 1,
    parameter = 0,
    conf.low = 0,
    conf.high = 0,
    llm_mean = 10,
    llm_sd = 0,
    user_mean = 10,
    user_sd = 0,
    sensitivity_sentence = ""
  )

  irr_sample <- data.frame(
    text = "Text 1",
    result = 10,
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-4",
    mode = "Scoren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = irr_summary,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    write_paragraphs = FALSE,
    stage_prompt_previews = list(scoring = "prompt"),
    input_info = list(
      file_type = "xlsx",
      selected_sheet = "Sheet1",
      text_column = "answer",
      grouping_column = "group",
      txt_split_lines = TRUE,
      anonymization_requested_mode = "gliner",
      anonymization_applied_mode = "none",
      anonymization_completed = FALSE,
      split_enabled = TRUE,
      split_chunk_size = 128,
      split_overlap = 16
    ),
    irr_sample = irr_sample
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)

  expect_equal(metadata$input$file_type, "xlsx")
  expect_equal(metadata$input$selected_sheet, "Sheet1")
  expect_equal(metadata$input$text_column, "answer")
  expect_equal(metadata$input$grouping_column, "group")
  expect_true(isTRUE(metadata$input$txt_split_lines))
  expect_equal(metadata$input$anonymization_requested_mode, "gliner")
  expect_equal(metadata$input$anonymization_applied_mode, "none")
  expect_false(isTRUE(metadata$input$anonymization_completed))
  expect_true(isTRUE(metadata$input$split_enabled))
  expect_equal(metadata$input$split_chunk_size, 128)
  expect_equal(metadata$input$split_overlap, 16)
  expect_equal(metadata$reliability$sample$text, "Text 1")
  expect_true("reliability" %in% names(sheets))
})

test_that("stage execution provenance is serialized", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c(10, 20),
    stringsAsFactors = FALSE
  )

  stage_execution_rows <- data.frame(
    prompt_id = "prompt-1",
    stage_id = "scoring",
    model_id = "main-model",
    started_at = "2026-04-04T10:00:00.000Z",
    completed_at = "2026-04-04T10:00:01.250Z",
    duration_ms = 1250,
    attempt_count = 2L,
    retry_count = 1L,
    max_tries = 5L,
    retry_delay_seconds = 3,
    max_interactions = 10L,
    completion_status = "success",
    error_messages = "temporary timeout",
    final_error_message = NA_character_,
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-4b",
    mode = "Scoren",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    write_paragraphs = FALSE,
    stage_prompt_previews = list(scoring = "prompt"),
    stage_execution_rows = stage_execution_rows
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)

  expect_equal(metadata$stage_executions[[1]]$stage_id, "scoring")
  expect_equal(metadata$stage_executions[[1]]$retry_count, 1L)
  expect_equal(
    metadata$stage_executions[[1]]$error_messages,
    "temporary timeout"
  )
  expect_true("stage_executions" %in% names(sheets))
  expect_equal(sheets$stage_executions$completion_status[[1]], "success")
})

test_that("AnalysisResult rejects result rows that do not reference text lineage", {
  text_lineage <- TextLineage(
    source_documents = data.frame(
      source_document_id = 1L,
      source_document_text = "Text 1",
      stringsAsFactors = FALSE
    ),
    documents = data.frame(
      document_id = 1L,
      source_document_id = 1L,
      document_text = "Text 1",
      stringsAsFactors = FALSE
    ),
    analysis_units = data.frame(
      analysis_unit_id = 1L,
      preprocessed_text = "Text 1",
      stringsAsFactors = FALSE
    ),
    document_units = data.frame(
      document_id = 1L,
      analysis_unit_id = 1L,
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    AnalysisResult(
      metadata = AnalysisMetadata(
        run_id = "run-invalid",
        mode_id = "scoring",
        language = "en",
        timestamp = Sys.time(),
        research_background = ""
      ),
      text_lineage = text_lineage,
      results = ScoringResult(
        scores = data.frame(
          analysis_unit_id = 2L,
          score = 10,
          stringsAsFactors = FALSE
        ),
        characteristic = "helpfulness"
      ),
      mode_config = ScoringConfig(
        scoring_characteristic = "helpfulness"
      )
    ),
    "results scores\\$analysis_unit_id must reference text_lineage@analysis_units"
  )
})

test_that("app_version and api_url are serialized", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c(10, 20),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-version",
    mode = "Scoren",
    research_background = "bg",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    write_paragraphs = FALSE,
    app_version = "1.3.2"
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)

  # app_version in metadata JSON and Excel
  expect_equal(metadata$app_version, "1.3.2")
  expect_true("app_version" %in% sheets$metadata$field)
  expect_equal(
    sheets$metadata$value[sheets$metadata$field == "app_version"],
    "1.3.2"
  )

  # api_url in stage_models
  expect_equal(
    metadata$stage_models[[1]]$api_url,
    "https://api.example.com/v1/chat/completions"
  )
  expect_true("api_url" %in% names(sheets$stage_models))

  # NULL app_version is handled
  analysis_result_no_version <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-no-version",
    mode = "Scoren",
    research_background = "bg",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    write_paragraphs = FALSE,
    app_version = NULL
  )
  md_no_version <- analysis_result_to_metadata_list(analysis_result_no_version)
  expect_null(md_no_version$app_version)
})

test_that("download bundle contains metadata json, excel, and report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("zip")
  skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-5",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("A", "B"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  bundle <- create_analysis_result_download_bundle(
    analysis_result,
    temp_dir = withr::local_tempdir()
  )
  contents <- zip::zip_list(bundle)$filename

  expect_true(file.exists(bundle))
  expect_setequal(contents, c("metadata.json", "results.xlsx", "report.html"))
})

test_that("download bundle surfaces metadata errors with the correct label", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("zip")
  skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  temp_dir <- withr::local_tempdir()

  analysis_result <- build_analysis_result(
    texts_df = .make_result_texts_df(
      document_text = "Text 1",
      preprocessed = "Text 1"
    ),
    results_table = data.frame(
      text = "Text 1",
      result = 10,
      stringsAsFactors = FALSE
    ),
    uuid = "run-metadata-error",
    mode = "Scoren",
    research_background = "background",
    style_prompt = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "helpfulness",
    write_paragraphs = FALSE,
    irr_result = list(bad = new.env(parent = emptyenv()))
  )

  expect_error(
    create_analysis_result_download_bundle(
      analysis_result,
      temp_dir = temp_dir
    ),
    "Metadata file generation error"
  )
})
