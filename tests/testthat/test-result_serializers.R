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
      analysis_units = 2L
    )
  )
  expect_equal(metadata_values[["source_documents"]], "2")
  expect_equal(metadata_values[["documents"]], "3")
  expect_equal(metadata_values[["analysis_units"]], "2")
  expect_false("reused_analyses" %in% sheets$metadata$field)
  expect_identical(
    names(metadata$results),
    c("labels", "multi_label", "assignments", "response_status")
  )
})

test_that("paragraph summary strategy is typed and exported as run metadata", {
  old <- options(
    paragraph_summary_strategy = "sample",
    paragraph_summary_max_reduction_iterations = 5L
  )
  withr::defer(options(old), testthat::teardown_env())

  texts_df <- .make_result_texts_df(c("Text 1", "Text 2"))
  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "A"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-summary-strategy",
    mode = "Categorisatie",
    research_background = "background",
    style_prompt = "concise",
    language = "en",
    models = .test_models(),
    categories = "A",
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE
  )

  expect_identical(
    analysis_result@mode_config@paragraph_summary_strategy,
    "sample"
  )
  expect_identical(
    analysis_result@mode_config@paragraph_summary_max_reduction_iterations,
    5L
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  expect_identical(metadata$mode_config$paragraph_summary_strategy, "sample")
  expect_identical(
    metadata$mode_config$paragraph_summary_max_reduction_iterations,
    5L
  )

  sheets <- analysis_result_to_export_sheets(analysis_result)
  metadata_values <- stats::setNames(
    sheets$metadata$value,
    sheets$metadata$field
  )
  expect_identical(metadata_values[["paragraph_summary_strategy"]], "sample")
  expect_identical(
    metadata_values[["paragraph_summary_max_reduction_iterations"]],
    "5"
  )

  json_path <- write_analysis_result_metadata_json(
    analysis_result,
    withr::local_tempdir()
  )
  exported_json <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  expect_identical(
    exported_json$mode_config$paragraph_summary_strategy,
    "sample"
  )
  expect_identical(
    exported_json$mode_config$paragraph_summary_max_reduction_iterations,
    5L
  )
})

test_that("paragraph summary metadata validates strategy and reduction limit", {
  expect_identical(
    CategorizationConfig()@paragraph_summary_strategy,
    "sample"
  )
  expect_error(
    CategorizationConfig(paragraph_summary_strategy = "unknown"),
    "batch.*sample"
  )
  expect_error(
    CategorizationConfig(
      paragraph_summary_max_reduction_iterations = 0L
    ),
    ">= 1"
  )
})

test_that("sampled paragraph coverage is preserved in exported results", {
  texts_df <- .make_result_texts_df(c("Text 1", "Text 2"))
  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "A"),
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(list(
    topic = "A",
    paragraph = "Summary based on a sample.",
    texts = "Text 2",
    analysis_unit_ids = 2L,
    prompt_fits = TRUE,
    source_coverage = "sampled"
  ))

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-sampled-summary",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = "",
    language = "en",
    models = .test_models(),
    categories = "A",
    write_paragraphs = TRUE
  )

  expect_identical(
    analysis_result@paragraphs@paragraphs$source_coverage,
    "sampled"
  )
  metadata <- analysis_result_to_metadata_list(analysis_result)
  expect_identical(
    metadata$paragraphs$paragraphs[[1]]$source_coverage,
    "sampled"
  )
  expect_identical(
    analysis_result_to_export_sheets(
      analysis_result
    )$paragraphs$source_coverage,
    "sampled"
  )

  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("zip")
  skip_if_not(isTRUE(rmarkdown::pandoc_available()))
  temp_dir <- withr::local_tempdir()
  bundle <- create_analysis_result_download_bundle(
    analysis_result,
    temp_dir = temp_dir
  )
  report_dir <- file.path(temp_dir, "sampled-report")
  dir.create(report_dir)
  zip::unzip(bundle, files = "report.html", exdir = report_dir)
  report_html <- paste(
    readLines(file.path(report_dir, "report.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(report_html, "random context-sized sample", fixed = TRUE)
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
  expect_equal(metadata$results$responses[[1]]$response_status, "matched_all")
  expect_equal(metadata$results$markings[[1]]$source_marked_text, "dogs?")
  expect_equal(metadata$results$markings[[1]]$match_start, 12L)
  expect_equal(metadata$results$markings[[1]]$match_method, "fuzzy")
})

test_that("report paragraphs are ordered by category frequency", {
  texts_df <- .make_result_texts_df(c(
    "Rare text",
    "Common text",
    "Common text"
  ))
  results_table <- data.frame(
    text = texts_df$preprocessed,
    result = c("Rare", "Common", "Common"),
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(
    list(
      topic = "Rare",
      paragraph = "Rare summary.",
      texts = "Rare text",
      analysis_unit_ids = 1L,
      prompt_fits = TRUE
    ),
    list(
      topic = "Common",
      paragraph = "Common summary.",
      texts = "Common text",
      analysis_unit_ids = 2L,
      prompt_fits = TRUE
    )
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-paragraph-frequency-single",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = c("Rare", "Common"),
    assign_multiple_categories = FALSE,
    write_paragraphs = TRUE
  )

  ordered <- .kwallm_report_paragraphs_by_frequency(analysis_result)
  labels <- .kwallm_paragraph_subject_lookup(analysis_result)

  expect_identical(
    unname(labels[as.character(ordered$subject_id)]),
    c("Common", "Rare")
  )
  expect_identical(ordered$paragraph_id, c(2L, 1L))
})

test_that("multi-label report paragraphs are ordered by category frequency", {
  texts_df <- .make_result_texts_df(c("Text 1", "Text 2", "Text 3"))
  results_table <- data.frame(
    text = texts_df$preprocessed,
    Rare = c(TRUE, FALSE, FALSE),
    Common = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(
    list(
      topic = "Rare",
      paragraph = "Rare summary.",
      texts = "Text 1",
      analysis_unit_ids = 1L,
      prompt_fits = TRUE
    ),
    list(
      topic = "Common",
      paragraph = "Common summary.",
      texts = texts_df$preprocessed,
      analysis_unit_ids = 1:3,
      prompt_fits = TRUE
    )
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = "run-paragraph-frequency-multi",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = c("Rare", "Common"),
    assign_multiple_categories = TRUE,
    write_paragraphs = TRUE
  )

  ordered <- .kwallm_report_paragraphs_by_frequency(analysis_result)
  labels <- .kwallm_paragraph_subject_lookup(analysis_result)

  expect_identical(
    unname(labels[as.character(ordered$subject_id)]),
    c("Common", "Rare")
  )
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
  expect_equal(nrow(analysis_result@results@responses), 1)
  expect_equal(nrow(analysis_result@results@markings), 1)
  expect_equal(nrow(report_df), 2)
  expect_true("document_id" %in% names(report_df))
  expect_identical(report_df$document_id, c(1L, 2L))
  expect_true(all(report_df$marked_text == "Shared"))
  expect_true(all(report_df$response_status == "matched_all"))
  expect_true("document_id" %in% names(sheets$results))
  expect_identical(sheets$results$document_id, c(1L, 2L))
  expect_true("marking_responses" %in% names(sheets))
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
  responses <- analysis_result@results@responses

  expect_equal(nrow(markings), 2)
  expect_equal(nrow(responses), 2)
  expect_identical(markings$source_marked_text, markings$marked_text)
  expect_true(all(markings$response_status == "matched_all"))
  expect_true(all(responses$response_status == "matched_all"))
})

test_that("marking results preserve unmarked chunk-code rows in report tables", {
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
    marked_text = c("cats", NA_character_),
    source_marked_text = c("cats", NA_character_),
    match_start = c(12L, NA_integer_),
    match_end = c(15L, NA_integer_),
    match_distance = c(0L, NA_integer_),
    match_method = c("exact", NA_character_),
    response_status = c("matched_all", "partial_after_max_interactions"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-marking-unmatched-rows",
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
  metadata <- analysis_result_to_metadata_list(analysis_result)

  expect_equal(nrow(analysis_result@results@chunks), 2)
  expect_equal(nrow(analysis_result@results@responses), 2)
  expect_equal(nrow(analysis_result@results@markings), 1)
  expect_equal(nrow(report_df), 2)
  expect_identical(report_df$marked_text[[1]], "cats")
  expect_true(is.na(report_df$marked_text[[2]]))
  expect_identical(
    report_df$response_status,
    c(
      "matched_all",
      "partial_after_max_interactions"
    )
  )
  expect_equal(
    metadata$results$responses[[2]]$response_status,
    "partial_after_max_interactions"
  )
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

test_that("categorization response_status flows through builder and serializers", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2", "Text 3"),
    preprocessed = c("Text 1", "Text 2", "Text 3")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2", "Text 3"),
    result = c("Theme 1", NA_character_, "Theme 2"),
    response_status = c("success", "failure", "success"),
    stringsAsFactors = FALSE
  )

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-cat-response-status",
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

  rs <- analysis_result@results@response_status
  expect_s3_class(rs, "data.frame")
  expect_equal(nrow(rs), 3)
  expect_identical(names(rs), c("analysis_unit_id", "response_status"))
  expect_identical(rs$response_status, c("success", "failure", "success"))

  metadata <- analysis_result_to_metadata_list(analysis_result)
  expect_true("response_status" %in% names(metadata$results))
  expect_equal(length(metadata$results$response_status), 3)
  expect_equal(
    metadata$results$response_status[[2]]$response_status,
    "failure"
  )

  sheets <- analysis_result_to_export_sheets(analysis_result)
  expect_true("categorization_response_status" %in% names(sheets))
  expect_equal(nrow(sheets$categorization_response_status), 3)
  expect_equal(
    sheets$categorization_response_status$response_status,
    c("success", "failure", "success")
  )
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
    result = c("Topic A", "Unknown/not applicable"),
    stringsAsFactors = FALSE
  )
  reduced_topics <- c("Topic A", "Unknown/not applicable")
  attr(reduced_topics, "reduction_summary") <- list(
    not_applicable_requested = TRUE,
    auto_added_not_applicable = TRUE,
    single_topic_fallback_applied = TRUE,
    not_applicable_check_performed = FALSE,
    reduction_iterations = 0L
  )
  attr(reduced_topics, "single_topic_fallback_applied") <- TRUE

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
    topics = c("Topic A", "Unknown/not applicable"),
    exclusive_topics = "Unknown/not applicable",
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
    candidate_topics = c("Topic A"),
    reduced_topics = reduced_topics,
    topics_were_edited = TRUE
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)
  sheets <- analysis_result_to_export_sheets(analysis_result)
  reduction_model <- .kwallm_get_stage_model_id(
    analysis_result,
    c("topic_reduction", "topic_not_applicable_check")
  )

  expect_equal(
    metadata$results$topic_provenance$candidate_topics,
    c("Topic A")
  )
  expect_equal(
    metadata$results$topic_provenance$reduced_topics,
    c("Topic A", "Unknown/not applicable")
  )
  expect_true(isTRUE(metadata$results$topic_provenance$human_edited))
  expect_true(isTRUE(
    metadata$results$topic_provenance$not_applicable_requested
  ))
  expect_true(isTRUE(
    metadata$results$topic_provenance$auto_added_not_applicable
  ))
  expect_true(isTRUE(
    metadata$results$topic_provenance$single_topic_fallback_applied
  ))
  expect_true(isTRUE(
    !metadata$results$topic_provenance$not_applicable_check_performed
  ))
  expect_equal(metadata$results$topic_provenance$reduction_iterations, 0L)
  expect_identical(
    names(metadata$results),
    c(
      "topic_provenance",
      "labels",
      "multi_label",
      "assignments",
      "response_status"
    )
  )
  expect_equal(
    sheets$topic_generation_settings$value[
      sheets$topic_generation_settings$setting ==
        "single_topic_fallback_applied"
    ],
    "TRUE"
  )
  expect_equal(
    metadata$stage_models$topic_not_applicable_check,
    NULL
  )
  expect_equal(reduction_model, "large-model")
})

test_that("topic metadata captures one-topic fallback applied after editing", {
  texts_df <- .make_result_texts_df(
    document_text = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2")
  )

  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("Topic A", "Unknown/not applicable"),
    stringsAsFactors = FALSE
  )
  reduced_topics <- c("Topic A", "Topic B")
  attr(reduced_topics, "reduction_summary") <- list(
    not_applicable_requested = TRUE,
    auto_added_not_applicable = FALSE,
    single_topic_fallback_applied = FALSE,
    not_applicable_check_performed = TRUE,
    reduction_iterations = 1L
  )
  final_topics <- c("Topic A", "Unknown/not applicable")
  attr(final_topics, "single_topic_fallback_applied") <- TRUE

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-3b",
    mode = "Onderwerpextractie",
    research_background = "background",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    topics = final_topics,
    exclusive_topics = "Unknown/not applicable",
    assign_multiple_categories = FALSE,
    human_in_the_loop = TRUE,
    write_paragraphs = FALSE,
    candidate_topics = c("Topic A", "Topic B"),
    reduced_topics = reduced_topics,
    topics_were_edited = TRUE
  )

  metadata <- analysis_result_to_metadata_list(analysis_result)

  expect_true(isTRUE(
    metadata$results$topic_provenance$single_topic_fallback_applied
  ))
  expect_false(isTRUE(
    metadata$results$topic_provenance$auto_added_not_applicable
  ))
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
  expect_true("reliability_sample" %in% names(sheets))
  expect_equal(sheets$reliability_sample$text, "Text 1")
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
    try_count = 2L,
    max_tries = 5L,
    retry_delay_seconds = 3,
    max_interactions = 10L,
    completion_status = "success",
    error_messages = "temporary timeout",
    final_error_message = NA_character_,
    prompt_scope = I(list(list(
      kind = "analysis_unit",
      analysis_unit_ids = 2L
    ))),
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

  expect_true("scoring" %in% names(metadata$stage_executions))
  expect_equal(metadata$stage_executions$scoring[[1]]$prompt_id, "prompt-1")
  expect_equal(metadata$stage_executions$scoring[[1]]$try_count, 2L)
  expect_equal(
    metadata$stage_executions$scoring[[1]]$error_messages,
    "temporary timeout"
  )
  expect_equal(
    metadata$stage_executions$scoring[[1]]$prompt_scope$kind,
    "analysis_unit"
  )
  expect_equal(
    metadata$stage_executions$scoring[[1]]$prompt_scope$analysis_unit_ids,
    2L
  )
  expect_true("stage_executions" %in% names(sheets))
  expect_equal(sheets$stage_executions$completion_status[[1]], "success")
  scope_from_sheet <- jsonlite::fromJSON(
    sheets$stage_executions$prompt_scope[[1]]
  )
  expect_equal(scope_from_sheet$kind, "analysis_unit")
  expect_equal(as.integer(scope_from_sheet$analysis_unit_ids), 2L)
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
    stage_prompt_previews = list(scoring = "prompt"),
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
    metadata$stage_models$scoring$api_url,
    "https://api.example.com/v1/chat/completions"
  )
  expect_equal(metadata$stage_prompts$scoring$prompt_preview, "prompt")
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

# 5 Multi-label fallback (categories = NULL) -----------------------------------

test_that("build_analysis_result infers multi-label categories from column names", {
  texts_df <- .make_result_texts_df(
    document_text = c("I liked it", "I disliked it")
  )

  results_table <- data.frame(
    text = c("I liked it", "I disliked it"),
    Positive = c(TRUE, FALSE),
    Negative = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "multi-label-fallback",
    mode = "Categorisatie",
    research_background = "bg",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = NULL,
    assign_multiple_categories = TRUE
  )

  labels <- result@results@labels
  assignments <- result@results@assignments

  expect_equal(sort(labels$label_text), c("Negative", "Positive"))
  expect_equal(nrow(assignments), 2)

  pos_id <- labels$label_id[labels$label_text == "Positive"]
  neg_id <- labels$label_id[labels$label_text == "Negative"]
  expect_true(
    any(assignments$analysis_unit_id == 1L & assignments$label_id == pos_id)
  )
  expect_true(
    any(assignments$analysis_unit_id == 2L & assignments$label_id == neg_id)
  )
})


# -- analysis_name property tests ---------------------------------------------

test_that("AnalysisMetadata stores analysis_name and defaults to empty", {
  meta <- AnalysisMetadata(
    run_id = "test-run",
    mode_id = "categorization",
    language = "en"
  )
  expect_equal(meta@analysis_name, "")
})

test_that("AnalysisMetadata accepts a non-empty analysis_name", {
  meta <- AnalysisMetadata(
    run_id = "test-run",
    mode_id = "scoring",
    language = "nl",
    analysis_name = "My Test Analysis"
  )
  expect_equal(meta@analysis_name, "My Test Analysis")
})

test_that("build_analysis_result stores analysis_name in metadata", {
  texts_df <- .make_result_texts_df(document_text = c("Doc A", "Doc B"))
  results_table <- data.frame(
    text = c("Doc A", "Doc B"),
    result = c("Cat 1", "Cat 2"),
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-name-test",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = c("Cat 1", "Cat 2"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt"),
    analysis_name = "Customer Satisfaction 2026"
  )

  expect_equal(ar@metadata@analysis_name, "Customer Satisfaction 2026")
})

test_that("build_analysis_result defaults analysis_name to empty string", {
  texts_df <- .make_result_texts_df(document_text = "Doc A")
  results_table <- data.frame(
    text = "Doc A",
    result = "Cat 1",
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-no-name",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = "Cat 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  expect_equal(ar@metadata@analysis_name, "")
})

test_that("analysis_name appears in metadata JSON output", {
  texts_df <- .make_result_texts_df(document_text = "Doc A")
  results_table <- data.frame(
    text = "Doc A",
    result = "Cat 1",
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-json",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = "Cat 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt"),
    analysis_name = "JSON Test Name"
  )

  metadata_list <- analysis_result_to_metadata_list(ar)
  expect_equal(metadata_list$analysis_name, "JSON Test Name")
})

test_that("analysis_name appears in Excel metadata sheet", {
  texts_df <- .make_result_texts_df(document_text = "Doc A")
  results_table <- data.frame(
    text = "Doc A",
    result = "Cat 1",
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-excel",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = "Cat 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt"),
    analysis_name = "Excel Test Name"
  )

  sheets <- analysis_result_to_export_sheets(ar)
  metadata_values <- stats::setNames(
    sheets$metadata$value,
    sheets$metadata$field
  )

  expect_true("analysis_name" %in% sheets$metadata$field)
  expect_equal(metadata_values[["analysis_name"]], "Excel Test Name")
})

test_that("empty analysis_name appears as empty string in metadata JSON", {
  texts_df <- .make_result_texts_df(document_text = "Doc A")
  results_table <- data.frame(
    text = "Doc A",
    result = "Cat 1",
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-empty-name",
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    language = "en",
    models = .test_models(),
    categories = "Cat 1",
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  metadata_list <- analysis_result_to_metadata_list(ar)
  expect_equal(metadata_list$analysis_name, "")

  sheets <- analysis_result_to_export_sheets(ar)
  metadata_values <- stats::setNames(
    sheets$metadata$value,
    sheets$metadata$field
  )
  expect_equal(metadata_values[["analysis_name"]], "")
})


# 9. Preprocessed text in results (privacy) -----------------------------------

test_that("report results use preprocessed text not raw document text for categorization", {
  texts_df <- .make_result_texts_df(
    document_text = c("I live at 12345 Amsterdam", "Raw PII text 2"),
    preprocessed = c("I live at <<removed>>", "Anonymized text 2")
  )

  results_table <- data.frame(
    text = c("I live at <<removed>>", "Anonymized text 2"),
    result = c("Theme 1", "Theme 2"),
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-privacy-cat",
    mode = "Categorisatie",
    research_background = "bg",
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

  report_df <- .kwallm_report_results_df(ar)
  expect_identical(
    report_df$text,
    c("I live at <<removed>>", "Anonymized text 2")
  )
  expect_false(any(grepl("12345", report_df$text)))

  sheets <- analysis_result_to_export_sheets(ar)
  expect_identical(sheets$results$text, report_df$text)
})

test_that("report results use preprocessed text not raw document text for multi-label categorization", {
  texts_df <- .make_result_texts_df(
    document_text = c("Raw PII text"),
    preprocessed = c("Anonymized text")
  )

  results_table <- data.frame(
    text = "Anonymized text",
    Cat1 = TRUE,
    Cat2 = FALSE,
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-privacy-cat-multi",
    mode = "Categorisatie",
    research_background = "bg",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    categories = c("Cat1", "Cat2"),
    exclusive_categories = character(),
    assign_multiple_categories = TRUE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )

  report_df <- .kwallm_report_results_df(ar)
  expect_identical(report_df$text, "Anonymized text")
  expect_false(any(grepl("Raw PII", report_df$text)))
})

test_that("report results use preprocessed text not raw document text for scoring", {
  texts_df <- .make_result_texts_df(
    document_text = c("Raw PII text"),
    preprocessed = c("Anonymized text")
  )

  results_table <- data.frame(
    text = "Anonymized text",
    result = 75,
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-privacy-scoring",
    mode = "Scoren",
    research_background = "bg",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    scoring_characteristic = "empathy",
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(scoring = "prompt")
  )

  report_df <- .kwallm_report_results_df(ar)
  expect_identical(report_df$text, "Anonymized text")
  expect_false(any(grepl("Raw PII", report_df$text)))

  sheets <- analysis_result_to_export_sheets(ar)
  expect_identical(sheets$results$text, report_df$text)
})

test_that("report results use preprocessed text not raw document text for marking", {
  texts_df <- .make_result_texts_df(
    document_text = c("I live at 12345 Amsterdam"),
    preprocessed = c("I live at <<removed>>")
  )

  results_table <- data.frame(
    analysis_unit_id = 1L,
    chunk_id = 1L,
    chunk_index = 1L,
    text = "I live at <<removed>>",
    chunk_text = "I live at <<removed>>",
    code = "Address",
    marked_text = "<<removed>>",
    source_marked_text = "<<removed>>",
    match_start = 11L,
    match_end = 21L,
    match_distance = 0L,
    match_method = "exact",
    response_status = "matched_all",
    stringsAsFactors = FALSE
  )

  ar <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = "run-privacy-marking",
    mode = "Markeren",
    research_background = "bg",
    style_prompt = NULL,
    irr_result = NULL,
    language = "en",
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .test_models(),
    codes = "Address",
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(marking = "prompt")
  )

  report_df <- .kwallm_report_results_df(ar)
  expect_identical(report_df$text, "I live at <<removed>>")
  expect_false(any(grepl("12345", report_df$text)))

  sheets <- analysis_result_to_export_sheets(ar)
  expect_identical(sheets$results$text, report_df$text)
})
