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

test_that("build_analysis_result preserves split lineage and group fan-out", {
  texts_df <- data.frame(
    raw = c("Chunk A", "Chunk B"),
    preprocessed = c("Chunk A", "Chunk B"),
    stringsAsFactors = FALSE
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
      text = "Original text",
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
    source_texts = c("Original text", "Original text"),
    input_info = list(file_type = "csv", text_column = "text")
  )

  report_context <- analysis_result_to_report_context(analysis_result)

  expect_true(is.list(report_context))
  expect_equal(nrow(analysis_result@text_lineage@source_documents), 1)
  expect_equal(nrow(analysis_result@text_lineage@documents), 2)
  expect_equal(analysis_result@text_lineage@document_groups$group_value, "G1")
  expect_equal(nrow(report_context$by_column_values), 2)
  expect_equal(report_context$df$result, c("Theme 1", "Theme 2"))

  # stage_models captures api_url
  expect_true("api_url" %in% names(analysis_result@stage_models))
  expect_equal(
    analysis_result@stage_models$api_url[[1]],
    "https://api.example.com/v1/chat/completions"
  )
})

test_that("marking paragraphs retain supporting excerpts in report context", {
  texts_df <- data.frame(
    raw = "Text about dogs",
    preprocessed = "Text about dogs",
    stringsAsFactors = FALSE
  )

  results_table <- data.frame(
    text = "Text about dogs",
    sub_text = "Text about dogs",
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

  attr(results_table, "paragraphs") <- list(list(
    topic = "Code 1",
    paragraph = "Summary paragraph.",
    texts = c("Text about **dogs**"),
    prompt_fits = TRUE
  ))

  analysis_result <- build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
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
  report_context <- analysis_result_to_report_context(analysis_result)

  expect_length(report_context$paragraphs, 1)
  expect_equal(report_context$paragraphs[[1]]$topic, "Code 1")
  expect_equal(report_context$paragraphs[[1]]$texts, c("Text about **dogs**"))
  expect_equal(metadata$results$markings[[1]]$source_marked_text, "dogs?")
  expect_equal(metadata$results$markings[[1]]$match_start, 12L)
  expect_equal(metadata$results$markings[[1]]$match_method, "fuzzy")
})

test_that("topic metadata includes candidate and reduced topics", {
  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
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
      chunk_size = 5,
      draws = 2,
      n_chunks = 3,
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
  report_context <- analysis_result_to_report_context(analysis_result)

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
  expect_equal(report_context$model_reductie, "large-model")
})

test_that("input provenance and irr sample are serialized", {
  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
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
  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
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

test_that("app_version and api_url are serialized", {
  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
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

  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
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
