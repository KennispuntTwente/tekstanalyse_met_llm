# Helpers for building the typed result object from runtime analysis data.

# 1 Public builder -------------------------------------------------------------

# Contains the single entry point the processing flow uses.
# This function turns one finished run into a stable AnalysisResult object.

#' Build an AnalysisResult object
#'
#' Convert the current runtime data for one analysis run into the S7 result
#' object that drives exports and reports.
#'
#' @param texts_df Data frame from the processing flow, usually `texts$df`.
#'   Must contain `raw`; if present, `preprocessed`, `document_id`,
#'   `source_document_id`, `source_text`, and `analysis_unit_id` are reused.
#'   Missing lineage columns are filled in by this builder.
#' @param results_table Joined processing output returned after mapping results
#'   back to the original texts, usually `results_table()` from
#'   `module_core_processing.R`. It should already be in
#'   final UI/export shape: categorization/topic single-label uses `result`,
#'   multi-label uses one logical column per label, scoring uses numeric
#'   `result`, and marking uses `text`, `sub_text`, `code`, and `marked_text`,
#'   with optional matching diagnostics in `source_marked_text`, `match_start`,
#'   `match_end`, `match_distance`, `match_method`, and `response_status`.
#'   Generated paragraphs, if any, live in `attr(results_table, "paragraphs")`.
#' @param uuid Character run identifier.
#' @param mode Display mode name or canonical mode id.
#' @param research_background Character prompt context entered by the user.
#' @param style_prompt Optional paragraph style prompt.
#' @param irr_result Optional interrater reliability summary list.
#' @param language App language code.
#' @param by_column_name Optional grouping column name.
#' @param by_column_lookup Optional lookup table, usually `by_column_lookup()`.
#'   Must contain `text` and `by_value`, where `text` matches the original
#'   source text and `by_value` is the grouped-report value for that source row.
#' @param models Named list with provider/model info per stage. Expected entries
#'   are `main` and, for topic reduction, optional `large`.
#' @param categories Optional categorization labels.
#' @param exclusive_categories Optional exclusive categorization labels.
#' @param scoring_characteristic Optional score label.
#' @param topics Optional topic labels.
#' @param exclusive_topics Optional exclusive topic labels.
#' @param codes Optional marking codes.
#' @param assign_multiple_categories Logical; whether one text can map to more than one label.
#' @param human_in_the_loop Logical; whether a human review step was used.
#' @param write_paragraphs Logical; whether paragraph output was generated.
#' @param context_window List with chunking or topic-generation settings.
#' @param stage_prompt_previews Optional named list of prompt previews keyed by
#'   stage id, usually built in `module_core_processing.R`. Common keys are
#'   `categorization`, `scoring`, `topic_candidate_generation`,
#'   `topic_reduction`, `topic_not_applicable_check`, `topic_assignment`,
#'   `marking`, and `paragraph_generation`.
#' @param stage_execution_rows Optional data frame with one row per
#'   `send_prompt_with_retries()` call made during the run. Expected columns are
#'   `prompt_id`, `stage_id`, `model_id`, `started_at`, `completed_at`,
#'   `duration_ms`, `attempt_count`, `retry_count`, `max_tries`,
#'   `retry_delay_seconds`, `max_interactions`, `completion_status`,
#'   `error_messages`, and `final_error_message`.
#' @param input_info List with upload, anonymization, and split provenance.
#'   This usually comes from `upload_info()`, `texts$anonymization_*`, and
#'   `split_settings()`, and may include `file_type`, `selected_sheet`,
#'   `text_column`, `grouping_column`, `filter_spec`, `txt_split_lines`,
#'   `anonymization_requested_mode`, `anonymization_applied_mode`,
#'   `anonymization_completed`, `split_enabled`, `split_chunk_size`, and
#'   `split_overlap`.
#' @param source_texts Optional character vector, usually `split_source_texts()`.
#'   Must have length `nrow(texts_df)` and map each current row back to the
#'   original source text before splitting.
#' @param candidate_topics Character vector of raw generated topics.
#' @param reduced_topics Character vector of reduced topics before final edits.
#' @param topics_were_edited Logical; whether a human changed the final topics.
#' @param irr_sample Optional data frame, usually `interrater_reliability$sample()`.
#'   This is stored as-is for provenance and should contain the sampled rows the
#'   IRR workflow showed to the reviewer.
#'
#' @return An `AnalysisResult` object containing typed metadata, input
#'   provenance, text lineage, stage provenance, mode-specific results,
#'   optional paragraphs and reliability output, and derived issues.
build_analysis_result <- function(
  texts_df,
  results_table,
  uuid,
  mode,
  research_background,
  style_prompt,
  irr_result = NULL,
  language,
  by_column_name = NULL,
  by_column_lookup = NULL,
  models,
  categories = NULL,
  exclusive_categories = NULL,
  scoring_characteristic = NULL,
  topics = NULL,
  exclusive_topics = NULL,
  codes = NULL,
  assign_multiple_categories = FALSE,
  human_in_the_loop = FALSE,
  write_paragraphs = FALSE,
  context_window = list(),
  stage_prompt_previews = list(),
  stage_execution_rows = NULL,
  app_version = getOption("kwallm__app_version", NULL),
  input_info = list(),
  source_texts = NULL,
  candidate_topics = character(),
  reduced_topics = character(),
  topics_were_edited = FALSE,
  irr_sample = NULL
) {
  stopifnot(is.data.frame(texts_df))
  stopifnot(is.data.frame(results_table))

  normalize_anonymization_mode <- function(value) {
    if (is.null(value) || !length(value) || is.na(value)) {
      return(NULL)
    }

    value <- as.character(value)[1]
    if (!nzchar(trimws(value))) {
      return(NULL)
    }

    if (identical(value, "simple")) {
      return("regex")
    }

    value
  }

  paragraph_entries <- attr(results_table, "paragraphs", exact = TRUE)

  mode_id <- .kwallm_mode_id_from_display(mode)
  texts_df <- .kwallm_prepare_texts_df(texts_df)
  if (!is.null(source_texts)) {
    if (length(source_texts) != nrow(texts_df)) {
      stop("source_texts must match nrow(texts_df)")
    }

    texts_df$source_text <- as.character(source_texts)
    texts_df$source_document_id <- match(
      texts_df$source_text,
      unique(texts_df$source_text)
    )
  }

  text_lineage <- .kwallm_build_text_lineage(
    texts_df = texts_df,
    by_column_lookup = by_column_lookup
  )
  topic_reduction_info <- attr(
    reduced_topics,
    "reduction_summary",
    exact = TRUE
  )

  metadata <- AnalysisMetadata(
    run_id = uuid,
    mode_id = mode_id,
    language = language,
    timestamp = Sys.time(),
    research_background = as.character(research_background %||% ""),
    app_version = if (is.null(app_version) || !nzchar(app_version)) {
      NULL
    } else {
      as.character(app_version)
    }
  )

  input <- AnalysisInput(
    file_type = input_info$file_type %||% NULL,
    selected_sheet = input_info$selected_sheet %||% NULL,
    text_column = input_info$text_column %||% NULL,
    grouping_column = by_column_name %||% input_info$grouping_column %||% NULL,
    filter_spec = input_info$filter_spec %||% NULL,
    txt_split_lines = input_info$txt_split_lines %||% NULL,
    anonymization_requested_mode = normalize_anonymization_mode(
      input_info$anonymization_requested_mode %||% NULL
    ),
    anonymization_applied_mode = normalize_anonymization_mode(
      input_info$anonymization_applied_mode %||% NULL
    ),
    anonymization_completed = input_info$anonymization_completed %||% NULL,
    split_enabled = input_info$split_enabled %||% !is.null(source_texts),
    split_chunk_size = input_info$split_chunk_size %||% NULL,
    split_overlap = input_info$split_overlap %||% NULL
  )

  stage_models <- .kwallm_build_stage_models(
    mode_id = mode_id,
    models = models,
    write_paragraphs = write_paragraphs,
    topic_reduction_info = topic_reduction_info
  )
  stage_prompts <- .kwallm_build_stage_prompts(stage_prompt_previews)
  stage_executions <- .kwallm_build_stage_executions(stage_execution_rows)

  result_payload <- switch(
    mode_id,
    categorization = .kwallm_build_categorization_result(
      texts_df = texts_df,
      results_table = results_table,
      labels = categories,
      exclusive_labels = exclusive_categories,
      multi_label = assign_multiple_categories
    ),
    scoring = .kwallm_build_scoring_result(
      texts_df = texts_df,
      results_table = results_table,
      scoring_characteristic = scoring_characteristic
    ),
    topic_extraction = .kwallm_build_topic_result(
      texts_df = texts_df,
      results_table = results_table,
      topics = topics,
      exclusive_topics = exclusive_topics,
      multi_label = assign_multiple_categories,
      context_window = context_window,
      candidate_topics = candidate_topics,
      reduced_topics = reduced_topics,
      human_edited = topics_were_edited
    ),
    marking = .kwallm_build_marking_result(
      texts_df = texts_df,
      results_table = results_table,
      codes = codes
    )
  )

  mode_config <- .kwallm_build_mode_config(
    mode_id = mode_id,
    scoring_characteristic = scoring_characteristic,
    assign_multiple_categories = assign_multiple_categories,
    human_in_the_loop = human_in_the_loop,
    write_paragraphs = write_paragraphs,
    style_prompt = style_prompt,
    context_window = context_window
  )

  paragraphs <- .kwallm_build_paragraph_set(
    paragraphs = paragraph_entries,
    mode_id = mode_id,
    texts_df = texts_df,
    labels_df = if (
      inherits(result_payload, c("CategorizationResult", "TopicResult"))
    ) {
      result_payload@labels
    } else {
      NULL
    },
    codes_df = if (inherits(result_payload, "MarkingResult")) {
      result_payload@codes
    } else {
      NULL
    }
  )

  bad_paragraphs <- paragraphs@paragraphs[
    paragraphs@paragraphs$prompt_fits %in% FALSE,
    ,
    drop = FALSE
  ]
  issues <- if (!nrow(bad_paragraphs)) {
    .kwallm_empty_issues()
  } else {
    data.frame(
      stage_id = rep("paragraph_generation", nrow(bad_paragraphs)),
      level = rep("warning", nrow(bad_paragraphs)),
      issue_code = rep("paragraph_prompt_overflow", nrow(bad_paragraphs)),
      message = rep(
        "Paragraph prompt did not fit in the model context window.",
        nrow(bad_paragraphs)
      ),
      stringsAsFactors = FALSE
    )
  }

  AnalysisResult(
    metadata = metadata,
    input = input,
    text_lineage = text_lineage,
    stage_models = stage_models,
    stage_prompts = stage_prompts,
    stage_executions = stage_executions,
    results = result_payload,
    paragraphs = paragraphs,
    reliability = if (is.null(irr_result)) {
      NULL
    } else {
      ReliabilityResult(
        summary = irr_result,
        sample = irr_sample
      )
    },
    issues = issues,
    mode_config = mode_config
  )
}


# 2 Component builders ---------------------------------------------------------

# Builds the larger typed pieces that go inside AnalysisResult.
# We keep these near the top so the file reads from whole-object parts down to details.

# Builds the document-to-analysis lineage tables used by exports and reports.
# We use this to preserve source document history after preprocessing and splitting.
.kwallm_build_text_lineage <- function(texts_df, by_column_lookup = NULL) {
  texts_df <- .kwallm_prepare_texts_df(texts_df)

  document_groups <- .kwallm_empty_document_groups()
  if (
    !is.null(by_column_lookup) &&
      is.data.frame(by_column_lookup) &&
      nrow(by_column_lookup) &&
      all(c("text", "by_value") %in% names(by_column_lookup))
  ) {
    source_lookup <- unique(texts_df[c("source_document_id", "source_text")])
    merged <- merge(
      source_lookup,
      by_column_lookup,
      by.x = "source_text",
      by.y = "text",
      all.x = FALSE,
      all.y = FALSE
    )

    if (nrow(merged)) {
      document_groups <- unique(data.frame(
        source_document_id = merged$source_document_id,
        group_value = as.character(merged$by_value),
        stringsAsFactors = FALSE
      ))
    }
  }

  source_documents <- unique(data.frame(
    source_document_id = texts_df$source_document_id,
    source_text = as.character(texts_df$source_text),
    stringsAsFactors = FALSE
  ))

  documents <- unique(data.frame(
    document_id = texts_df$document_id,
    source_document_id = texts_df$source_document_id,
    document_text = as.character(texts_df$raw),
    stringsAsFactors = FALSE
  ))

  analysis_units <- unique(data.frame(
    analysis_unit_id = texts_df$analysis_unit_id,
    preprocessed_text = as.character(texts_df$preprocessed),
    stringsAsFactors = FALSE
  ))

  document_units <- unique(data.frame(
    document_id = texts_df$document_id,
    analysis_unit_id = texts_df$analysis_unit_id,
    stringsAsFactors = FALSE
  ))

  TextLineage(
    source_documents = source_documents,
    documents = documents,
    analysis_units = analysis_units,
    document_units = document_units,
    document_groups = document_groups
  )
}

# Builds the stage-to-model mapping for the current mode.
# We use this to record which model handled each analysis stage in the export contract.
.kwallm_build_stage_models <- function(
  mode_id,
  models,
  write_paragraphs = FALSE,
  topic_reduction_info = NULL
) {
  provider_fields <- function(provider) {
    provider_class <- class(provider)
    url <- if (is.null(provider)) {
      NULL
    } else {
      tryCatch(provider$url, error = function(e) NULL)
    }

    list(
      kind = if (length(provider_class)) provider_class[[1]] else NA_character_,
      model_id = if (is.null(provider) || is.null(provider$parameters)) {
        NA_character_
      } else {
        as.character(provider$parameters$model %||% NA_character_)
      },
      api_url = if (is.null(url) || !nzchar(url)) {
        NA_character_
      } else {
        as.character(url)
      }
    )
  }

  main_provider <- provider_fields(models$main %||% NULL)
  large_provider <- provider_fields(models$large %||% NULL)

  rows <- switch(
    mode_id,
    categorization = data.frame(
      stage_id = c(
        "categorization",
        if (isTRUE(write_paragraphs)) "paragraph_generation"
      ),
      provider_kind = c(
        main_provider$kind,
        if (isTRUE(write_paragraphs)) main_provider$kind
      ),
      api_url = c(
        main_provider$api_url,
        if (isTRUE(write_paragraphs)) main_provider$api_url
      ),
      model_id = c(
        main_provider$model_id,
        if (isTRUE(write_paragraphs)) main_provider$model_id
      ),
      stringsAsFactors = FALSE
    ),
    scoring = data.frame(
      stage_id = "scoring",
      provider_kind = main_provider$kind,
      api_url = main_provider$api_url,
      model_id = main_provider$model_id,
      stringsAsFactors = FALSE
    ),
    topic_extraction = data.frame(
      stage_id = c(
        "topic_candidate_generation",
        "topic_reduction",
        if (
          isTRUE(topic_reduction_info$not_applicable_check_performed %||% FALSE)
        ) {
          "topic_not_applicable_check"
        },
        "topic_assignment",
        if (isTRUE(write_paragraphs)) "paragraph_generation"
      ),
      provider_kind = c(
        main_provider$kind,
        large_provider$kind,
        if (
          isTRUE(topic_reduction_info$not_applicable_check_performed %||% FALSE)
        ) {
          large_provider$kind
        },
        main_provider$kind,
        if (isTRUE(write_paragraphs)) main_provider$kind
      ),
      api_url = c(
        main_provider$api_url,
        large_provider$api_url,
        if (
          isTRUE(topic_reduction_info$not_applicable_check_performed %||% FALSE)
        ) {
          large_provider$api_url
        },
        main_provider$api_url,
        if (isTRUE(write_paragraphs)) main_provider$api_url
      ),
      model_id = c(
        main_provider$model_id,
        large_provider$model_id,
        if (
          isTRUE(topic_reduction_info$not_applicable_check_performed %||% FALSE)
        ) {
          large_provider$model_id
        },
        main_provider$model_id,
        if (isTRUE(write_paragraphs)) main_provider$model_id
      ),
      stringsAsFactors = FALSE
    ),
    marking = data.frame(
      stage_id = c(
        "marking",
        if (isTRUE(write_paragraphs)) "paragraph_generation"
      ),
      provider_kind = c(
        main_provider$kind,
        if (isTRUE(write_paragraphs)) main_provider$kind
      ),
      api_url = c(
        main_provider$api_url,
        if (isTRUE(write_paragraphs)) main_provider$api_url
      ),
      model_id = c(
        main_provider$model_id,
        if (isTRUE(write_paragraphs)) main_provider$model_id
      ),
      stringsAsFactors = FALSE
    ),
    .kwallm_empty_stage_models()
  )

  unique(rows)
}

# Builds the stage-to-prompt mapping for the current mode.
# We use this to store short prompt previews in metadata and report context.
.kwallm_build_stage_prompts <- function(stage_prompt_previews = list()) {
  rows <- .kwallm_empty_stage_prompts()

  if (is.null(stage_prompt_previews)) {
    return(rows)
  }

  if (!is.list(stage_prompt_previews)) {
    stop("stage_prompt_previews must be a named list")
  }

  if (!length(stage_prompt_previews)) {
    return(rows)
  }

  stage_ids <- names(stage_prompt_previews)
  if (is.null(stage_ids)) {
    stop("stage_prompt_previews must be a named list")
  }

  prompt_values <- vapply(
    stage_prompt_previews,
    function(value) {
      if (is.null(value)) {
        return("")
      }
      as.character(value)[1]
    },
    character(1)
  )
  keep <- nzchar(stage_ids) & nzchar(trimws(prompt_values))

  if (any(keep)) {
    rows <- rbind(
      rows,
      data.frame(
        stage_id = stage_ids[keep],
        prompt_preview = prompt_values[keep],
        stringsAsFactors = FALSE
      )
    )
  }

  unique(rows)
}

# Builds the per-call execution provenance table.
# We use this to keep retry and duration metadata alongside stage models and prompts.
.kwallm_build_stage_executions <- function(stage_execution_rows = NULL) {
  if (is.null(stage_execution_rows) || !is.data.frame(stage_execution_rows)) {
    return(.kwallm_empty_stage_executions())
  }

  if (!nrow(stage_execution_rows)) {
    return(.kwallm_empty_stage_executions())
  }

  required_cols <- c(
    "prompt_id",
    "stage_id",
    "model_id",
    "started_at",
    "completed_at",
    "duration_ms",
    "attempt_count",
    "retry_count",
    "max_tries",
    "retry_delay_seconds",
    "max_interactions",
    "completion_status",
    "error_messages",
    "final_error_message"
  )

  for (column in setdiff(required_cols, names(stage_execution_rows))) {
    stage_execution_rows[[column]] <- NA
  }

  rows <- stage_execution_rows[required_cols]
  rows$prompt_id <- as.character(rows$prompt_id)
  rows$stage_id <- as.character(rows$stage_id)
  rows$model_id <- as.character(rows$model_id)
  rows$started_at <- as.character(rows$started_at)
  rows$completed_at <- as.character(rows$completed_at)
  rows$duration_ms <- as.numeric(rows$duration_ms)
  rows$attempt_count <- as.integer(rows$attempt_count)
  rows$retry_count <- as.integer(rows$retry_count)
  rows$max_tries <- as.integer(rows$max_tries)
  rows$retry_delay_seconds <- as.numeric(rows$retry_delay_seconds)
  rows$max_interactions <- as.integer(rows$max_interactions)
  rows$completion_status <- as.character(rows$completion_status)
  rows$error_messages <- as.character(rows$error_messages)
  rows$final_error_message <- as.character(rows$final_error_message)

  unique(rows)
}

# Builds the typed categorization payload.
# We use this to normalize both single-label and multi-label categorization output.
.kwallm_build_categorization_result <- function(
  texts_df,
  results_table,
  labels,
  exclusive_labels,
  multi_label
) {
  labels_df <- .kwallm_build_labels(
    values = labels %||%
      if ("result" %in% names(results_table)) {
        results_table$result
      } else {
        names(results_table)[setdiff(names(results_table), "text")]
      },
    exclusive_values = exclusive_labels
  )

  assignments <- if (!isTRUE(multi_label)) {
    .kwallm_build_assignments_from_single(
      texts_df,
      results_table$result,
      labels_df
    )
  } else {
    .kwallm_build_assignments_from_multi(texts_df, results_table, labels_df)
  }

  CategorizationResult(
    labels = labels_df,
    assignments = assignments,
    multi_label = isTRUE(multi_label)
  )
}

# Builds the typed scoring payload.
# We use this to store numeric scores in one consistent table with the configured label.
.kwallm_build_scoring_result <- function(
  texts_df,
  results_table,
  scoring_characteristic
) {
  scores <- unique(data.frame(
    analysis_unit_id = texts_df$analysis_unit_id,
    score = as.numeric(results_table$result),
    stringsAsFactors = FALSE
  ))

  ScoringResult(
    scores = scores,
    characteristic = as.character(scoring_characteristic %||% "Score"),
    scale_min = 0,
    scale_max = 100
  )
}

# Builds the typed topic payload.
# We use this to keep topic assignments together with the topic-generation history.
.kwallm_build_topic_result <- function(
  texts_df,
  results_table,
  topics,
  exclusive_topics,
  multi_label,
  context_window = list(),
  candidate_topics = character(),
  reduced_topics = character(),
  human_edited = FALSE
) {
  reduction_summary <- attr(
    reduced_topics,
    "reduction_summary",
    exact = TRUE
  ) %||%
    list()

  labels_df <- .kwallm_build_labels(
    values = topics,
    exclusive_values = exclusive_topics
  )

  assignments <- if (!isTRUE(multi_label)) {
    .kwallm_build_assignments_from_single(
      texts_df,
      results_table$result,
      labels_df
    )
  } else {
    .kwallm_build_assignments_from_multi(texts_df, results_table, labels_df)
  }

  TopicResult(
    labels = labels_df,
    assignments = assignments,
    multi_label = isTRUE(multi_label),
    topic_provenance = TopicProvenance(
      candidate_topics = as.character(candidate_topics %||% character()),
      reduced_topics = as.character(reduced_topics %||% character()),
      final_topics = labels_df$label_text,
      human_edited = isTRUE(human_edited),
      not_applicable_requested = isTRUE(
        reduction_summary$not_applicable_requested %||% FALSE
      ),
      auto_added_not_applicable = isTRUE(
        reduction_summary$auto_added_not_applicable %||% FALSE
      ),
      not_applicable_check_performed = isTRUE(
        reduction_summary$not_applicable_check_performed %||% FALSE
      ),
      reduction_iterations = if (
        is.null(reduction_summary$reduction_iterations)
      ) {
        NULL
      } else {
        as.integer(reduction_summary$reduction_iterations)
      },
      chunk_size = context_window$chunk_size %||% NULL,
      draws = context_window$draws %||% NULL,
      n_chunks = context_window$n_chunks %||% NULL,
      context_window_tokens = context_window$n_tokens_context_window %||% NULL
    )
  )
}

# Builds the typed marking payload.
# We use this to normalize marked spans into codes, chunks, and individual markings.
.kwallm_build_marking_result <- function(texts_df, results_table, codes) {
  column_or <- function(column, default) {
    if (column %in% names(rows)) {
      return(rows[[column]])
    }

    rep(default, nrow(rows))
  }

  rows <- results_table[
    !is.na(results_table$marked_text) & nzchar(results_table$marked_text),
    ,
    drop = FALSE
  ]

  code_values <- as.character(codes %||% rows$code %||% character())
  code_values <- trimws(code_values)
  code_values <- unique(code_values[!is.na(code_values) & nzchar(code_values)])
  codes_df <- data.frame(
    code_id = seq_along(code_values),
    code_text = code_values,
    stringsAsFactors = FALSE
  )

  if (!nrow(rows)) {
    return(MarkingResult(
      codes = codes_df,
      chunks = .kwallm_empty_chunks(),
      markings = .kwallm_empty_markings()
    ))
  }

  mapping <- unique(texts_df[c("raw", "analysis_unit_id")])
  rows$analysis_unit_id <- mapping$analysis_unit_id[match(
    rows$text,
    mapping$raw
  )]

  chunk_keys <- unique(rows[c("analysis_unit_id", "sub_text")])
  chunk_keys$chunk_id <- seq_len(nrow(chunk_keys))
  chunk_keys$chunk_index <- ave(
    chunk_keys$analysis_unit_id,
    chunk_keys$analysis_unit_id,
    FUN = seq_along
  )

  chunks <- data.frame(
    chunk_id = chunk_keys$chunk_id,
    analysis_unit_id = chunk_keys$analysis_unit_id,
    chunk_index = as.integer(chunk_keys$chunk_index),
    chunk_text = as.character(chunk_keys$sub_text),
    stringsAsFactors = FALSE
  )

  code_id <- codes_df$code_id[match(rows$code, codes_df$code_text)]
  chunk_id <- chunk_keys$chunk_id[match(
    paste(rows$analysis_unit_id, rows$sub_text),
    paste(chunk_keys$analysis_unit_id, chunk_keys$sub_text)
  )]

  markings <- data.frame(
    mark_id = seq_len(nrow(rows)),
    chunk_id = chunk_id,
    code_id = code_id,
    source_marked_text = as.character(
      column_or("source_marked_text", rows$marked_text)
    ),
    marked_text = as.character(rows$marked_text),
    match_start = as.integer(
      column_or("match_start", NA_integer_)
    ),
    match_end = as.integer(
      column_or("match_end", NA_integer_)
    ),
    match_distance = as.integer(
      column_or("match_distance", NA_integer_)
    ),
    match_method = as.character(
      column_or("match_method", NA_character_)
    ),
    response_status = as.character(
      column_or("response_status", "matched_all")
    ),
    stringsAsFactors = FALSE
  )

  MarkingResult(
    codes = codes_df,
    chunks = chunks,
    markings = markings
  )
}

# Builds the typed mode-specific config object.
# We use this to keep mode settings in one place instead of rebuilding them later.
.kwallm_build_mode_config <- function(
  mode_id,
  scoring_characteristic = NULL,
  assign_multiple_categories = FALSE,
  human_in_the_loop = FALSE,
  write_paragraphs = FALSE,
  style_prompt = NULL,
  context_window = list()
) {
  switch(
    mode_id,
    categorization = CategorizationConfig(
      assign_multiple_categories = isTRUE(assign_multiple_categories),
      human_in_the_loop = isTRUE(human_in_the_loop),
      write_paragraphs = isTRUE(write_paragraphs),
      paragraph_style_prompt = if (
        !is.null(style_prompt) && nzchar(style_prompt)
      ) {
        style_prompt
      } else {
        NULL
      }
    ),
    scoring = ScoringConfig(
      scoring_characteristic = as.character(scoring_characteristic %||% "Score")
    ),
    topic_extraction = TopicConfig(
      assign_multiple_categories = isTRUE(assign_multiple_categories),
      human_in_the_loop = isTRUE(human_in_the_loop),
      write_paragraphs = isTRUE(write_paragraphs),
      paragraph_style_prompt = if (
        !is.null(style_prompt) && nzchar(style_prompt)
      ) {
        style_prompt
      } else {
        NULL
      },
      topic_generation_settings = data.frame(
        setting = c("chunk_size", "draws", "n_chunks", "context_window_tokens"),
        value = as.character(c(
          context_window$chunk_size %||% NA,
          context_window$draws %||% NA,
          context_window$n_chunks %||% NA,
          context_window$n_tokens_context_window %||% NA
        )),
        stringsAsFactors = FALSE
      )
    ),
    marking = MarkingConfig(
      write_paragraphs = isTRUE(write_paragraphs),
      paragraph_style_prompt = if (
        !is.null(style_prompt) && nzchar(style_prompt)
      ) {
        style_prompt
      } else {
        NULL
      },
      text_size_tokens = as.numeric(context_window$max_tokens %||% 0),
      overlap_size_tokens = as.numeric(context_window$overlap %||% 0)
    )
  )
}

# Builds stored paragraphs and their source-document links.
# We use this to keep generated paragraphs traceable back to supporting texts.
.kwallm_build_paragraph_set <- function(
  paragraphs,
  mode_id,
  texts_df,
  labels_df = NULL,
  codes_df = NULL
) {
  if (is.null(paragraphs) || !length(paragraphs)) {
    return(ParagraphSet())
  }

  subject_kind <- if (mode_id == "marking") "code" else "label"
  subject_lookup <- if (subject_kind == "code") {
    stats::setNames(codes_df$code_id, codes_df$code_text)
  } else {
    stats::setNames(labels_df$label_id, labels_df$label_text)
  }

  documents <- unique(texts_df[c("document_id", "raw")])
  names(documents)[names(documents) == "raw"] <- "document_text"

  paragraph_rows <- vector("list", length(paragraphs))
  source_rows <- vector("list", length(paragraphs))

  for (i in seq_along(paragraphs)) {
    paragraph <- paragraphs[[i]]
    topic_name <- as.character(paragraph$topic %||% "")
    subject_id <- unname(subject_lookup[[topic_name]])
    prompt_fits <- isTRUE(paragraph$prompt_fits %||% FALSE)

    paragraph_rows[[i]] <- data.frame(
      paragraph_id = i,
      subject_kind = subject_kind,
      subject_id = as.integer(subject_id %||% NA_integer_),
      paragraph_text = as.character(paragraph$paragraph %||% ""),
      prompt_fits = prompt_fits,
      stringsAsFactors = FALSE
    )

    excerpt_texts <- as.character(paragraph$texts %||% character())
    source_texts <- excerpt_texts
    if (mode_id == "marking") {
      source_texts <- gsub("\\*\\*", "", source_texts)
    }

    matched_idx <- match(source_texts, documents$document_text)
    keep <- !is.na(matched_idx)

    if (any(keep)) {
      source_rows[[i]] <- data.frame(
        paragraph_id = rep.int(i, sum(keep)),
        document_id = documents$document_id[matched_idx[keep]],
        excerpt_text = excerpt_texts[keep],
        stringsAsFactors = FALSE
      )
    } else {
      source_rows[[i]] <- .kwallm_empty_paragraph_sources()
    }
  }

  ParagraphSet(
    paragraphs = do.call(rbind, paragraph_rows),
    paragraph_sources = unique(do.call(rbind, source_rows))
  )
}

# 3 Shared table builders ------------------------------------------------------

# Contains reusable helpers that build normalized lookup tables.
# These are shared across the mode-specific payload builders above.

# Builds the normalized label table.
# We use this to give every category or topic a stable numeric id in exports.
.kwallm_build_labels <- function(values, exclusive_values = character()) {
  values <- .kwallm_safe_unique_strings(values)
  exclusive_values <- .kwallm_safe_unique_strings(exclusive_values)

  data.frame(
    label_id = seq_along(values),
    label_text = values,
    is_exclusive = values %in% exclusive_values,
    stringsAsFactors = FALSE
  )
}

# Builds assignments for single-label output.
# We use this when each analysis unit can receive at most one label.
.kwallm_build_assignments_from_single <- function(
  texts_df,
  result_values,
  labels_df
) {
  assignments <- unique(data.frame(
    analysis_unit_id = texts_df$analysis_unit_id,
    label_text = as.character(result_values),
    stringsAsFactors = FALSE
  ))
  assignments <- assignments[
    !is.na(assignments$label_text) & nzchar(assignments$label_text),
    ,
    drop = FALSE
  ]
  assignments$label_id <- labels_df$label_id[
    match(assignments$label_text, labels_df$label_text)
  ]
  assignments[c("analysis_unit_id", "label_id")]
}

# Builds assignments for multi-label output.
# We use this when each label has its own TRUE/FALSE column in the result data frame.
.kwallm_build_assignments_from_multi <- function(
  texts_df,
  result_df,
  labels_df
) {
  rows <- vector("list", length = 0)

  for (i in seq_len(nrow(labels_df))) {
    label_text <- labels_df$label_text[[i]]
    if (!label_text %in% names(result_df)) {
      next
    }

    keep <- result_df[[label_text]] %in% TRUE
    if (!any(keep, na.rm = TRUE)) {
      next
    }

    rows[[length(rows) + 1L]] <- data.frame(
      analysis_unit_id = texts_df$analysis_unit_id[keep],
      label_id = labels_df$label_id[[i]],
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    return(.kwallm_empty_assignments())
  }

  unique(do.call(rbind, rows))
}


# 4 Small normalization helpers ------------------------------------------------

# Keeps the lowest-level helpers together at the bottom of the file.
# These functions clean incoming values before the larger builders use them.

# Cleans a character vector and drops empty values.
# We use this before building label and code lookup tables.
.kwallm_safe_unique_strings <- function(x) {
  x <- as.character(x %||% character())
  x <- trimws(x)
  unique(x[!is.na(x) & nzchar(x)])
}

# Ensures texts_df has the columns the result model expects.
# We use this as the base normalization step before building lineage or results.
.kwallm_prepare_texts_df <- function(texts_df) {
  stopifnot(is.data.frame(texts_df))

  if (!"raw" %in% names(texts_df)) {
    stop("texts_df must contain a 'raw' column")
  }

  if (!"preprocessed" %in% names(texts_df)) {
    texts_df$preprocessed <- texts_df$raw
  }

  if (!"document_id" %in% names(texts_df)) {
    texts_df$document_id <- seq_len(nrow(texts_df))
  }

  if (!"source_document_id" %in% names(texts_df)) {
    texts_df$source_document_id <- texts_df$document_id
  }

  if (!"source_text" %in% names(texts_df)) {
    texts_df$source_text <- texts_df$raw
  }

  if (!"analysis_unit_id" %in% names(texts_df)) {
    texts_df$analysis_unit_id <- match(
      texts_df$preprocessed,
      unique(texts_df$preprocessed)
    )
  }

  texts_df
}
