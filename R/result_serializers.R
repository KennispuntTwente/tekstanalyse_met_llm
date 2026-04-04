# Helpers for turning AnalysisResult into report, JSON, and Excel outputs.

# 1 Public serializers ---------------------------------------------------------

# Contains the conversion functions used outside this file.
# These functions expose the stable output shapes used by reports and exports.

#' Convert AnalysisResult to report context
#'
#' Build the list shape expected by the report templates from a typed
#' AnalysisResult object.
#'
#' @param analysis_result An AnalysisResult object.
#'
#' @return A named list matching the report template contract.
analysis_result_to_report_context <- function(analysis_result) {
  stopifnot(inherits(analysis_result, "AnalysisResult"))

  mode_id <- analysis_result@metadata@mode_id
  report_context <- list(
    schema_version = analysis_result@metadata@schema_version,
    run_id = analysis_result@metadata@run_id,
    mode_id = mode_id,
    mode = .kwallm_mode_display_from_id(mode_id),
    language = analysis_result@metadata@language,
    time = analysis_result@metadata@timestamp,
    research_background = analysis_result@metadata@research_background,
    df = .kwallm_results_raw_df(analysis_result),
    model = .kwallm_get_stage_model_id(
      analysis_result,
      c(
        "categorization",
        "scoring",
        "topic_candidate_generation",
        "topic_assignment",
        "marking"
      )
    ),
    model_reductie = .kwallm_get_stage_model_id(
      analysis_result,
      c("topic_reduction", "topic_not_applicable_check")
    ),
    prompt = .kwallm_get_stage_prompt(
      analysis_result,
      c("categorization", "scoring", "topic_assignment", "marking")
    ),
    style_prompt = .kwallm_paragraph_style_prompt(analysis_result),
    irr = if (is.null(analysis_result@reliability)) {
      NULL
    } else {
      analysis_result@reliability@summary
    },
    by_column_name = analysis_result@input@grouping_column,
    by_column_values = .kwallm_group_lookup_from_lineage(analysis_result),
    paragraphs = .kwallm_report_paragraphs(analysis_result),
    issues = analysis_result@issues
  )

  utils::modifyList(
    report_context,
    .kwallm_mode_specific_report_fields(analysis_result)
  )
}

#' Convert AnalysisResult to metadata JSON content
#'
#' Build the nested list that will be written to metadata.json.
#'
#' @param analysis_result An AnalysisResult object.
#'
#' @return A named list ready for jsonlite::toJSON().
analysis_result_to_metadata_list <- function(analysis_result) {
  stopifnot(inherits(analysis_result, "AnalysisResult"))

  list(
    schema_version = analysis_result@metadata@schema_version,
    run_id = analysis_result@metadata@run_id,
    mode_id = analysis_result@metadata@mode_id,
    language = analysis_result@metadata@language,
    timestamp = .kwallm_timestamp_string(analysis_result@metadata@timestamp),
    research_background = analysis_result@metadata@research_background,
    app_version = analysis_result@metadata@app_version,
    input = list(
      file_type = analysis_result@input@file_type,
      selected_sheet = analysis_result@input@selected_sheet,
      text_column = analysis_result@input@text_column,
      grouping_column = analysis_result@input@grouping_column,
      filter_spec = if (is.null(analysis_result@input@filter_spec)) {
        NULL
      } else {
        .kwallm_df_to_records(analysis_result@input@filter_spec)
      },
      txt_split_lines = analysis_result@input@txt_split_lines,
      anonymization_requested_mode = analysis_result@input@anonymization_requested_mode,
      anonymization_applied_mode = analysis_result@input@anonymization_applied_mode,
      anonymization_completed = analysis_result@input@anonymization_completed,
      split_enabled = analysis_result@input@split_enabled,
      split_chunk_size = analysis_result@input@split_chunk_size,
      split_overlap = analysis_result@input@split_overlap
    ),
    stage_models = .kwallm_df_to_records(analysis_result@stage_models),
    stage_prompts = .kwallm_df_to_records(analysis_result@stage_prompts),
    stage_executions = .kwallm_df_to_records(analysis_result@stage_executions),
    text_lineage = list(
      source_documents = .kwallm_df_to_records(
        analysis_result@text_lineage@source_documents
      ),
      documents = .kwallm_df_to_records(analysis_result@text_lineage@documents),
      analysis_units = .kwallm_df_to_records(
        analysis_result@text_lineage@analysis_units
      ),
      document_units = .kwallm_df_to_records(
        analysis_result@text_lineage@document_units
      ),
      document_groups = .kwallm_df_to_records(
        analysis_result@text_lineage@document_groups
      )
    ),
    mode_config = switch(
      analysis_result@metadata@mode_id,
      categorization = list(
        assign_multiple_categories = analysis_result@mode_config@assign_multiple_categories,
        human_in_the_loop = analysis_result@mode_config@human_in_the_loop,
        write_paragraphs = analysis_result@mode_config@write_paragraphs,
        paragraph_style_prompt = analysis_result@mode_config@paragraph_style_prompt
      ),
      scoring = list(
        scoring_characteristic = analysis_result@mode_config@scoring_characteristic
      ),
      topic_extraction = list(
        assign_multiple_categories = analysis_result@mode_config@assign_multiple_categories,
        human_in_the_loop = analysis_result@mode_config@human_in_the_loop,
        write_paragraphs = analysis_result@mode_config@write_paragraphs,
        paragraph_style_prompt = analysis_result@mode_config@paragraph_style_prompt,
        topic_generation_settings = .kwallm_df_to_records(
          analysis_result@mode_config@topic_generation_settings
        )
      ),
      marking = list(
        write_paragraphs = analysis_result@mode_config@write_paragraphs,
        paragraph_style_prompt = analysis_result@mode_config@paragraph_style_prompt,
        text_size_tokens = analysis_result@mode_config@text_size_tokens,
        overlap_size_tokens = analysis_result@mode_config@overlap_size_tokens
      )
    ),
    results = switch(
      analysis_result@metadata@mode_id,
      categorization = list(
        labels = .kwallm_df_to_records(analysis_result@results@labels),
        assignments = .kwallm_df_to_records(
          analysis_result@results@assignments
        ),
        multi_label = analysis_result@results@multi_label
      ),
      scoring = list(
        scores = .kwallm_df_to_records(analysis_result@results@scores),
        characteristic = analysis_result@results@characteristic,
        scale_min = analysis_result@results@scale_min,
        scale_max = analysis_result@results@scale_max
      ),
      topic_extraction = list(
        labels = .kwallm_df_to_records(analysis_result@results@labels),
        assignments = .kwallm_df_to_records(
          analysis_result@results@assignments
        ),
        multi_label = analysis_result@results@multi_label,
        topic_provenance = list(
          candidate_topics = as.character(
            analysis_result@results@topic_provenance@candidate_topics
          ),
          reduced_topics = as.character(
            analysis_result@results@topic_provenance@reduced_topics
          ),
          final_topics = as.character(
            analysis_result@results@topic_provenance@final_topics
          ),
          human_edited = analysis_result@results@topic_provenance@human_edited,
          not_applicable_requested = analysis_result@results@topic_provenance@not_applicable_requested,
          auto_added_not_applicable = analysis_result@results@topic_provenance@auto_added_not_applicable,
          not_applicable_check_performed = analysis_result@results@topic_provenance@not_applicable_check_performed,
          reduction_iterations = analysis_result@results@topic_provenance@reduction_iterations,
          chunk_size = analysis_result@results@topic_provenance@chunk_size,
          draws = analysis_result@results@topic_provenance@draws,
          n_chunks = analysis_result@results@topic_provenance@n_chunks,
          context_window_tokens = analysis_result@results@topic_provenance@context_window_tokens
        )
      ),
      marking = list(
        codes = .kwallm_df_to_records(analysis_result@results@codes),
        chunks = .kwallm_df_to_records(analysis_result@results@chunks),
        markings = .kwallm_df_to_records(analysis_result@results@markings)
      )
    ),
    paragraphs = list(
      paragraphs = .kwallm_df_to_records(analysis_result@paragraphs@paragraphs),
      paragraph_sources = .kwallm_df_to_records(
        analysis_result@paragraphs@paragraph_sources
      )
    ),
    reliability = if (is.null(analysis_result@reliability)) {
      NULL
    } else {
      list(
        summary = analysis_result@reliability@summary,
        sample = analysis_result@reliability@sample
      )
    },
    issues = .kwallm_df_to_records(analysis_result@issues)
  )
}

#' Convert AnalysisResult to Excel sheets
#'
#' Build the named list of data frames that becomes results.xlsx.
#'
#' @param analysis_result An AnalysisResult object.
#'
#' @return A named list of data frames, one per Excel sheet.
analysis_result_to_export_sheets <- function(analysis_result) {
  stopifnot(inherits(analysis_result, "AnalysisResult"))

  report_context <- analysis_result_to_report_context(analysis_result)

  sheets <- list(
    metadata = data.frame(
      field = c(
        "schema_version",
        "run_id",
        "mode_id",
        "language",
        "timestamp",
        "research_background",
        "app_version"
      ),
      value = c(
        as.character(analysis_result@metadata@schema_version),
        analysis_result@metadata@run_id,
        analysis_result@metadata@mode_id,
        analysis_result@metadata@language,
        .kwallm_timestamp_string(analysis_result@metadata@timestamp),
        analysis_result@metadata@research_background,
        .kwallm_excel_scalar(analysis_result@metadata@app_version)
      ),
      stringsAsFactors = FALSE
    ),
    input = data.frame(
      field = c(
        "file_type",
        "selected_sheet",
        "text_column",
        "grouping_column",
        "txt_split_lines",
        "anonymization_requested_mode",
        "anonymization_applied_mode",
        "anonymization_completed",
        "split_enabled",
        "split_chunk_size",
        "split_overlap"
      ),
      value = c(
        .kwallm_excel_scalar(analysis_result@input@file_type),
        .kwallm_excel_scalar(analysis_result@input@selected_sheet),
        .kwallm_excel_scalar(analysis_result@input@text_column),
        .kwallm_excel_scalar(analysis_result@input@grouping_column),
        .kwallm_excel_scalar(analysis_result@input@txt_split_lines),
        .kwallm_excel_scalar(
          analysis_result@input@anonymization_requested_mode
        ),
        .kwallm_excel_scalar(analysis_result@input@anonymization_applied_mode),
        .kwallm_excel_scalar(analysis_result@input@anonymization_completed),
        .kwallm_excel_scalar(analysis_result@input@split_enabled),
        .kwallm_excel_scalar(analysis_result@input@split_chunk_size),
        .kwallm_excel_scalar(analysis_result@input@split_overlap)
      ),
      stringsAsFactors = FALSE
    ),
    results = report_context$df,
    source_documents = analysis_result@text_lineage@source_documents,
    documents = analysis_result@text_lineage@documents,
    analysis_units = analysis_result@text_lineage@analysis_units,
    document_units = analysis_result@text_lineage@document_units,
    document_groups = analysis_result@text_lineage@document_groups,
    stage_models = analysis_result@stage_models,
    stage_prompts = analysis_result@stage_prompts,
    stage_executions = analysis_result@stage_executions,
    paragraphs = analysis_result@paragraphs@paragraphs,
    paragraph_sources = analysis_result@paragraphs@paragraph_sources,
    issues = analysis_result@issues
  )

  if (
    inherits(analysis_result@results, c("CategorizationResult", "TopicResult"))
  ) {
    sheets$labels <- analysis_result@results@labels
    sheets$assignments <- analysis_result@results@assignments
  }

  if (inherits(analysis_result@results, "ScoringResult")) {
    sheets$scores <- analysis_result@results@scores
  }

  if (inherits(analysis_result@results, "MarkingResult")) {
    sheets$codes <- analysis_result@results@codes
    sheets$chunks <- analysis_result@results@chunks
    sheets$markings <- analysis_result@results@markings
  }

  if (inherits(analysis_result@results, "TopicResult")) {
    sheets$topic_generation_settings <- data.frame(
      setting = c(
        "human_edited",
        "not_applicable_requested",
        "auto_added_not_applicable",
        "not_applicable_check_performed",
        "reduction_iterations",
        "chunk_size",
        "draws",
        "n_chunks",
        "context_window_tokens"
      ),
      value = as.character(c(
        analysis_result@results@topic_provenance@human_edited,
        analysis_result@results@topic_provenance@not_applicable_requested,
        analysis_result@results@topic_provenance@auto_added_not_applicable,
        analysis_result@results@topic_provenance@not_applicable_check_performed,
        analysis_result@results@topic_provenance@reduction_iterations,
        analysis_result@results@topic_provenance@chunk_size,
        analysis_result@results@topic_provenance@draws,
        analysis_result@results@topic_provenance@n_chunks,
        analysis_result@results@topic_provenance@context_window_tokens
      )),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(analysis_result@reliability)) {
    reliability_summary <- analysis_result@reliability@summary
    sheets$reliability <- tryCatch(
      as.data.frame(reliability_summary, stringsAsFactors = FALSE),
      error = function(e) {
        data.frame(
          key = names(reliability_summary),
          value = vapply(
            reliability_summary,
            function(x) paste(x, collapse = ", "),
            character(1)
          ),
          stringsAsFactors = FALSE
        )
      }
    )
  }

  Filter(function(x) is.data.frame(x), sheets)
}

#' Write metadata.json for one AnalysisResult
#'
#' Serialize metadata for one analysis run to metadata.json inside a temporary
#' directory. If writing fails, a text file with the error is written instead.
#'
#' @param analysis_result An AnalysisResult object.
#' @param temp_dir Directory where the output file should be written.
#'
#' @return Path to metadata.json, or to a metadata error text file on failure.
write_analysis_result_metadata_json <- function(
  analysis_result,
  temp_dir = tempdir()
) {
  output_file_json <- file.path(temp_dir, "metadata.json")
  output_file_txt <- file.path(temp_dir, "metadata_error.txt")

  tryCatch(
    {
      json_text <- jsonlite::toJSON(
        analysis_result_to_metadata_list(analysis_result),
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null"
      )
      writeLines(json_text, con = output_file_json, useBytes = TRUE)
      output_file_json
    },
    error = function(e) {
      writeLines(
        paste("Error during metadata JSON creation:", conditionMessage(e)),
        con = output_file_txt
      )
      output_file_txt
    }
  )
}


# 2 Report reconstruction helpers ----------------------------------------------

# Rebuilds the report-context tables and paragraph structures from typed results.
# The report templates still consume this specific shape.

# Rebuilds the raw categorization or topic data frame expected by reports.
# We use this for both categorization and topic extraction because they share the same shape.
.kwallm_results_raw_df_categorization <- function(analysis_result) {
  result <- analysis_result@results
  base <- .kwallm_document_unit_map(analysis_result)
  labels_lookup <- .kwallm_labels_lookup(result@labels)

  if (!isTRUE(result@multi_label)) {
    assignments <- result@assignments
    assignments$result <- labels_lookup[as.character(assignments$label_id)]
    merged <- merge(
      base,
      assignments[c("analysis_unit_id", "result")],
      by = "analysis_unit_id",
      all.x = TRUE,
      all.y = FALSE
    )
    out <- merged[c("document_text", "result")]
    names(out) <- c("text", "result")
    return(out)
  }

  out <- data.frame(text = base$document_text, stringsAsFactors = FALSE)
  for (label in result@labels$label_text) {
    out[[label]] <- FALSE
  }

  for (i in seq_len(nrow(result@labels))) {
    label_id <- result@labels$label_id[[i]]
    label_text <- result@labels$label_text[[i]]
    unit_ids <- unique(
      result@assignments$analysis_unit_id[
        result@assignments$label_id %in% label_id
      ]
    )
    out[[label_text]] <- base$analysis_unit_id %in% unit_ids
  }

  out
}

# Rebuilds the raw scoring data frame expected by reports.
# We use this so scoring templates can keep their existing input shape.
.kwallm_results_raw_df_scoring <- function(analysis_result) {
  result <- analysis_result@results
  base <- .kwallm_document_unit_map(analysis_result)

  merged <- merge(
    base,
    result@scores,
    by = "analysis_unit_id",
    all.x = TRUE,
    all.y = FALSE
  )

  out <- merged[c("document_text", "score")]
  names(out) <- c("text", "result")
  out
}

# Rebuilds the raw marking data frame expected by reports.
# We use this to expand chunk/code combinations back into the old flat table format.
.kwallm_results_raw_df_marking <- function(analysis_result) {
  result <- analysis_result@results
  base <- .kwallm_document_unit_map(analysis_result)
  codes_lookup <- .kwallm_codes_lookup(result@codes)

  if (!nrow(result@chunks)) {
    return(data.frame(
      text = character(),
      sub_text = character(),
      code = character(),
      marked_text = character(),
      stringsAsFactors = FALSE
    ))
  }

  chunk_docs <- merge(
    result@chunks,
    base[c("analysis_unit_id", "document_text")],
    by = "analysis_unit_id",
    all.x = TRUE,
    all.y = FALSE
  )

  if (!nrow(result@codes)) {
    out <- chunk_docs[c("document_text", "chunk_text")]
    out$code <- character(nrow(out))
    out$marked_text <- NA_character_
    names(out)[1:2] <- c("text", "sub_text")
    return(out)
  }

  grids <- vector("list", nrow(result@codes))
  for (i in seq_len(nrow(result@codes))) {
    grids[[i]] <- data.frame(
      chunk_docs,
      code_id = result@codes$code_id[[i]],
      stringsAsFactors = FALSE
    )
  }
  grid <- do.call(rbind, grids)

  marks <- result@markings
  merged <- merge(
    grid,
    marks[c("chunk_id", "code_id", "marked_text")],
    by = c("chunk_id", "code_id"),
    all.x = TRUE,
    all.y = FALSE
  )
  merged$code <- codes_lookup[as.character(merged$code_id)]

  out <- merged[c("document_text", "chunk_text", "code", "marked_text")]
  names(out) <- c("text", "sub_text", "code", "marked_text")
  out
}

# Dispatches to the correct raw-data reconstruction helper for the active mode.
# We use this so report_context and Excel exports share one reconstructed result table.
.kwallm_results_raw_df <- function(analysis_result) {
  switch(
    analysis_result@metadata@mode_id,
    categorization = .kwallm_results_raw_df_categorization(analysis_result),
    scoring = .kwallm_results_raw_df_scoring(analysis_result),
    topic_extraction = .kwallm_results_raw_df_categorization(analysis_result),
    marking = .kwallm_results_raw_df_marking(analysis_result)
  )
}

# Looks up the label or code text used by paragraph rows.
# We use this when rebuilding the paragraph list consumed by report templates.
.kwallm_paragraph_subject_lookup <- function(analysis_result) {
  result <- analysis_result@results

  if (inherits(result, c("CategorizationResult", "TopicResult"))) {
    return(stats::setNames(result@labels$label_text, result@labels$label_id))
  }
  if (inherits(result, "MarkingResult")) {
    return(stats::setNames(result@codes$code_text, result@codes$code_id))
  }

  stats::setNames(character(), integer())
}

# Rebuilds the paragraph list structure expected by the report templates.
# Exports keep the full lineage tables; reports use this simpler nested list.
.kwallm_report_paragraphs <- function(analysis_result) {
  paragraphs <- analysis_result@paragraphs@paragraphs
  paragraph_sources <- analysis_result@paragraphs@paragraph_sources

  if (!nrow(paragraphs)) {
    return(NULL)
  }

  subject_lookup <- .kwallm_paragraph_subject_lookup(analysis_result)
  documents <- analysis_result@text_lineage@documents[,
    c("document_id", "document_text"),
    drop = FALSE
  ]
  document_lookup <- stats::setNames(
    documents$document_text,
    documents$document_id
  )

  output <- vector("list", nrow(paragraphs))
  for (i in seq_len(nrow(paragraphs))) {
    paragraph_id <- paragraphs$paragraph_id[[i]]
    sources <- paragraph_sources[
      paragraph_sources$paragraph_id %in% paragraph_id,
      ,
      drop = FALSE
    ]
    supporting_texts <- if (nrow(sources)) {
      fallback_text <- document_lookup[as.character(sources$document_id)]
      ifelse(
        nzchar(sources$excerpt_text),
        sources$excerpt_text,
        unname(fallback_text)
      )
    } else {
      character()
    }

    output[[i]] <- list(
      topic = unname(subject_lookup[[as.character(paragraphs$subject_id[[
        i
      ]])]]),
      paragraph = paragraphs$paragraph_text[[i]],
      texts = as.character(supporting_texts),
      prompt_fits = isTRUE(paragraphs$prompt_fits[[i]])
    )
  }

  output
}

# Adds the mode-specific fields expected by current report templates.
# We use this to keep report_context small but still mode-aware.
.kwallm_mode_specific_report_fields <- function(analysis_result) {
  result <- analysis_result@results
  mode_config <- analysis_result@mode_config

  switch(
    analysis_result@metadata@mode_id,
    categorization = list(
      categories = result@labels$label_text,
      exclusive_categories = result@labels$label_text[
        result@labels$is_exclusive %in% TRUE
      ],
      assign_multiple_categories = isTRUE(result@multi_label),
      human_in_the_loop = isTRUE(mode_config@human_in_the_loop),
      write_paragraphs = isTRUE(mode_config@write_paragraphs)
    ),
    scoring = list(
      scoring_characteristic = result@characteristic
    ),
    topic_extraction = list(
      topics = result@labels$label_text,
      exclusive_topics = result@labels$label_text[
        result@labels$is_exclusive %in% TRUE
      ],
      assign_multiple_categories = isTRUE(result@multi_label),
      human_in_the_loop = isTRUE(mode_config@human_in_the_loop),
      write_paragraphs = isTRUE(mode_config@write_paragraphs),
      candidate_topics = result@topic_provenance@candidate_topics,
      reduced_topics = result@topic_provenance@reduced_topics,
      final_topics = result@topic_provenance@final_topics,
      topic_generation_settings = result@topic_provenance
    ),
    marking = list(
      codes = result@codes$code_text,
      write_paragraphs = isTRUE(mode_config@write_paragraphs),
      text_size_tokens = mode_config@text_size_tokens,
      overlap_size_tokens = mode_config@overlap_size_tokens
    )
  )
}


# 3 Structured export helpers --------------------------------------------------

# Converts typed tables into reusable lookup objects for JSON and report output.
# These helpers are shared by multiple public serializers above.

# Converts a data frame into a list of row records.
# We use this when metadata.json should preserve row structure without row names.
.kwallm_df_to_records <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list())
  }

  rows <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, .kwallm_scalar_or_null)
    rows[[i]] <- row
  }
  rows
}

# Gets the first matching model id for one or more stage ids.
# We use this to populate compact model fields in report_context.
.kwallm_get_stage_model_id <- function(analysis_result, stage_id) {
  rows <- analysis_result@stage_models
  value <- rows$model_id[rows$stage_id %in% stage_id][1]
  .kwallm_scalar_or_null(value)
}

# Gets the first matching prompt preview for one or more stage ids.
# We use this to populate compact prompt fields in report_context.
.kwallm_get_stage_prompt <- function(analysis_result, stage_id) {
  rows <- analysis_result@stage_prompts
  value <- rows$prompt_preview[rows$stage_id %in% stage_id][1]
  .kwallm_scalar_or_null(value)
}

# Gets the paragraph style instruction, which is distinct from the full prompt preview.
# We use this in report_context so existing templates keep seeing the user-entered style text.
.kwallm_paragraph_style_prompt <- function(analysis_result) {
  mode_config <- analysis_result@mode_config

  if (
    !inherits(
      mode_config,
      c(
        "CategorizationConfig",
        "TopicConfig",
        "MarkingConfig"
      )
    )
  ) {
    return(NULL)
  }

  .kwallm_scalar_or_null(mode_config@paragraph_style_prompt)
}

# Joins document rows to analysis-unit ids.
# We use this as the shared base table when reconstructing report result frames.
.kwallm_document_unit_map <- function(analysis_result) {
  merge(
    analysis_result@text_lineage@documents,
    analysis_result@text_lineage@document_units,
    by = "document_id",
    all.x = TRUE,
    all.y = FALSE
  )
}

# Rebuilds the grouped-report lookup from the lineage tables.
# We use this so grouped reports keep working after source documents are split.
.kwallm_group_lookup_from_lineage <- function(analysis_result) {
  groups <- analysis_result@text_lineage@document_groups
  if (!nrow(groups)) {
    return(NULL)
  }

  docs <- analysis_result@text_lineage@documents[,
    c("document_text", "source_document_id"),
    drop = FALSE
  ]
  merged <- merge(
    docs,
    groups,
    by = "source_document_id",
    all.x = FALSE,
    all.y = FALSE
  )

  data.frame(
    text = merged$document_text,
    by_value = merged$group_value,
    stringsAsFactors = FALSE
  )
}

# Builds a label-id to label-text lookup.
# We use this while reconstructing categorization and topic result tables.
.kwallm_labels_lookup <- function(labels_df) {
  stats::setNames(labels_df$label_text, labels_df$label_id)
}

# Builds a code-id to code-text lookup.
# We use this while reconstructing marking result tables.
.kwallm_codes_lookup <- function(codes_df) {
  stats::setNames(codes_df$code_text, codes_df$code_id)
}


# 4 Small value helpers --------------------------------------------------------

# Keeps small coercion and formatting helpers at the bottom of the file.
# These helpers are intentionally simple and feed the larger serializers above.

# Converts a canonical mode id back to the display label used in reports.
# We use this so report_context keeps the same mode wording as the UI.
.kwallm_mode_display_from_id <- function(mode_id) {
  switch(
    mode_id,
    categorization = "Categorisatie",
    scoring = "Scoren",
    topic_extraction = "Onderwerpextractie",
    marking = "Markeren",
    mode_id
  )
}

# Collapses vectors that are entirely missing to NULL.
# We use this before writing JSON records so empty values do not become noisy arrays.
.kwallm_null_if_all_missing <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(NULL)
  }
  x
}

# Returns a scalar value when possible, otherwise a cleaned vector or NULL.
# We use this to make JSON output stable for both single values and small vectors.
.kwallm_scalar_or_null <- function(x) {
  x <- .kwallm_null_if_all_missing(x)
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) == 1L) {
    return(unname(x[[1]]))
  }
  unname(x)
}

# Formats timestamps as UTC ISO-8601 strings.
# We use this so metadata.json and Excel exports share the same timestamp format.
.kwallm_timestamp_string <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Converts a value to a single Excel-safe character cell.
# We use this for the small metadata and input sheets inside results.xlsx.
.kwallm_excel_scalar <- function(x) {
  x <- .kwallm_scalar_or_null(x)
  if (is.null(x)) {
    return(NA_character_)
  }
  as.character(x)
}
