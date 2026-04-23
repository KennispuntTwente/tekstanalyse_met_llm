# Helpers for turning AnalysisResult into report, JSON, and Excel outputs.

# 1 Public serializers ---------------------------------------------------------

# Contains the conversion functions used outside this file.
# These functions expose the stable output shapes used by exports.

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
    analysis_name = analysis_result@metadata@analysis_name,
    app_version = analysis_result@metadata@app_version,
    text_counts = .kwallm_analysis_result_text_counts(analysis_result),
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
    stage_models = .kwallm_stage_records_by_stage(
      analysis_result@stage_models,
      singleton = TRUE
    ),
    stage_prompts = .kwallm_stage_records_by_stage(
      analysis_result@stage_prompts,
      singleton = TRUE
    ),
    stage_executions = .kwallm_stage_records_by_stage(
      analysis_result@stage_executions,
      singleton = FALSE
    ),
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
        multi_label = analysis_result@results@multi_label,
        assignments = .kwallm_df_to_records(
          analysis_result@results@assignments
        ),
        response_status = .kwallm_df_to_records(
          analysis_result@results@response_status
        )
      ),
      scoring = list(
        scores = .kwallm_df_to_records(analysis_result@results@scores),
        characteristic = analysis_result@results@characteristic,
        scale_min = analysis_result@results@scale_min,
        scale_max = analysis_result@results@scale_max
      ),
      topic_extraction = list(
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
          single_topic_fallback_applied = analysis_result@results@topic_provenance@single_topic_fallback_applied,
          not_applicable_check_performed = analysis_result@results@topic_provenance@not_applicable_check_performed,
          reduction_iterations = analysis_result@results@topic_provenance@reduction_iterations,
          batch_size = analysis_result@results@topic_provenance@batch_size,
          draws = analysis_result@results@topic_provenance@draws,
          n_batches = analysis_result@results@topic_provenance@n_batches,
          context_window_tokens = analysis_result@results@topic_provenance@context_window_tokens
        ),
        labels = .kwallm_df_to_records(analysis_result@results@labels),
        multi_label = analysis_result@results@multi_label,
        assignments = .kwallm_df_to_records(
          analysis_result@results@assignments
        ),
        response_status = .kwallm_df_to_records(
          analysis_result@results@response_status
        )
      ),
      marking = list(
        codes = .kwallm_df_to_records(analysis_result@results@codes),
        chunks = .kwallm_df_to_records(analysis_result@results@chunks),
        responses = .kwallm_df_to_records(analysis_result@results@responses),
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

  text_counts <- .kwallm_analysis_result_text_counts(analysis_result)

  sheets <- list(
    metadata = data.frame(
      field = c(
        "schema_version",
        "run_id",
        "mode_id",
        "language",
        "timestamp",
        "research_background",
        "analysis_name",
        "app_version",
        "source_documents",
        "documents",
        "analysis_units"
      ),
      value = c(
        as.character(analysis_result@metadata@schema_version),
        analysis_result@metadata@run_id,
        analysis_result@metadata@mode_id,
        analysis_result@metadata@language,
        .kwallm_timestamp_string(analysis_result@metadata@timestamp),
        analysis_result@metadata@research_background,
        analysis_result@metadata@analysis_name,
        .kwallm_excel_scalar(analysis_result@metadata@app_version),
        .kwallm_excel_scalar(text_counts$source_documents),
        .kwallm_excel_scalar(text_counts$documents),
        .kwallm_excel_scalar(text_counts$analysis_units)
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
    results = .kwallm_report_results_df(analysis_result),
    source_documents = analysis_result@text_lineage@source_documents,
    documents = analysis_result@text_lineage@documents,
    analysis_units = analysis_result@text_lineage@analysis_units,
    document_units = analysis_result@text_lineage@document_units,
    document_groups = analysis_result@text_lineage@document_groups,
    stage_models = analysis_result@stage_models,
    stage_prompts = analysis_result@stage_prompts,
    stage_executions = .kwallm_excel_df(analysis_result@stage_executions),
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

  if (inherits(analysis_result@results, "CategorizationResult")) {
    sheets$categorization_response_status <-
      analysis_result@results@response_status
  }

  if (inherits(analysis_result@results, "ScoringResult")) {
    sheets$scores <- analysis_result@results@scores
  }

  if (inherits(analysis_result@results, "MarkingResult")) {
    sheets$codes <- analysis_result@results@codes
    sheets$chunks <- analysis_result@results@chunks
    sheets$marking_responses <- analysis_result@results@responses
    sheets$markings <- analysis_result@results@markings
  }

  if (inherits(analysis_result@results, "TopicResult")) {
    sheets$topic_generation_settings <- data.frame(
      setting = c(
        "human_edited",
        "not_applicable_requested",
        "auto_added_not_applicable",
        "single_topic_fallback_applied",
        "not_applicable_check_performed",
        "reduction_iterations",
        "batch_size",
        "draws",
        "n_batches",
        "context_window_tokens"
      ),
      value = vapply(
        list(
          analysis_result@results@topic_provenance@human_edited,
          analysis_result@results@topic_provenance@not_applicable_requested,
          analysis_result@results@topic_provenance@auto_added_not_applicable,
          analysis_result@results@topic_provenance@single_topic_fallback_applied,
          analysis_result@results@topic_provenance@not_applicable_check_performed,
          analysis_result@results@topic_provenance@reduction_iterations,
          analysis_result@results@topic_provenance@batch_size,
          analysis_result@results@topic_provenance@draws,
          analysis_result@results@topic_provenance@n_batches,
          analysis_result@results@topic_provenance@context_window_tokens
        ),
        .kwallm_excel_scalar,
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  }

  if (inherits(analysis_result@results, "TopicResult")) {
    sheets$topic_response_status <- analysis_result@results@response_status
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
    if (is.data.frame(analysis_result@reliability@sample)) {
      sheets$reliability_sample <- analysis_result@reliability@sample
    }
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

# Rebuilds the report tables and lookup structures from typed results.
# Reports and exports share these smaller helpers directly.

# Rebuilds the categorization/topic report table from typed results.
# We use this for both categorization and topic extraction because they share the same shape.
.kwallm_report_results_df_categorization <- function(analysis_result) {
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
    out <- merged[c("document_id", "preprocessed_text", "result")]
    names(out) <- c("document_id", "text", "result")
    return(out)
  }

  out <- data.frame(
    document_id = base$document_id,
    text = base$preprocessed_text,
    stringsAsFactors = FALSE
  )
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

# Rebuilds the scoring report table from typed results.
# We use this so scoring templates can keep their existing input shape.
.kwallm_report_results_df_scoring <- function(analysis_result) {
  result <- analysis_result@results
  base <- .kwallm_document_unit_map(analysis_result)

  merged <- merge(
    base,
    result@scores,
    by = "analysis_unit_id",
    all.x = TRUE,
    all.y = FALSE
  )

  out <- merged[c("document_id", "preprocessed_text", "score")]
  names(out) <- c("document_id", "text", "result")
  out
}

# Rebuilds the marking report table from typed results.
# We use this to expand chunk/code combinations back into the flat table format.
.kwallm_report_results_df_marking <- function(analysis_result) {
  result <- analysis_result@results
  base <- .kwallm_document_unit_map(analysis_result)
  codes_lookup <- .kwallm_codes_lookup(result@codes)

  if (!nrow(result@chunks)) {
    return(data.frame(
      document_id = integer(),
      text = character(),
      chunk_text = character(),
      code = character(),
      marked_text = character(),
      response_status = character(),
      stringsAsFactors = FALSE
    ))
  }

  chunk_docs <- merge(
    result@chunks,
    base[c("analysis_unit_id", "document_id", "preprocessed_text")],
    by = "analysis_unit_id",
    all.x = TRUE,
    all.y = FALSE
  )

  if (!nrow(result@codes)) {
    out <- chunk_docs[c("document_id", "preprocessed_text", "chunk_text")]
    out$code <- character(nrow(out))
    out$marked_text <- NA_character_
    out$response_status <- NA_character_
    names(out)[1:3] <- c("document_id", "text", "chunk_text")
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

  responses <- result@responses
  marks <- result@markings
  merged <- merge(
    grid,
    responses[c("chunk_id", "code_id", "response_status")],
    by = c("chunk_id", "code_id"),
    all.x = TRUE,
    all.y = FALSE
  )
  merged <- merge(
    merged,
    marks[c("chunk_id", "code_id", "marked_text")],
    by = c("chunk_id", "code_id"),
    all.x = TRUE,
    all.y = FALSE
  )
  merged$code <- codes_lookup[as.character(merged$code_id)]

  out <- merged[c(
    "document_id",
    "preprocessed_text",
    "chunk_text",
    "code",
    "marked_text",
    "response_status"
  )]
  names(out) <- c(
    "document_id",
    "text",
    "chunk_text",
    "code",
    "marked_text",
    "response_status"
  )
  out
}

# Dispatches to the correct report-table reconstruction helper for the active mode.
# We use this so reports and Excel exports share one reconstructed result table.
.kwallm_report_results_df <- function(analysis_result) {
  switch(
    analysis_result@metadata@mode_id,
    categorization = .kwallm_report_results_df_categorization(analysis_result),
    scoring = .kwallm_report_results_df_scoring(analysis_result),
    topic_extraction = .kwallm_report_results_df_categorization(
      analysis_result
    ),
    marking = .kwallm_report_results_df_marking(analysis_result)
  )
}

# Looks up the label or code text used by paragraph rows.
# We use this when report templates need subject labels for paragraph rows.
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

# Resolves the supporting texts for one paragraph row.
# We use excerpts when present and otherwise fall back to full document text.
.kwallm_paragraph_supporting_texts <- function(analysis_result, paragraph_id) {
  paragraph_sources <- analysis_result@paragraphs@paragraph_sources
  sources <- paragraph_sources[
    paragraph_sources$paragraph_id %in% paragraph_id,
    ,
    drop = FALSE
  ]
  if (!nrow(sources)) {
    return(character())
  }

  documents <- analysis_result@text_lineage@documents[,
    c("document_id", "document_text"),
    drop = FALSE
  ]
  document_lookup <- stats::setNames(
    documents$document_text,
    documents$document_id
  )

  excerpt_text <- as.character(sources$excerpt_text)
  fallback_text <- unname(document_lookup[as.character(sources$document_id)])

  supporting_texts <- ifelse(
    !is.na(excerpt_text) & nzchar(excerpt_text),
    excerpt_text,
    fallback_text
  )

  as.character(stats::na.omit(supporting_texts))
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

  lapply(seq_len(nrow(df)), function(i) {
    lapply(as.list(df[i, , drop = FALSE]), .kwallm_scalar_or_null)
  })
}

# Groups stage-keyed provenance rows into a nested metadata structure.
# We use this so metadata.json is organized by stage instead of one flat list.
.kwallm_stage_records_by_stage <- function(df, singleton = FALSE) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list())
  }

  stage_ids <- unique(as.character(df$stage_id))
  grouped <- vector("list", length(stage_ids))
  names(grouped) <- stage_ids

  for (stage_id in stage_ids) {
    stage_rows <- df[df$stage_id == stage_id, , drop = FALSE]
    stage_rows$stage_id <- NULL
    stage_records <- .kwallm_df_to_records(stage_rows)

    if (isTRUE(singleton)) {
      grouped[[stage_id]] <- stage_records[[1]] %||% list()
    } else {
      grouped[[stage_id]] <- stage_records
    }
  }

  grouped
}

# Computes the main text-count layers used across metadata and reports.
# We keep these counts together so every output surface describes the run the same way.
.kwallm_analysis_result_text_counts <- function(analysis_result) {
  lineage <- analysis_result@text_lineage

  list(
    source_documents = nrow(lineage@source_documents),
    documents = nrow(lineage@documents),
    analysis_units = nrow(lineage@analysis_units)
  )
}

# Builds the short count summary shown near the top of each HTML report.
# This keeps the wording consistent with the exported metadata fields.
.kwallm_report_text_count_summary <- function(analysis_result) {
  counts <- .kwallm_analysis_result_text_counts(analysis_result)

  if (identical(analysis_result@metadata@language, "nl")) {
    parts <- c(
      paste0(counts$source_documents, " bronteksten/rijen zijn meegenomen")
    )

    if (counts$documents != counts$source_documents) {
      parts <- c(
        parts,
        paste0(
          "na splitsen leverde dat ",
          counts$documents,
          " analyseerbare teksten/chunks op"
        )
      )
    }

    parts <- c(
      parts,
      paste0(
        counts$analysis_units,
        " unieke teksten zijn naar het LLM gestuurd"
      )
    )

    return(paste0(paste(parts, collapse = "; "), "."))
  }

  parts <- c(
    paste0(counts$source_documents, " source texts/rows were included")
  )

  if (counts$documents != counts$source_documents) {
    parts <- c(
      parts,
      paste0(
        "after splitting, this became ",
        counts$documents,
        " texts/chunks in the results"
      )
    )
  }

  parts <- c(
    parts,
    paste0(counts$analysis_units, " unique texts were sent to the LLM")
  )

  paste0(paste(parts, collapse = "; "), ".")
}

# Gets the first matching model id for one or more stage ids.
# We use this for report templates that refer to stage-specific model ids.
.kwallm_get_stage_model_id <- function(analysis_result, stage_id) {
  rows <- analysis_result@stage_models
  value <- rows$model_id[rows$stage_id %in% stage_id][1]
  .kwallm_scalar_or_null(value)
}

# Joins document rows to analysis-unit ids and their preprocessed text.
# We use this as the shared base table when reconstructing report result frames.
# The preprocessed_text column is the text after any anonymization/preprocessing
# and is the text the LLM actually analyzed.
.kwallm_document_unit_map <- function(analysis_result) {
  doc_units <- merge(
    analysis_result@text_lineage@documents,
    analysis_result@text_lineage@document_units,
    by = "document_id",
    all.x = TRUE,
    all.y = FALSE
  )
  merge(
    doc_units,
    analysis_result@text_lineage@analysis_units,
    by = "analysis_unit_id",
    all.x = TRUE,
    all.y = FALSE
  )
}

# Rebuilds the grouped-report lookup from the lineage tables.
# We keep document ids here so grouped reports can join rows by stable identity
# instead of text value alone when duplicate texts appear in the same group.
.kwallm_report_group_lookup <- function(analysis_result) {
  groups <- analysis_result@text_lineage@document_groups
  if (!nrow(groups)) {
    return(NULL)
  }

  docs <- analysis_result@text_lineage@documents[,
    c("document_id", "document_text", "source_document_id"),
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
    document_id = merged$document_id,
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
# We use this so generated filenames keep the same mode wording as the UI.
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

# Returns a scalar value when possible, otherwise a cleaned vector or NULL.
# We use this to make JSON output stable for both single values and small vectors.
.kwallm_scalar_or_null <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }

  if (is.list(x) && !is.data.frame(x)) {
    if (
      length(x) == 1L &&
        is.list(x[[1]]) &&
        !is.data.frame(x[[1]])
    ) {
      return(x[[1]])
    }

    if (all(vapply(x, is.null, logical(1)))) {
      return(NULL)
    }

    return(x)
  }

  if (all(is.na(x))) {
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

# Converts list columns to JSON strings for Excel-safe data frames.
# We use this for sheets like stage_executions that carry structured scope data.
.kwallm_excel_df <- function(df) {
  if (!is.data.frame(df)) {
    return(df)
  }

  out <- df
  list_cols <- vapply(out, is.list, logical(1))
  if (!any(list_cols)) {
    return(out)
  }

  for (column in names(out)[list_cols]) {
    if (!nrow(out)) {
      out[[column]] <- character(0)
      next
    }

    out[[column]] <- vapply(
      out[[column]],
      function(value) {
        value <- .kwallm_scalar_or_null(value)
        if (is.null(value)) {
          return(NA_character_)
        }

        jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
      },
      character(1)
    )
  }

  out
}
