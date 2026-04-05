# Helpers shared by the main processing module.

# 1 Launch helpers -------------------------------------------------------------

#' Check whether the number of texts stays under the configured maximum
#'
#' Used in `module_core_processing` before launching async work.
#'
#' @param preprocessed_texts Character vector of preprocessed texts, or `NULL`.
#' @param lang Translator object used for the validation message.
#' @param maximum Maximum number of texts allowed.
#' @param notify_fn Function used to show the error notification.
#'
#' @return `TRUE` when processing may continue, otherwise `FALSE`.
processing_texts_under_maximum <- function(
  preprocessed_texts,
  lang,
  maximum = getOption("processing__max_texts", 3000),
  notify_fn = shiny::showNotification
) {
  n_texts <- length(preprocessed_texts %||% character(0))

  if (n_texts > maximum) {
    notify_fn(
      paste0(
        lang$t("Je mag maximaal "),
        maximum,
        lang$t(" teksten analyseren.")
      ),
      type = "error"
    )
    return(FALSE)
  }

  TRUE
}


#' Check whether GLiNER anonymization is still pending
#'
#' Used by `module_core_processing` to stop users from launching an analysis
#' while they selected GLiNER but have not yet saved the anonymized texts.
#'
#' @param texts Reactive-values-like object from `text_management_server()`.
#'
#' @return `TRUE` when GLiNER is the requested anonymization mode and the
#'   anonymization has not been completed yet, otherwise `FALSE`.
processing_has_pending_gliner_anonymization <- function(texts) {
  if (is.null(texts)) {
    return(FALSE)
  }

  identical(texts$anonymization_requested_mode %||% NULL, "gliner") &&
    !isTRUE(texts$anonymization_completed)
}


#' Check whether anonymization state allows launching processing
#'
#' Used in `module_core_processing` before dispatching an analysis run. GLiNER
#' requires an explicit save step; until then, the app should not send the raw
#' texts to the LLM.
#'
#' @param texts Reactive-values-like object from `text_management_server()`.
#' @param lang Translator object used for notification text.
#' @param notify_fn Function used to show the error notification.
#'
#' @return `TRUE` when processing may continue, otherwise `FALSE`.
processing_anonymization_ready <- function(
  texts,
  lang,
  notify_fn = shiny::showNotification
) {
  if (!processing_has_pending_gliner_anonymization(texts)) {
    return(TRUE)
  }

  notify_fn(
    lang$t("GLiNER-anonimisering nog niet voltooid..."),
    type = "error"
  )
  FALSE
}


#' Check whether text splitting is still in progress
#'
#' Used in `module_core_processing` before dispatching an analysis run. While
#' a split job is still running, `texts$preprocessed` may still reflect the
#' old unsplit snapshot, so launching analysis would use stale text state.
#'
#' @param split_in_progress Logical scalar indicating whether splitting is
#'   currently running.
#' @param lang Translator object used for notification text.
#' @param notify_fn Function used to show the error notification.
#'
#' @return `TRUE` when processing may continue, otherwise `FALSE`.
processing_split_ready <- function(
  split_in_progress,
  lang,
  notify_fn = shiny::showNotification
) {
  if (!isTRUE(split_in_progress)) {
    return(TRUE)
  }

  notify_fn(
    lang$t("Teksten worden nog gesplitst..."),
    type = "error"
  )
  FALSE
}


# 2 Report and paragraph helpers -----------------------------------------------

#' Collect texts per label from processing results
#'
#' Used before paragraph writing in `module_core_processing`.
#' It builds the grouped text lists needed to write one paragraph per
#' category/topic, regardless of whether the results use a single `result`
#' column or multiple binary columns.
#'
#' @param results Data frame returned by a processing step.
#' @param labels Character vector with labels to collect texts for.
#' @param assign_multiple_categories Logical; `TRUE` when `results` stores one
#'   logical column per label, `FALSE` when it stores a single `result` column.
#'
#' @return A named list where each element is a character vector of texts for
#'   one label. Empty groups are removed.
collect_grouped_texts <- function(results, labels, assign_multiple_categories) {
  stopifnot(is.data.frame(results), is.character(labels))

  grouped_texts <- vector("list", length(labels))
  names(grouped_texts) <- labels

  if (!isTRUE(assign_multiple_categories)) {
    # Single-label results store the chosen label in one `result` column.
    for (label in labels) {
      grouped_texts[[label]] <- results$text[results$result == label]
    }
  } else {
    # Multi-label results store one logical column per label.
    for (label in labels) {
      grouped_texts[[label]] <- results$text[results[[label]]]
    }
  }

  # Skip empty groups so later paragraph-writing only sees labels with texts.
  grouped_texts[
    purrr::map_lgl(grouped_texts, ~ isTRUE(length(.x) > 0))
  ]
}


#' Collect paragraph-writing inputs per label from processing results
#'
#' Used before paragraph writing in `module_core_processing`.
#' It preserves the `analysis_unit_id` values alongside the texts so paragraph
#' provenance can be reconstructed without text matching.
#'
#' @param results Data frame returned by a processing step.
#' @param labels Character vector with labels to collect texts for.
#' @param assign_multiple_categories Logical; `TRUE` when `results` stores one
#'   logical column per label, `FALSE` when it stores a single `result` column.
#'
#' @return A named list where each element is a list with `texts` and
#'   `analysis_unit_ids` entries for one label. Empty groups are removed.
collect_grouped_paragraph_inputs <- function(
  results,
  labels,
  assign_multiple_categories
) {
  stopifnot(
    is.data.frame(results),
    is.character(labels),
    "text" %in% names(results),
    "analysis_unit_id" %in% names(results)
  )

  grouped_inputs <- vector("list", length(labels))
  names(grouped_inputs) <- labels

  build_group_entry <- function(keep) {
    keep <- keep %in% TRUE
    list(
      texts = as.character(results$text[keep]),
      analysis_unit_ids = as.integer(results$analysis_unit_id[keep])
    )
  }

  if (!isTRUE(assign_multiple_categories)) {
    for (label in labels) {
      grouped_inputs[[label]] <- build_group_entry(results$result == label)
    }
  } else {
    for (label in labels) {
      grouped_inputs[[label]] <- build_group_entry(results[[label]])
    }
  }

  grouped_inputs[
    purrr::map_lgl(grouped_inputs, ~ isTRUE(length(.x$texts) > 0))
  ]
}


#' Write one paragraph per grouped set of texts
#'
#' Used inside async workers in `module_core_processing` for categorization and
#' topic assignment. This keeps the progress, streaming, and interruption logic
#' out of the main worker bodies.
#'
#' @param grouped_texts Named list where each element is a list with `texts`
#'   and aligned `analysis_unit_ids` entries for one category/topic.
#' @param research_background Single background string passed to
#'   `write_paragraph()`.
#' @param style_prompt Single style instruction string passed to
#'   `write_paragraph()`.
#' @param llm_provider LLM provider object used by `write_paragraph()`.
#' @param lang Translator object used for UI text and output language.
#' @param progress_secondary Optional async progress controller for the per-group
#'   progress bar.
#' @param interrupter Optional interrupter object with `$execInterrupts()`.
#' @param llm_stream_async Optional async streaming controller.
#' @param streaming_enabled Logical; whether live paragraph streaming should be
#'   shown.
#'
#' @return A named list with the paragraph objects returned by
#'   `write_paragraph()`.
write_grouped_paragraphs <- function(
  grouped_texts,
  research_background,
  style_prompt,
  llm_provider,
  lang,
  progress_secondary = NULL,
  interrupter = NULL,
  llm_stream_async = NULL,
  streaming_enabled = FALSE
) {
  stopifnot(is.list(grouped_texts), !is.null(names(grouped_texts)))

  normalize_group_entry <- function(entry) {
    if (!is.list(entry) || is.null(entry$texts)) {
      stop(
        paste(
          "grouped_texts entries must be lists with texts and analysis_unit_ids"
        )
      )
    }

    texts <- as.character(entry$texts)
    analysis_unit_ids <- entry$analysis_unit_ids %||% NULL

    if (is.null(analysis_unit_ids)) {
      stop("grouped_texts entries must contain analysis_unit_ids")
    }

    analysis_unit_ids <- as.integer(analysis_unit_ids)
    stopifnot(length(analysis_unit_ids) == length(texts))

    list(
      texts = texts,
      analysis_unit_ids = analysis_unit_ids
    )
  }

  # No groups means there is nothing to summarize.
  if (!length(grouped_texts)) {
    return(list())
  }

  if (!is.null(progress_secondary)) {
    # Reuse the secondary bar to show paragraph progress per group.
    progress_secondary$show()
    progress_secondary$set_with_total(0, length(grouped_texts), "...")
    on.exit(progress_secondary$hide(), add = TRUE)
  }

  stream_callback <- NULL
  if (isTRUE(streaming_enabled) && !is.null(llm_stream_async)) {
    # Stream partial paragraph text into the UI while the model is writing.
    llm_stream_async$show()
    stream_callback <- function(token, meta) {
      partial_response <- meta$partial_response
      if (is.null(partial_response)) {
        partial_response <- ""
      }
      llm_stream_async$set(partial_response)
      invisible(TRUE)
    }
  }

  purrr::imap(
    grouped_texts,
    function(topic_entry, topic_name) {
      normalized_entry <- normalize_group_entry(topic_entry)

      if (!is.null(interrupter)) {
        interrupter$execInterrupts()
      }

      if (!is.null(progress_secondary)) {
        progress_secondary$set_with_total(
          which(names(grouped_texts) == topic_name),
          length(grouped_texts),
          paste0(
            lang$t("Schrijven over '"),
            topic_name,
            "'..."
          )
        )
      }

      if (isTRUE(streaming_enabled) && !is.null(llm_stream_async)) {
        llm_stream_async$clear()
      }

      # Write one paragraph for one category/topic.
      write_paragraph(
        texts = normalized_entry$texts,
        analysis_unit_ids = normalized_entry$analysis_unit_ids,
        topic = topic_name,
        research_background = research_background,
        style_prompt = style_prompt,
        llm_provider = llm_provider,
        language = lang$get_translation_language(),
        stream_callback = stream_callback
      )
    }
  )
}


# 3 Result assembly helpers ----------------------------------------------------

#' Join worker results back to the original uploaded texts
#'
#' Used after async processing completes in `module_core_processing`.
#' Workers operate on deduplicated analysis units, while the UI and downloads
#' should use the current document text again.
#'
#' @param texts_df Data frame with at least `document_text`, `preprocessed`, and
#'   `analysis_unit_id` columns.
#' @param results_table_pre Data frame returned by processing.
#'   It must contain `analysis_unit_id` for the worker outputs. Multiple
#'   document rows may share one `analysis_unit_id`.
#'
#' @return A data frame with the current document text restored as `text`.
join_processing_results <- function(texts_df, results_table_pre) {
  stopifnot(is.data.frame(texts_df))
  stopifnot(is.data.frame(results_table_pre))

  stopifnot(
    "analysis_unit_id" %in% names(texts_df),
    "analysis_unit_id" %in% names(results_table_pre)
  )

  worker_results <- results_table_pre
  if ("text" %in% names(worker_results)) {
    worker_results$text <- NULL
  }

  # One analysis unit can map back to many document rows, so this join
  # intentionally fans worker output back out to the current row layer.
  results_table <- texts_df |>
    dplyr::left_join(
      worker_results,
      by = "analysis_unit_id",
      relationship = "many-to-many"
    ) |>
    dplyr::select(-preprocessed) |>
    dplyr::rename(text = document_text)

  results_table
}


#' Check whether processing results contain invalid missing values
#'
#' Used in `module_core_processing` after worker completion and again before
#' download preparation. Some modes use `NA` to signal worker failure, while
#' marking legitimately allows missing snippet columns after joining back to the
#' original uploaded texts.
#'
#' @param results_df Final result data frame shown in the app.
#' @param mode Single processing mode name.
#'
#' @return `TRUE` when the result shape indicates a failed analysis response,
#'   otherwise `FALSE`.
processing_results_have_invalid_na <- function(results_df, mode) {
  stopifnot(is.data.frame(results_df))
  stopifnot(is.character(mode), length(mode) == 1)

  if (mode == "Markeren") {
    return(FALSE)
  }

  if ("result" %in% names(results_df)) {
    return(anyNA(results_df$result))
  }

  result_cols <- setdiff(names(results_df), "text")
  length(result_cols) > 0 && anyNA(results_df[result_cols])
}


#' Count how many paragraph subjects should exist for one AnalysisResult
#'
#' Used in `module_core_processing` to distinguish valid empty paragraph output
#' (for example marking runs with no matches) from missing paragraphs when
#' there were results to summarize.
#'
#' @param analysis_result AnalysisResult to inspect.
#'
#' @return Integer scalar with the number of labels or codes that have
#'   supporting results and therefore could yield a paragraph.
analysis_result_expected_paragraph_subject_count <- function(analysis_result) {
  stopifnot(inherits(analysis_result, "AnalysisResult"))

  result <- analysis_result@results

  if (inherits(result, c("CategorizationResult", "TopicResult"))) {
    return(as.integer(length(unique(result@assignments$label_id))))
  }

  if (inherits(result, "MarkingResult")) {
    return(as.integer(length(unique(result@markings$code_id))))
  }

  0L
}


# 4 Export helpers -------------------------------------------------------------

#' Write an AnalysisResult to Excel
#'
#' Used by the download preparation flow in `module_core_processing`.
#' The helper always returns a file path; on failure it writes a `.txt` file
#' with the error so the caller can surface that message cleanly.
#'
#' @param analysis_result AnalysisResult to export.
#' @param temp_dir Directory where the output file should be written.
#'
#' @return Path to the created `.xlsx` file, or to a `.txt` error file.
write_analysis_result_excel <- function(
  analysis_result,
  temp_dir = tempdir()
) {
  excel_file <- file.path(temp_dir, "results.xlsx")
  error_file <- file.path(temp_dir, "results_error.txt")

  tryCatch(
    {
      writexl::write_xlsx(
        x = analysis_result_to_export_sheets(analysis_result),
        path = excel_file
      )
      excel_file
    },
    error = function(e) {
      writeLines(
        paste("Error during Excel creation:", conditionMessage(e)),
        con = error_file
      )
      error_file
    }
  )
}


# Bundles the helpers injected into report render environments.
# We use this in both the app render path and the report smoke tests.
analysis_result_report_globals <- function() {
  list(
    .kwallm_report_results_df = .kwallm_report_results_df,
    .kwallm_report_group_lookup = .kwallm_report_group_lookup,
    .kwallm_analysis_result_text_counts = .kwallm_analysis_result_text_counts,
    .kwallm_report_text_count_summary = .kwallm_report_text_count_summary,
    .kwallm_get_stage_model_id = .kwallm_get_stage_model_id,
    .kwallm_paragraph_subject_lookup = .kwallm_paragraph_subject_lookup,
    .kwallm_paragraph_supporting_texts = .kwallm_paragraph_supporting_texts
  )
}


# Bundles the functions the async download worker needs.
# We use the same flat `.args = c(list(...), *_globals())` shape as other async workers.
analysis_result_async_globals <- function() {
  c(
    list(
      create_analysis_result_download_bundle = create_analysis_result_download_bundle,
      write_analysis_result_metadata_json = write_analysis_result_metadata_json,
      write_analysis_result_excel = write_analysis_result_excel,
      write_analysis_result_report_html = write_analysis_result_report_html,
      analysis_result_to_metadata_list = analysis_result_to_metadata_list,
      analysis_result_to_export_sheets = analysis_result_to_export_sheets,
      analysis_result_report_globals = analysis_result_report_globals,
      .kwallm_mode_display_from_id = .kwallm_mode_display_from_id
    ),
    analysis_result_report_globals()
  )
}


#' Render an AnalysisResult to a HTML report
#'
#' Used by the download preparation flow in `module_core_processing` for modes
#' that should include a report. Like the Excel helper, it returns a path to
#' either the report or a text file with error details.
#'
#' @param analysis_result AnalysisResult to render.
#' @param temp_dir Directory where the output file should be written.
#'
#' @return Path to the created `.html` report, or to a `.txt` error file.
write_analysis_result_report_html <- function(
  analysis_result,
  temp_dir = tempdir()
) {
  output_file_html <- file.path(temp_dir, "report.html")
  output_file_txt <- file.path(temp_dir, "report_error.txt")
  report_env <- list2env(
    analysis_result_report_globals(),
    parent = parent.frame()
  )

  tryCatch(
    {
      rmarkdown::render(
        input = here::here(
          "R",
          paste0(
            "report_",
            .kwallm_mode_display_from_id(analysis_result@metadata@mode_id),
            "_",
            analysis_result@metadata@language,
            ".Rmd"
          )
        ),
        output_file = output_file_html,
        params = list(
          analysis_result = analysis_result
        ),
        envir = report_env
      )

      output_file_html
    },
    error = function(e) {
      error_details <- paste(
        "Error during rendering:",
        conditionMessage(e),
        "\n\n--- Traceback ---\n",
        paste(capture.output(traceback()), collapse = "\n"),
        "\n\n--- Full Error Object ---\n",
        paste(capture.output(print(e)), collapse = "\n")
      )

      writeLines(error_details, con = output_file_txt)
      output_file_txt
    }
  )
}


#' Create the downloadable zip bundle for one analysis run
#'
#' Used inside the async download-preparation worker in
#' `module_core_processing`. It generates the required output files, checks for
#' error files, and zips the final bundle into one archive.
#'
#' @param analysis_result AnalysisResult to bundle.
#' @param temp_dir Directory used for intermediate files and the final zip.
#'
#' @return Path to the generated `.zip` archive.
create_analysis_result_download_bundle <- function(
  analysis_result,
  temp_dir = tempdir()
) {
  bundle_dir <- file.path(temp_dir, paste0("kwallm_", uuid::UUIDgenerate()))
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  metadata_file <- write_analysis_result_metadata_json(
    analysis_result,
    temp_dir = bundle_dir
  )
  excel_file <- write_analysis_result_excel(
    analysis_result,
    temp_dir = bundle_dir
  )

  rmarkdown_file <- write_analysis_result_report_html(
    analysis_result,
    temp_dir = bundle_dir
  )
  files <- c(metadata_file, excel_file, rmarkdown_file)

  for (file in files) {
    if (!file.exists(file)) {
      stop("Output file not found, no error available")
    }
    if (grepl("\\.txt$", file)) {
      # Export helpers write `.txt` files on failure, so surface those as real
      # errors before zipping.
      label <- switch(
        basename(file),
        metadata_error.txt = "Metadata file",
        results_error.txt = "Excel file",
        report_error.txt = "Rmarkdown file",
        "Output file"
      )
      stop(paste0(
        label,
        " generation error: ",
        paste(readLines(file, warn = FALSE), collapse = "\n")
      ))
    }
  }

  zip_path <- file.path(
    temp_dir,
    paste0(uuid::UUIDgenerate(), "_results.zip")
  )

  # Zip the generated files relative to the output directory so the archive
  # contains clean filenames.
  zip::zipr(
    zipfile = zip_path,
    files = files,
    root = bundle_dir
  )

  zip_path
}


# 5 UI helpers -----------------------------------------------------------------

#' Disable inputs when processing is active
#'
#' Creates an observer that disables/enables the specified input IDs
#' based on the processing state. Uses shinyjs::toggleState internally.
#'
#' @param processing Reactive value indicating processing state (TRUE = processing)
#' @param input_ids Character vector of input IDs to toggle
#'
#' @return An observer (invisible)
#' @examples
#' disable_when_processing(processing, c("toggle", "submit_btn", "text_input"))
disable_when_processing <- function(processing, input_ids) {
  observe({
    for (id in input_ids) {
      shinyjs::toggleState(id, condition = !processing())
    }
  })
}
