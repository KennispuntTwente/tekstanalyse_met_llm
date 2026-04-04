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


#' Write one paragraph per grouped set of texts
#'
#' Used inside async workers in `module_core_processing` for categorization and
#' topic assignment. This keeps the progress, streaming, and interruption logic
#' out of the main worker bodies.
#'
#' @param grouped_texts Named list where each element is a character vector of
#'   texts for one category/topic.
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
    function(topic_texts, topic_name) {
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
        texts = topic_texts,
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
#' Workers operate on preprocessed texts, while the UI and downloads should use
#' the original raw texts again.
#'
#' @param texts_df Data frame with at least `raw` and `preprocessed` columns.
#' @param results_table_pre Data frame returned by processing.
#'   Its `text` column must match `texts_df$preprocessed`.
#'   `texts_df$preprocessed`.
#'
#' @return A data frame with the raw text restored as `text`.
join_processing_results <- function(texts_df, results_table_pre) {
  stopifnot(is.data.frame(texts_df))
  stopifnot(is.data.frame(results_table_pre))

  paragraphs <- attr(results_table_pre, "paragraphs", exact = TRUE)

  # Join by the preprocessed text the worker actually saw, then restore the raw
  # uploaded text as the main `text` column.
  results_table <- texts_df |>
    dplyr::left_join(
      results_table_pre,
      by = dplyr::join_by("preprocessed" == "text")
    ) |>
    dplyr::select(-preprocessed) |>
    dplyr::rename(text = raw)

  attr(results_table, "paragraphs") <- paragraphs
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
          report_context = analysis_result_to_report_context(analysis_result)
        ),
        envir = new.env()
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
      label <- if (grepl("^data_", basename(file))) {
        "Excel file"
      } else {
        "Rmarkdown file"
      }
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
