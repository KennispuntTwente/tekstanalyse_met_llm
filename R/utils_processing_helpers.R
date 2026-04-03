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

#' Check whether a mode should generate a HTML report
#'
#' Used in `module_core_processing` when preparing download files.
#' This keeps the "which modes get a report?" rule in one place.
#'
#' @param mode Single mode name such as `"Categorisatie"` or `"Markeren"`.
#'
#' @return `TRUE` when the mode should get a report file, otherwise `FALSE`.
processing_mode_supports_report <- function(mode) {
  mode %in% c("Categorisatie", "Scoren", "Onderwerpextractie", "Markeren")
}

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
#' @param worker_results_df Data frame returned by the worker, with a `text`
#'   column that matches `preprocessed`.
#'
#' @return A data frame with the raw text restored as `text`, plus any
#'   attributes such as `paragraphs` copied over from the worker result.
join_processing_results <- function(texts_df, worker_results_df) {
  stopifnot(is.data.frame(texts_df), is.data.frame(worker_results_df))

  # Join by the preprocessed text the worker actually saw, then restore the raw
  # uploaded text as the main `text` column.
  final_df <- texts_df |>
    dplyr::left_join(
      worker_results_df,
      by = dplyr::join_by("preprocessed" == "text")
    ) |>
    dplyr::select(-preprocessed) |>
    dplyr::rename(text = raw)

  attr(final_df, "paragraphs") <- attr(worker_results_df, "paragraphs")
  final_df
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
  stopifnot(is.data.frame(results_df), is.character(mode), length(mode) == 1)

  if (mode == "Markeren") {
    return(FALSE)
  }

  if ("result" %in% names(results_df)) {
    return(anyNA(results_df$result))
  }

  result_cols <- setdiff(names(results_df), "text")
  length(result_cols) > 0 && anyNA(results_df[result_cols])
}


#' Build the result bundle used for downloads and reports
#'
#' Used in `module_core_processing` after processing and optional
#' inter-rater-reliability have finished. This gathers the result data and the
#' mode-specific metadata in one structured list.
#'
#' @param final_results_df Final result data frame shown to the user.
#' @param uuid Unique analysis id used in filenames and logs.
#' @param mode Single processing mode name.
#' @param research_background Single background string used for the run.
#' @param style_prompt Single style prompt string used for the run.
#' @param irr_result Optional inter-rater-reliability result object.
#' @param language Output language code for reports.
#' @param by_column_name Optional column name used for grouped analyses.
#' @param by_column_values Optional vector of selected values from that column.
#' @param models List with configured model objects.
#' @param categories Optional character vector of categories.
#' @param exclusive_categories Optional character vector of exclusive
#'   categories.
#' @param scoring_characteristic Optional single scoring characteristic string.
#' @param topics Optional character vector of topics.
#' @param exclusive_topics Optional character vector of exclusive topics.
#' @param codes Optional character vector of marking codes.
#' @param assign_multiple_categories Logical; whether multiple labels per text
#'   were allowed.
#' @param human_in_the_loop Logical; whether topic editing was enabled.
#' @param write_paragraphs Logical; whether paragraphs were requested.
#' @param context_window List with context-window and chunking settings.
#' @param prompt_text Optional prompt example stored in the result bundle.
#'
#' @return A named list containing the final data plus metadata used by the
#'   Excel export and R Markdown report.
build_processing_result_list <- function(
  final_results_df,
  uuid,
  mode,
  research_background,
  style_prompt,
  irr_result = NULL,
  language,
  by_column_name = NULL,
  by_column_values = NULL,
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
  prompt_text = NULL
) {
  # Base fields are shared across all modes.
  result_list <- list(
    df = final_results_df,
    time = Sys.time(),
    uuid = uuid,
    mode = mode,
    research_background = research_background,
    style_prompt = style_prompt,
    irr = irr_result,
    language = language,
    by_column_name = by_column_name,
    by_column_values = by_column_values
  )

  if (mode == "Categorisatie") {
    # Deductive categorization stores the configured categories and prompt.
    result_list$model <- models$main$parameters$model
    result_list$categories <- categories
    result_list$exclusive_categories <- exclusive_categories
    result_list$assign_multiple_categories <- assign_multiple_categories
    result_list$prompt <- prompt_text
    result_list$human_in_the_loop <- human_in_the_loop
    result_list$write_paragraphs <- write_paragraphs
  }

  if (mode == "Scoren") {
    # Scoring needs the main model and the characteristic definition.
    result_list$model <- models$main$parameters$model
    result_list$scoring_characteristic <- scoring_characteristic
    result_list$prompt <- prompt_text
  }

  if (mode == "Onderwerpextractie") {
    # Topic extraction records both models plus chunking settings used upstream.
    result_list$model <- models$main$parameters$model
    result_list$model_reductie <- models$large$parameters$model
    result_list$topics <- topics
    result_list$exclusive_topics <- exclusive_topics
    result_list$assign_multiple_categories <- assign_multiple_categories
    result_list$write_paragraphs <- write_paragraphs
    result_list$chunking_parameters <- tibble::tibble(
      parameter = c(
        "chunk_size",
        "draws",
        "n_tokens_context_window",
        "n_chunks"
      ),
      value = c(
        context_window$chunk_size,
        context_window$draws,
        context_window$n_tokens_context_window,
        context_window$n_chunks
      )
    )
  }

  if (mode == "Markeren") {
    # Marking stores code definitions and chunk overlap settings.
    result_list$model <- models$main$parameters$model
    result_list$codes <- codes
    result_list$write_paragraphs <- write_paragraphs
    result_list$prompt <- prompt_text
    result_list$text_size_tokens <- context_window$max_tokens
    result_list$overlap_size_tokens <- context_window$overlap
  }

  paragraphs <- attr(final_results_df, "paragraphs")
  if (!is.null(paragraphs)) {
    # Paragraphs are stored as an attribute on the results data frame.
    result_list$paragraphs <- paragraphs
  }

  result_list
}


# 4 Export helpers -------------------------------------------------------------

#' Write the result bundle to Excel
#'
#' Used by the download preparation flow in `module_core_processing`.
#' The helper always returns a file path; on failure it writes a `.txt` file
#' with the error so the caller can surface that message cleanly.
#'
#' @param result_list Result bundle created by `build_processing_result_list()`.
#' @param temp_dir Directory where the output file should be written.
#'
#' @return Path to the created `.xlsx` file, or to a `.txt` error file.
write_processing_result_excel <- function(result_list, temp_dir = tempdir()) {
  excel_file <- file.path(
    temp_dir,
    paste0("data_", result_list$uuid, ".xlsx")
  )

  error_file <- file.path(
    temp_dir,
    paste0("data_", result_list$uuid, "_error.txt")
  )

  # Convert each element in the result bundle to something writexl can store as
  # one worksheet.
  safe_write_xlsx <- function(result_list, excel_file) {
    sheets <- lapply(result_list, function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      if (length(x) == 1 && is.atomic(x) && is.na(x)) {
        return(data.frame(value = NA, stringsAsFactors = FALSE))
      }
      if (is.data.frame(x)) {
        return(x)
      }
      if (is.atomic(x) || is.character(x)) {
        return(data.frame(value = x, stringsAsFactors = FALSE))
      }
      if (is.list(x)) {
        df <- tryCatch(as.data.frame(x), error = function(e) NULL)
        if (!is.null(df)) {
          return(df)
        }

        # Fall back to captured printed output for list objects that do not map
        # cleanly to a data frame.
        captured <- capture.output(print(x))
        return(data.frame(
          captured_output = captured,
          stringsAsFactors = FALSE
        ))
      }

      captured <- capture.output(print(x))
      data.frame(
        captured_output = captured,
        stringsAsFactors = FALSE
      )
    })

    names(sheets) <- names(result_list)
    sheets <- Filter(Negate(is.null), sheets)

    writexl::write_xlsx(x = sheets, path = excel_file)
  }

  tryCatch(
    {
      safe_write_xlsx(result_list, excel_file)
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


#' Render the result bundle to a HTML report
#'
#' Used by the download preparation flow in `module_core_processing` for modes
#' that should include a report. Like the Excel helper, it returns a path to
#' either the report or a text file with error details.
#'
#' @param result_list Result bundle created by `build_processing_result_list()`.
#' @param temp_dir Directory where the output file should be written.
#'
#' @return Path to the created `.html` report, or to a `.txt` error file.
write_processing_result_rmarkdown <- function(
  result_list,
  temp_dir = tempdir()
) {
  output_file_html <- file.path(
    temp_dir,
    paste0("report_", result_list$uuid, ".html")
  )

  output_file_txt <- file.path(
    temp_dir,
    paste0("report_", result_list$uuid, "_error.txt")
  )

  tryCatch(
    {
      rmarkdown::render(
        input = paste0(
          "R/report_",
          result_list$mode,
          "_",
          result_list$language,
          ".Rmd"
        ),
        output_file = output_file_html,
        params = list(result_list = result_list),
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
#' @param result_list Result bundle created by `build_processing_result_list()`.
#' @param temp_dir Directory used for intermediate files and the final zip.
#'
#' @return Path to the generated `.zip` archive.
create_processing_download_bundle <- function(
  result_list,
  temp_dir = tempdir()
) {
  excel_file <- write_processing_result_excel(result_list, temp_dir = temp_dir)

  files <- c(excel_file)
  if (processing_mode_supports_report(result_list$mode)) {
    rmarkdown_file <- write_processing_result_rmarkdown(
      result_list,
      temp_dir = temp_dir
    )
    files <- c(files, rmarkdown_file)
  }

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
    root = dirname(excel_file)
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
