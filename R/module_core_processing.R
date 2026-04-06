# Module responsible for launching analysis runs, showing progress,
# and preparing downloadable results

# 1 UI -------------------------------------------------------------------------

# Shows the processing controls and status areas.
# This is the visible UI for starting work, tracking progress, and downloading.

processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    progress_bar_ui("progress_primary", visible = TRUE),
    progress_bar_ui("progress_secondary", visible = FALSE),
    llm_streaming_ui("llm_stream", visible = FALSE),
    br(),
    div(
      class = "text-center",
      uiOutput(ns("process_button")),
    ),
    div(
      class = "text-center",
      style = "margin-top: 10px;",
      uiOutput(ns("cancel_button"))
    ),
    uiOutput(ns("download_ui"))
  )
}


# 2 Server ---------------------------------------------------------------------

# Runs the full processing flow behind the UI.
# This section manages async work, results, downloads, and user actions.

processing_server <- function(
  id,
  mode,
  interrater_reliability_toggle,
  texts,
  llm_provider_rv,
  models,
  categories,
  scoring_characteristic,
  codes,
  research_background,
  style_prompt,
  human_in_the_loop = reactiveVal(TRUE),
  assign_multiple_categories = reactiveVal(TRUE),
  write_paragraphs = reactiveVal(TRUE),
  context_window,
  by_column_name = reactiveVal(NULL),
  by_column_lookup = reactiveVal(NULL),
  split_settings = reactiveVal(list()),
  upload_info = reactiveVal(list()),
  split_in_progress = reactiveVal(FALSE),
  lang = default_lang()
) {
  ns <- NS(id)

  moduleServer(
    id,
    function(input, output, session) {
      ## 2.1 State management --------------------------------------------------

      # Keeps the reactive state for one analysis run.
      # These values track progress, results, download state, and completion.

      ### 2.1.1 Run state ------------------------------------------------------

      # Basic overview of the process:
      #   > Wait for the process button click
      #   > Dispatch to the active mode's async flow
      #   > Store the raw processing result in `results_table_pre()`
      #   > Join it back to the original texts in `results_table()`
      #   > Optionally run interrater reliability
      #   > Build the export bundle and expose the download UI

      # Key reactive state for one analysis run:
      #   processing: TRUE while an analysis is actively running
      #   started: TRUE once a run has been launched, even after processing ends
      #   results_table_pre: raw processing output based on preprocessed texts
      #   paragraph_entries_generated: paragraph output kept alongside results
      #   results_table: joined row-level results shown in the app and tests
      #   irr_result: stored interrater reliability summary, if collected
      #   irr_sample: sampled rows rated during interrater reliability
      #   stage_execution_rows_generated: recorded LLM-call provenance rows
      #   preparing_download: TRUE while export files are being created
      #   zip_file: path to the prepared download bundle
      #   topics: current topic list used by topic extraction
      #   exclusive_topics: topics that must remain exclusive
      #   topics_definitive: TRUE once the topic list is finalized
      #   candidate_topics_generated: raw generated topics before reduction
      #   reduced_topics_generated: reduced topics before any manual edits
      #   topics_were_edited: TRUE once the reduced topics were changed by hand
      #   success: TRUE once the full flow reaches download-ready state
      #   analysis_started_at: click timestamp used for end-to-end timing
      processing <- reactiveVal(FALSE)
      started <- reactiveVal(FALSE)
      results_table_pre <- reactiveVal(NULL)
      paragraph_entries_generated <- reactiveVal(NULL)
      results_table <- reactiveVal(NULL)
      irr_result <- reactiveVal(NULL)
      irr_sample <- reactiveVal(NULL)
      stage_execution_rows_generated <- reactiveVal(NULL)
      preparing_download <- reactiveVal(NULL)
      zip_file <- reactiveVal(NULL)
      topics <- reactiveVal(NULL)
      exclusive_topics <- reactiveVal(NULL)
      topics_definitive <- reactiveVal(FALSE)
      candidate_topics_generated <- reactiveVal(NULL)
      reduced_topics_generated <- reactiveVal(NULL)
      topics_were_edited <- reactiveVal(FALSE)
      topic_editor_was_used <- reactiveVal(FALSE)
      success <- reactiveVal(NULL)

      # Stable identifier for the current processing task and export bundle
      uuid <- uuid::UUIDgenerate()

      # Timestamp for end-to-end duration (click -> download-ready)
      analysis_started_at <- reactiveVal(NULL)
      pending_prompt_scope_decision <- reactiveVal(NULL)
      prompt_scope_skip_counts <- reactiveVal(list())

      current_analysis_unit_ids <- function() {
        # These ids line up with texts$preprocessed: one id per unique analysis
        # unit, not one id per document row in texts$df.
        ids <- texts$analysis_units$analysis_unit_id %||% NULL
        if (is.null(ids)) {
          stop("texts$analysis_units$analysis_unit_id must be available")
        }

        as.integer(ids)
      }

      normalize_topic_labels <- function(topic_values) {
        if (is.null(topic_values)) {
          return(character())
        }

        normalized <- as.character(topic_values)
        normalized <- normalized[!is.na(normalized)]
        normalized <- trimws(normalized)

        unique(normalized[nzchar(normalized)])
      }

      topic_assignment_fit_info <- function(
        current_topics,
        current_exclusive_topics = exclusive_topics() %||% character()
      ) {
        topic_assignment_prompt_context_window_check(
          texts = texts$preprocessed,
          topics = current_topics,
          research_background = research_background(),
          llm_provider = models$main,
          assign_multiple_categories = assign_multiple_categories(),
          exclusive_topics = current_exclusive_topics
        )
      }

      topic_assignment_overflow_notice <- function(fit_info) {
        sprintf(
          lang()$t(
            "De gegenereerde onderwerpen passen niet binnen het context-window van het toekenningsmodel (%d > %d tokens). Daarom is het bewerkscherm voor onderwerpen geopend. Verminder het aantal of de lengte van de onderwerpen voordat je doorgaat."
          ),
          fit_info$prompt_tokens,
          fit_info$context_window_tokens
        )
      }

      prompt_scope_failure_settings <- function(scope_kind = "analysis_unit") {
        scope_kind <- as.character(scope_kind %||% "analysis_unit")[[1]]
        scope_option_prefix <- paste0(
          "prompt_scope_failure__",
          gsub("[^A-Za-z0-9]+", "_", scope_kind),
          "__"
        )

        get_scope_option <- function(name, default = NULL, legacy_name = NULL) {
          value <- getOption(paste0(scope_option_prefix, name), NULL)
          if (
            is.null(value) &&
              identical(scope_kind, "analysis_unit") &&
              !is.null(legacy_name)
          ) {
            value <- getOption(legacy_name, NULL)
          }
          if (is.null(value)) {
            value <- getOption(
              paste0("prompt_scope_failure__default_", name),
              default
            )
          }

          value
        }

        action <- get_scope_option(
          name = "action",
          default = "fail",
          legacy_name = "analysis_unit_failure__action"
        )
        action <- as.character(action %||% "fail")[[1]]
        if (!action %in% c("ask", "skip", "fail")) {
          action <- "fail"
        }

        max_auto_skips <- get_scope_option(
          name = "max_auto_skips",
          default = NULL,
          legacy_name = "analysis_unit_failure__max_auto_skips"
        )
        if (
          is.null(max_auto_skips) ||
            !length(max_auto_skips) ||
            is.na(max_auto_skips[[1]])
        ) {
          max_auto_skips <- NULL
        } else {
          max_auto_skips <- max(0L, as.integer(max_auto_skips[[1]]))
        }

        on_max_auto_skips <- get_scope_option(
          name = "on_max_auto_skips",
          default = "fail",
          legacy_name = "analysis_unit_failure__on_max_auto_skips"
        )
        on_max_auto_skips <- as.character(on_max_auto_skips %||% "fail")[[1]]
        if (!on_max_auto_skips %in% c("ask", "fail")) {
          on_max_auto_skips <- "fail"
        }

        list(
          scope_kind = scope_kind,
          action = action,
          max_auto_skips = max_auto_skips,
          on_max_auto_skips = on_max_auto_skips
        )
      }

      get_prompt_scope_skip_count <- function(scope_kind) {
        counts <- prompt_scope_skip_counts()
        if (!is.list(counts)) {
          return(0L)
        }

        value <- counts[[scope_kind]] %||% 0L
        value <- as.integer(value)[1]
        if (is.na(value) || value < 0L) {
          return(0L)
        }

        value
      }

      increment_prompt_scope_skip_count <- function(scope_kind) {
        counts <- prompt_scope_skip_counts()
        if (!is.list(counts)) {
          counts <- list()
        }

        counts[[scope_kind]] <- get_prompt_scope_skip_count(scope_kind) + 1L
        prompt_scope_skip_counts(counts)
        invisible(counts[[scope_kind]])
      }

      prompt_scope_failure_preview <- function(text, max_chars = 280L) {
        text <- as.character(text %||% "")[[1]]
        text <- trimws(text)

        if (!nzchar(text)) {
          return(lang()$t("[geen tekst beschikbaar]"))
        }

        if (nchar(text) <= max_chars) {
          return(text)
        }

        paste0(substr(text, 1, max_chars), "...")
      }

      prompt_scope_subject_label <- function(subject_kind) {
        label <- switch(
          as.character(subject_kind %||% "")[[1]],
          category = lang()$t("Categorie"),
          topic = lang()$t("Onderwerp"),
          code = lang()$t("Code"),
          as.character(subject_kind %||% lang()$t("Prompt"))[[1]]
        )

        tolower(label)
      }

      prompt_scope_failure_label <- function(state) {
        scope_kind <- as.character(state$scope_kind %||% "analysis_unit")[[1]]

        if (identical(scope_kind, "analysis_unit")) {
          total_units <- state$total_scopes %||%
            length(texts$preprocessed %||% character(0))
          return(sprintf(
            lang()$t("analyse-eenheid %d van %d"),
            state$failed_index %||% NA_integer_,
            total_units
          ))
        }

        if (identical(scope_kind, "analysis_unit_group")) {
          return(sprintf(
            "%s '%s'",
            prompt_scope_subject_label(state$subject_kind),
            as.character(state$subject_value %||% "?")[[1]]
          ))
        }

        if (identical(scope_kind, "chunk_code")) {
          return(sprintf(
            lang()$t("chunk %d voor code '%s'"),
            as.integer(
              state$failed_chunk_index %||% state$failed_index %||% NA_integer_
            ),
            as.character(state$subject_value %||% "?")[[1]]
          ))
        }

        as.character(scope_kind %||% lang()$t("Prompt"))[[1]]
      }

      show_prompt_scope_failure_modal <- function(state) {
        settings <- prompt_scope_failure_settings(state$scope_kind)
        limit_notice <- NULL
        if (
          identical(settings$action, "skip") &&
            !is.null(settings$max_auto_skips) &&
            isTRUE(state$skip_count >= settings$max_auto_skips)
        ) {
          limit_notice <- if (identical(settings$on_max_auto_skips, "ask")) {
            lang()$t(
              "De limiet voor automatisch overslaan is bereikt. Kies hieronder of je deze prompt opnieuw wilt proberen of alsnog handmatig wilt overslaan."
            )
          } else {
            lang()$t(
              "De limiet voor automatisch overslaan is bereikt. Nieuwe fouten stoppen de analyse volledig."
            )
          }
        }

        showModal(modalDialog(
          title = lang()$t("Prompt mislukt"),
          easyClose = FALSE,
          tags$div(
            style = "display:none;",
            `data-kwallm-modal-id` = "prompt_scope_failure_modal",
            `data-kwallm-modal-details` = sprintf(
              "mode=%s scope_kind=%s failed_index=%d analysis_unit_id=%d chunk_id=%d",
              state$mode_label %||% "unknown",
              state$scope_kind %||% "unknown",
              state$failed_index %||% NA_integer_,
              state$failed_analysis_unit_id %||% NA_integer_,
              state$failed_chunk_id %||% NA_integer_
            )
          ),
          tags$p(sprintf(
            lang()$t(
              "De prompt voor %s kon niet worden verwerkt nadat de standaard retries zijn uitgeput."
            ),
            prompt_scope_failure_label(state)
          )),
          if (!is.null(limit_notice)) {
            div(class = "alert alert-warning", limit_notice)
          },
          tags$p(lang()$t("Foutmelding:")),
          tags$pre(
            style = "white-space: pre-wrap; word-break: break-word;",
            state$failure_message %||% lang()$t("Onbekende fout.")
          ),
          tags$p(lang()$t("Tekstvoorbeeld:")),
          tags$div(
            class = "border rounded p-2 bg-light",
            style = "white-space: pre-wrap; word-break: break-word; max-height: 220px; overflow-y: auto;",
            prompt_scope_failure_preview(state$failed_text)
          ),
          footer = modal_footer_buttons(
            left = actionButton(
              ns("skip_failed_prompt_scope"),
              lang()$t("Sla prompt over"),
              class = "btn btn-secondary"
            ),
            right = actionButton(
              ns("retry_failed_prompt_scope"),
              lang()$t("Probeer opnieuw"),
              class = "btn btn-primary"
            )
          )
        ))
      }

      handle_prompt_scope_failure <- function(
        decision,
        mode_label,
        on_retry,
        on_skip
      ) {
        scope_kind <- as.character(decision$scope_kind %||% "analysis_unit")[[
          1
        ]]
        settings <- prompt_scope_failure_settings(scope_kind)
        skip_count <- get_prompt_scope_skip_count(scope_kind)
        next_action <- prompt_scope_failure_next_action(
          action = settings$action,
          skip_count = skip_count,
          max_auto_skips = settings$max_auto_skips,
          on_max_auto_skips = settings$on_max_auto_skips
        )

        if (identical(next_action, "fail")) {
          app_error(
            paste0(
              "Maximum number of automatic prompt-scope skips reached for ",
              scope_kind,
              " (",
              settings$max_auto_skips,
              ")"
            ),
            when = "prompt-scope failure handling",
            fatal = TRUE,
            lang = lang()
          )
          return(invisible(NULL))
        }

        if (identical(next_action, "skip")) {
          skip_total <- increment_prompt_scope_skip_count(scope_kind)
          log_action(
            "prompt_scope_auto_skipped",
            details = sprintf(
              paste0(
                "mode=%s scope_kind=%s failed_index=%d analysis_unit_id=%d ",
                "chunk_id=%d subject_value=%s skip_count=%d"
              ),
              mode_label %||% "unknown",
              scope_kind,
              decision$failed_index %||% NA_integer_,
              decision$failed_analysis_unit_id %||% NA_integer_,
              decision$failed_chunk_id %||% NA_integer_,
              decision$subject_value %||% NA_character_,
              skip_total
            )
          )
          on_skip("skipped_automatically")
          return(invisible(NULL))
        }

        pending_prompt_scope_decision(list(
          mode_label = mode_label,
          scope_kind = scope_kind,
          failed_index = as.integer(decision$failed_index),
          total_scopes = as.integer(
            decision$total_scopes %||%
              length(texts$preprocessed %||% character(0))
          ),
          failed_analysis_unit_id = as.integer(
            decision$failed_analysis_unit_id
          ),
          failed_chunk_id = as.integer(
            decision$failed_chunk_id %||% NA_integer_
          ),
          failed_chunk_index = as.integer(
            decision$failed_chunk_index %||% NA_integer_
          ),
          subject_kind = as.character(decision$subject_kind %||% "")[[1]],
          subject_value = as.character(decision$subject_value %||% "")[[1]],
          failed_text = as.character(decision$failed_text %||% ""),
          failure_message = as.character(decision$failure_message %||% ""),
          skip_count = as.integer(skip_count),
          resume = function(action) {
            if (identical(action, "retry")) {
              log_action(
                "prompt_scope_retry_requested",
                details = sprintf(
                  paste0(
                    "mode=%s scope_kind=%s failed_index=%d analysis_unit_id=%d ",
                    "chunk_id=%d subject_value=%s"
                  ),
                  mode_label %||% "unknown",
                  scope_kind,
                  decision$failed_index %||% NA_integer_,
                  decision$failed_analysis_unit_id %||% NA_integer_,
                  decision$failed_chunk_id %||% NA_integer_,
                  decision$subject_value %||% NA_character_
                )
              )
              on_retry()
              return(invisible(NULL))
            }

            skip_total <- increment_prompt_scope_skip_count(scope_kind)
            log_action(
              "prompt_scope_skip_requested",
              details = sprintf(
                paste0(
                  "mode=%s scope_kind=%s failed_index=%d analysis_unit_id=%d ",
                  "chunk_id=%d subject_value=%s skip_count=%d"
                ),
                mode_label %||% "unknown",
                scope_kind,
                decision$failed_index %||% NA_integer_,
                decision$failed_analysis_unit_id %||% NA_integer_,
                decision$failed_chunk_id %||% NA_integer_,
                decision$subject_value %||% NA_character_,
                skip_total
              )
            )
            on_skip("skipped_after_user_confirmation")
            invisible(NULL)
          }
        ))

        show_prompt_scope_failure_modal(pending_prompt_scope_decision())
        invisible(NULL)
      }

      resolve_prompt_scope_decision <- function(action) {
        state <- pending_prompt_scope_decision()
        if (is.null(state) || !is.function(state$resume)) {
          return(invisible(NULL))
        }

        pending_prompt_scope_decision(NULL)
        removeModal()
        state$resume(action)
        invisible(NULL)
      }

      observeEvent(
        input$retry_failed_prompt_scope,
        {
          resolve_prompt_scope_decision("retry")
        },
        ignoreInit = TRUE
      )

      observeEvent(
        input$skip_failed_prompt_scope,
        {
          resolve_prompt_scope_decision("skip")
        },
        ignoreInit = TRUE
      )

      ### 2.1.2 Test exports ---------------------------------------------------

      # Export a small state snapshot so tests can wait for milestones.
      shiny::exportTestValues(
        processing = processing(),
        started = started(),
        success = success(),
        paragraph_entries = paragraph_entries_generated(),
        results_table = results_table(),
        prompt_scope_decision_pending = !is.null(pending_prompt_scope_decision()),
        analysis_unit_decision_pending = !is.null(pending_prompt_scope_decision())
      )

      ## 2.2 Launch setup ------------------------------------------------------

      # Prepares the shared launch rules used before any mode starts.
      # This keeps common startup checks and helper wiring in one place.

      # These helpers are used by all modes to keep process startup,
      # promise wiring, and failure handling consistent.

      # Performs the shared setup before an async analysis starts.
      # Used by the mode-specific start helpers so state, timing, progress, and
      # async log context are initialized in one place.
      start_processing_run <- function(set_initial_progress = TRUE) {
        n_preprocessed_texts <- length(texts$preprocessed %||% character(0))

        started(TRUE)
        processing(TRUE)
        results_table_pre(NULL)
        paragraph_entries_generated(NULL)
        results_table(NULL)
        candidate_topics_generated(NULL)
        reduced_topics_generated(NULL)
        topics_were_edited(FALSE)
        topic_editor_was_used(FALSE)
        irr_result(NULL)
        irr_sample(NULL)
        stage_execution_rows_generated(NULL)
        pending_prompt_scope_decision(NULL)
        prompt_scope_skip_counts(list())
        analysis_started_at(Sys.time())
        log_analysis_start(
          mode = mode(),
          n_texts = n_preprocessed_texts,
          model = models$main$parameters$model %||% "unknown"
        )

        if (isTRUE(set_initial_progress)) {
          progress_primary$set_with_total(0, n_preprocessed_texts, "...")
        }

        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        log_context_capture(
          is_async = TRUE,
          mode = getOption("app__mode", "unknown")
        )
      }

      # Connects a `mirai` promise to a reactive setter plus shared error
      # handling. Used by the start helpers to keep promise wiring short.
      bind_async_result <- function(
        promise,
        setter,
        when,
        debug_message = NULL,
        stop_stream = TRUE,
        hide_stream = TRUE
      ) {
        promise %...>%
          setter %...!%
          {
            if (isTRUE(stop_stream)) {
              llm_stream$async$stop()
            }
            if (isTRUE(hide_stream)) {
              llm_stream$hide()
            }

            err <- .
            err_msg <- tryCatch(conditionMessage(err), error = function(e) {
              as.character(err)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(err), collapse = "|")
            log_action(
              "analysis_failed",
              details = sprintf(
                "mode=%s when=%s error_class=%s error=%s",
                mode() %||% "unknown",
                when,
                err_class,
                err_msg
              )
            )
            app_error(
              err,
              when = when,
              fatal = TRUE,
              lang = lang()
            )
          }

        if (!is.null(debug_message)) {
          log_debug(debug_message, component = "analysis")
        }

        invisible(NULL)
      }

      # Appends new stage execution rows for the current run.
      # Topic modelling uses multiple workers, so execution provenance is accumulated.
      append_stage_execution_rows <- function(rows) {
        if (is.null(rows) || !is.data.frame(rows) || !nrow(rows)) {
          return(invisible(NULL))
        }

        current_rows <- stage_execution_rows_generated()
        if (
          is.null(current_rows) ||
            !is.data.frame(current_rows) ||
            !nrow(current_rows)
        ) {
          stage_execution_rows_generated(rows)
        } else {
          combined_rows <- rbind(current_rows, rows)
          stage_execution_rows_generated(
            combined_rows[!duplicated(combined_rows$prompt_id), , drop = FALSE]
          )
        }

        invisible(NULL)
      }

      ## 2.3 Categorisatie -----------------------------------------------------

      # Handles the categorization mode.
      # Validation and async launch logic stay local to this mode.

      ### 2.3.1 Worker launch --------------------------------------------------

      # Runs categorization for all texts and optionally writes category paragraphs.
      start_categorization <- function(
        existing_results = NULL,
        paragraph_entries = NULL,
        start_index = 1L,
        resume_stage = c("analysis", "paragraph_generation"),
        is_resume = FALSE
      ) {
        resume_stage <- match.arg(resume_stage)
        req(texts$preprocessed)
        if (!isTRUE(is_resume)) {
          if (
            !processing_texts_under_maximum(
              preprocessed_texts = texts$preprocessed,
              lang = lang()
            )
          ) {
            return()
          }
          if (categories$editing()) {
            shiny::showNotification(
              lang()$t(
                "Je moet eerst de categorieen opslaan voordat je verder kunt gaan."
              ),
              type = "error"
            )
            return()
          }
          if (categories$unique_non_empty_count() < 2) {
            shiny::showNotification(
              lang()$t("Je moet minimaal 2 categorieen opgeven."),
              type = "error"
            )
            return()
          }
          req(isFALSE(context_window$any_fit_problem))
        }

        log_context <- if (isTRUE(is_resume)) {
          log_context_capture(
            is_async = TRUE,
            mode = getOption("app__mode", "unknown")
          )
        } else {
          start_processing_run()
        }

        promise <- mirai::mirai(
          {
            log_context_apply(log_context)
            prepare_async_analysis_worker("categorization")
            .kwallm__prompt_execution_reset()

            on_progress <- function(i, n, text) {
              progress_primary$set_with_total(i, n, text)
              if (i == 1 || i %% 5 == 0 || i == n) {
                log_info(
                  sprintf("Categorization progress: %d/%d", i, n),
                  component = "analysis"
                )
              }
            }

            results <- existing_results
            paragraphs <- paragraph_entries

            if (identical(resume_stage, "analysis")) {
              categorization_output <- categorize_texts(
                texts = texts,
                analysis_unit_ids = analysis_unit_ids,
                categories = categories,
                research_background = research_background,
                llm_provider = llm_provider,
                assign_multiple_categories = assign_multiple_categories,
                exclusive_categories = exclusive_categories,
                on_progress = on_progress,
                interrupter = interrupter,
                start_index = start_index,
                existing_results = existing_results,
                failure_action = "return_decision"
              )

              if (
                identical(
                  categorization_output$status %||% NULL,
                  "decision_required"
                )
              ) {
                categorization_output$stage_execution_rows <-
                  .kwallm__prompt_execution_get()
                categorization_output$resume_stage <- "analysis"
                return(categorization_output)
              }

              results <- categorization_output$results %||%
                categorization_output
            }

            if (write_paragraphs) {
              paragraph_output <- tryCatch(
                {
                  categories_texts <- collect_grouped_paragraph_inputs(
                    results = results,
                    labels = categories,
                    assign_multiple_categories = assign_multiple_categories
                  )

                  write_grouped_paragraphs(
                    grouped_texts = categories_texts,
                    research_background = research_background,
                    style_prompt = style_prompt,
                    llm_provider = llm_provider,
                    lang = lang,
                    subject_kind = "category",
                    progress_secondary = progress_secondary,
                    interrupter = interrupter,
                    llm_stream_async = llm_stream_async,
                    streaming_enabled = streaming_enabled,
                    existing_paragraphs = if (
                      identical(resume_stage, "paragraph_generation")
                    ) {
                      paragraph_entries
                    } else {
                      NULL
                    },
                    start_index = if (
                      identical(resume_stage, "paragraph_generation")
                    ) {
                      start_index
                    } else {
                      1L
                    },
                    failure_action = "return_decision"
                  )
                },
                error = handle_detailed_error("Category paragraph writing")
              )

              if (
                identical(
                  paragraph_output$status %||% NULL,
                  "decision_required"
                )
              ) {
                paragraph_output$results <- results
                paragraph_output$resume_stage <- "paragraph_generation"
                paragraph_output$stage_execution_rows <-
                  .kwallm__prompt_execution_get()
                return(paragraph_output)
              }

              paragraphs <- paragraph_output$paragraphs %||% paragraph_output
            } else {
              paragraphs <- NULL
            }

            list(
              results = results,
              paragraphs = paragraphs,
              stage_execution_rows = .kwallm__prompt_execution_get()
            )
          },
          .args = c(
            list(
              llm_provider = models$main,
              texts = texts$preprocessed,
              analysis_unit_ids = current_analysis_unit_ids(),
              existing_results = existing_results,
              paragraph_entries = paragraph_entries,
              start_index = as.integer(start_index),
              resume_stage = as.character(resume_stage),
              research_background = research_background(),
              style_prompt = style_prompt(),
              categories = categories$texts(),
              exclusive_categories = categories$exclusive_texts(),
              assign_multiple_categories = assign_multiple_categories(),
              write_paragraphs = write_paragraphs(),
              handle_detailed_error = handle_detailed_error,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            analysis_async_categorization_globals(),
            analysis_async_worker_setup_globals(),
            analysis_async_processing_globals(),
            analysis_async_tokenizer_globals(),
            log_async_globals(log_context),
            send_prompt_with_retries_async_globals()
          )
        )

        bind_async_result(
          promise = promise,
          setter = function(value) {
            append_stage_execution_rows(value$stage_execution_rows)

            if (identical(value$status %||% NULL, "decision_required")) {
              handle_prompt_scope_failure(
                decision = value,
                mode_label = mode(),
                on_retry = function() {
                  if (identical(value$scope_kind %||% NULL, "analysis_unit")) {
                    start_categorization(
                      existing_results = value$results,
                      start_index = value$failed_index,
                      resume_stage = "analysis",
                      is_resume = TRUE
                    )
                  } else {
                    start_categorization(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                },
                on_skip = function(skip_status) {
                  if (identical(value$scope_kind %||% NULL, "analysis_unit")) {
                    skip_row <- value$skip_row
                    skip_row$response_status <- skip_status
                    start_categorization(
                      existing_results = rbind(value$results, skip_row),
                      start_index = value$failed_index + 1L,
                      resume_stage = "analysis",
                      is_resume = TRUE
                    )
                  } else {
                    start_categorization(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index + 1L,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                }
              )
              return(invisible(NULL))
            }

            paragraph_entries_generated(value$paragraphs %||% NULL)
            results_table_pre(value$results)
          },
          when = "main processing of categorization",
          debug_message = "Started async processing for categorization"
        )
      }

      ## 2.4 Scoren ------------------------------------------------------------

      # Handles the scoring mode.
      # Validation and async launch logic stay local to this mode.

      ### 2.4.1 Worker launch --------------------------------------------------

      # Runs scoring for all texts. Unlike categorization, no paragraph step follows.
      start_scoring <- function(
        existing_results = NULL,
        start_index = 1L,
        is_resume = FALSE
      ) {
        req(texts$preprocessed)
        if (!isTRUE(is_resume)) {
          if (
            !processing_texts_under_maximum(
              preprocessed_texts = texts$preprocessed,
              lang = lang()
            )
          ) {
            return()
          }
          if (isTRUE(nchar(scoring_characteristic()) < 1)) {
            shiny::showNotification(
              lang()$t("Geef een karakteristiek op."),
              type = "error"
            )
            return()
          }
          req(isFALSE(context_window$any_fit_problem))
        }

        log_context <- if (isTRUE(is_resume)) {
          log_context_capture(
            is_async = TRUE,
            mode = getOption("app__mode", "unknown")
          )
        } else {
          start_processing_run()
        }

        promise <- mirai::mirai(
          {
            log_context_apply(log_context)
            prepare_async_analysis_worker("scoring")
            .kwallm__prompt_execution_reset()

            on_progress <- function(i, n, text) {
              progress_primary$set_with_total(i, n, text)
              if (i == 1 || i %% 5 == 0 || i == n) {
                log_info(
                  sprintf("Scoring progress: %d/%d", i, n),
                  component = "analysis"
                )
              }
            }

            scoring_output <- score_texts(
              texts = texts,
              analysis_unit_ids = analysis_unit_ids,
              scoring_characteristic = scoring_characteristic,
              research_background = research_background,
              llm_provider = llm_provider,
              on_progress = on_progress,
              interrupter = interrupter,
              start_index = start_index,
              existing_results = existing_results,
              failure_action = "return_decision"
            )

            if (
              identical(scoring_output$status %||% NULL, "decision_required")
            ) {
              scoring_output$stage_execution_rows <-
                .kwallm__prompt_execution_get()
              return(scoring_output)
            }

            list(
              results = scoring_output$results %||% scoring_output,
              stage_execution_rows = .kwallm__prompt_execution_get()
            )
          },
          .args = c(
            list(
              llm_provider = models$main,
              texts = texts$preprocessed,
              analysis_unit_ids = current_analysis_unit_ids(),
              existing_results = existing_results,
              start_index = as.integer(start_index),
              research_background = research_background(),
              scoring_characteristic = scoring_characteristic(),
              progress_primary = progress_primary$async,
              interrupter = interrupter
            ),
            analysis_async_scoring_globals(),
            analysis_async_worker_setup_globals(),
            log_async_globals(log_context),
            send_prompt_with_retries_async_globals()
          )
        )

        bind_async_result(
          promise = promise,
          setter = function(value) {
            append_stage_execution_rows(value$stage_execution_rows)

            if (identical(value$status %||% NULL, "decision_required")) {
              handle_prompt_scope_failure(
                decision = value,
                mode_label = mode(),
                on_retry = function() {
                  start_scoring(
                    existing_results = value$results,
                    start_index = value$failed_index,
                    is_resume = TRUE
                  )
                },
                on_skip = function(skip_status) {
                  skip_row <- value$skip_row
                  skip_row$response_status <- skip_status
                  start_scoring(
                    existing_results = rbind(value$results, skip_row),
                    start_index = value$failed_index + 1L,
                    is_resume = TRUE
                  )
                }
              )
              return(invisible(NULL))
            }

            results_table_pre(value$results)
          },
          when = "main processing of scoring",
          debug_message = "Started async processing for scoring"
        )
      }

      ## 2.5 Onderwerpextractie ------------------------------------------------

      # Handles the topic-modelling flow from start to finish.
      # This covers topic generation, optional editing, and final assignment.

      # Topic-modelling flow: first generate/reduce topics, then optionally let
      # the user edit them, then assign topics and write paragraphs.

      ### 2.5.1 Topic generation -----------------------------------------------

      # Generates candidate topics and reduces them to a usable list.
      # This is the first async step before any human review happens.

      # Starts the first topic-modelling phase.
      # This worker generates candidate topics and reduces them before any human
      # editing or topic assignment happens.
      start_topic_generation <- function() {
        req(texts$preprocessed)
        # `text_batches` here are prompt batches over unique analysis-unit texts.
        req(context_window$text_batches)
        if (
          !processing_texts_under_maximum(
            preprocessed_texts = texts$preprocessed,
            lang = lang()
          )
        ) {
          return()
        }
        req(isFALSE(context_window$any_fit_problem))
        req(isFALSE(context_window$too_many_batches))

        log_context <- start_processing_run(set_initial_progress = FALSE)

        promise <- mirai::mirai(
          {
            log_context_apply(log_context)
            .kwallm__prompt_execution_reset()

            # Step 1: Generate candidate topics
            log_info(
              "Step 1/5: Generating candidate topics...",
              component = "analysis"
            )
            progress_primary$set_with_total(
              1,
              5,
              lang$t("Onderwerpen genereren...")
            )
            progress_secondary$show()
            progress_secondary$set_with_total(
              0,
              length(text_batches),
              lang$t("...")
            )

            candidate_topics <- tryCatch(
              create_candidate_topics(
                text_batches = text_batches,
                analysis_unit_ids = analysis_unit_ids,
                research_background = research_background,
                llm_provider = llm_provider_main,
                language = lang$get_translation_language(),
                on_progress = function(i, n, chunk, result) {
                  progress_secondary$set_with_total(
                    i,
                    n,
                    paste(result, collapse = ",")
                  )
                },
                interrupter = interrupter
              ),
              error = handle_detailed_error("Candidate topic generation")
            )
            progress_secondary$hide()

            # Step 2: Reduce topics
            interrupter$execInterrupts()
            log_info("Step 2/5: Reducing topics...", component = "analysis")
            progress_primary$set_with_total(
              2,
              5,
              lang$t("Onderwerpen reduceren...")
            )

            topics <- tryCatch(
              reduce_topics(
                candidate_topics,
                research_background,
                llm_provider_large,
                language = lang$get_translation_language()
              ),
              error = handle_detailed_error("Topic reduction")
            )

            # Make intermediate results available
            list(
              candidate_topics = candidate_topics,
              topics = topics,
              stage_execution_rows = .kwallm__prompt_execution_get()
            )
          },
          .args = c(
            log_async_globals(log_context),
            send_prompt_with_retries_async_globals(),
            list(
              llm_provider_main = models$main,
              llm_provider_large = models$large,
              texts = texts$preprocessed,
              analysis_unit_ids = current_analysis_unit_ids(),
              research_background = research_background(),
              mode = mode(),
              handle_detailed_error = handle_detailed_error,
              text_batches = context_window$text_batches,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter
            ),
            analysis_async_topic_modelling_globals(),
            analysis_async_tokenizer_globals()
          )
        )
        bind_async_result(
          promise = promise,
          setter = function(value) {
            normalized_topics <- normalize_topic_labels(value$topics)
            append_stage_execution_rows(value$stage_execution_rows)
            candidate_topics_generated(value$candidate_topics)
            reduced_topics_generated(normalized_topics)
            topics_were_edited(FALSE)
            topics(normalized_topics)
          },
          when = "main processing (step 1-2) of topic modelling",
          debug_message = "Started async processing for topic modelling (step 1-2)",
          stop_stream = FALSE,
          hide_stream = TRUE
        )
      }

      ### 2.5.2 Topic editing --------------------------------------------------

      # Launches the optional topic-editing step.
      # Used after topic generation when human review is enabled.

      # Listens for topics becoming available.
      # Opens editing when human review is enabled, otherwise auto-confirms.
      observeEvent(topics(), {
        req(topics())
        req(!topics_definitive())

        # Normalize the exclusive-topic list before the next step uses it.
        # Set 'Onbekend/niet van toepassing' as exclusive topic (if present)
        if (lang()$t("Onbekend/niet van toepassing") %in% topics()) {
          exclusive_topics(c(
            exclusive_topics(),
            lang()$t("Onbekend/niet van toepassing")
          ))
        }
        # Remove any exclusive topics which may not be present in the topics
        exclusive_topics(
          exclusive_topics()[exclusive_topics() %in% topics()]
        )

        fit_info <- topic_assignment_fit_info(topics())
        topic_assignment_overflow <- !isTRUE(fit_info$fits)

        if (topic_assignment_overflow) {
          showNotification(
            topic_assignment_overflow_notice(fit_info),
            type = "error",
            duration = NULL
          )
          log_warn(
            sprintf(
              paste0(
                "Generated topics exceed assignment context window: ",
                "prompt_tokens=%d, context_window_tokens=%d"
              ),
              fit_info$prompt_tokens,
              fit_info$context_window_tokens
            ),
            component = "topics"
          )
        }

        # If no human in the loop and the generated topics fit, auto-confirm.
        if (!isTRUE(human_in_the_loop()) && !topic_assignment_overflow) {
          topics_were_edited(FALSE)
          topics_definitive(TRUE)
          return()
        }

        topic_editor_was_used(TRUE)

        progress_primary$set_with_total(
          2.5,
          5,
          lang()$t("Onderwerpen bewerken...")
        )

        edited_topics <- edit_topics_server(
          "edit_topics",
          topics = topics,
          exclusive_topics = exclusive_topics,
          llm_provider = models$large,
          assignment_texts = reactive(texts$preprocessed),
          assignment_llm_provider = reactive(models$main),
          research_background = research_background,
          assign_multiple_categories = assign_multiple_categories,
          lang = lang
        )

        observeEvent(
          edited_topics(),
          {
            topics_were_edited(
              !identical(edited_topics(), reduced_topics_generated())
            )
            topics(edited_topics())
            topics_definitive(TRUE)
          },
          ignoreInit = TRUE,
          once = TRUE,
          autoDestroy = TRUE
        )
      })

      ### 2.5.3 Topic assignment -----------------------------------------------

      # Starts the final topic step after topics are confirmed.
      # This assigns topics to texts and optionally writes report paragraphs.

      # Starts the second topic-modelling phase after topics are definitive.
      # This worker assigns topics to texts and optionally writes topic
      # paragraphs.
      start_topic_assignment <- function(
        existing_results = NULL,
        paragraph_entries = NULL,
        start_index = 1L,
        resume_stage = c("analysis", "paragraph_generation"),
        is_resume = FALSE
      ) {
        resume_stage <- match.arg(resume_stage)
        req(topics())
        if (!assign_multiple_categories()) {
          # If not assigning multiple categories, set each topic is exclusive
          exclusive_topics(topics())
        }

        log_info(
          sprintf(
            "Starting topic assignment: n_topics=%d, topics=%s",
            length(topics()),
            paste(topics(), collapse = ", ")
          ),
          component = "analysis"
        )

        # Write progress
        progress_primary$set_with_total(
          3,
          5,
          lang()$t("Onderwerpen toekennen...")
        )

        progress_secondary$show()
        progress_secondary$set_with_total(
          0,
          length(texts$preprocessed),
          "..."
        )

        log_context <- if (isTRUE(is_resume)) {
          log_context_capture(
            is_async = TRUE,
            mode = getOption("app__mode", "unknown")
          )
        } else {
          log_context_capture(
            is_async = TRUE,
            mode = getOption("app__mode", "unknown")
          )
        }

        promise <- mirai::mirai(
          {
            log_context_apply(log_context)
            prepare_async_analysis_worker("topic_assignment")
            .kwallm__prompt_execution_reset()

            # Step 4: Assign topics via standalone batch function
            longest_assignment_text <- texts[[which.max(count_tokens(texts))]]
            assignment_prompt <- if (assign_multiple_categories) {
              prompt_multi_category(
                text = longest_assignment_text,
                categories = topics,
                research_background = research_background,
                exclusive_categories = exclusive_topics
              )
            } else {
              prompt_category(
                text = longest_assignment_text,
                categories = topics,
                research_background = research_background
              )
            }

            assignment_context_window <- get_context_window_size_in_tokens(
              llm_provider$parameters$model
            )
            if (is.null(assignment_context_window)) {
              assignment_context_window <- 2048
            }

            assignment_prompt_tokens <- assignment_prompt |>
              tidyprompt::construct_prompt_text() |>
              count_tokens()

            if (assignment_prompt_tokens > assignment_context_window) {
              stop(paste0(
                "Topic assignment prompt exceeds the model context window (",
                assignment_prompt_tokens,
                " > ",
                assignment_context_window,
                " tokens)."
              ))
            }

            on_progress <- function(i, n, text) {
              progress_secondary$set_with_total(i, n, text)
            }

            topic_assignment_results <- existing_results
            paragraphs <- paragraph_entries

            if (identical(resume_stage, "analysis")) {
              topic_assignment_output <- tryCatch(
                {
                  assign_topics(
                    texts = texts,
                    analysis_unit_ids = analysis_unit_ids,
                    topics = topics,
                    research_background = research_background,
                    llm_provider = llm_provider,
                    assign_multiple_categories = assign_multiple_categories,
                    exclusive_topics = exclusive_topics,
                    on_progress = on_progress,
                    interrupter = interrupter,
                    start_index = start_index,
                    existing_results = existing_results,
                    failure_action = "return_decision"
                  )
                },
                error = handle_detailed_error("Topic assignment")
              )

              if (
                identical(
                  topic_assignment_output$status %||% NULL,
                  "decision_required"
                )
              ) {
                progress_secondary$hide()
                topic_assignment_output$stage_execution_rows <-
                  .kwallm__prompt_execution_get()
                topic_assignment_output$resume_stage <- "analysis"
                return(topic_assignment_output)
              }

              topic_assignment_results <-
                topic_assignment_output$results %||% topic_assignment_output
              progress_secondary$hide()
            }

            ## Step 5: Write paragraphs about the topics
            log_info("Step 5/5: Writing paragraphs...", component = "analysis")
            progress_primary$set_with_total(
              4,
              5,
              lang$t("Rapport schrijven...")
            )

            if (write_paragraphs) {
              paragraph_output <- tryCatch(
                {
                  topics_texts_list <- collect_grouped_paragraph_inputs(
                    results = topic_assignment_results,
                    labels = topics,
                    assign_multiple_categories = assign_multiple_categories
                  )

                  write_grouped_paragraphs(
                    grouped_texts = topics_texts_list,
                    research_background = research_background,
                    style_prompt = style_prompt,
                    llm_provider = llm_provider,
                    lang = lang,
                    subject_kind = "topic",
                    progress_secondary = progress_secondary,
                    interrupter = interrupter,
                    llm_stream_async = llm_stream_async,
                    streaming_enabled = streaming_enabled,
                    existing_paragraphs = if (
                      identical(resume_stage, "paragraph_generation")
                    ) {
                      paragraph_entries
                    } else {
                      NULL
                    },
                    start_index = if (
                      identical(resume_stage, "paragraph_generation")
                    ) {
                      start_index
                    } else {
                      1L
                    },
                    failure_action = "return_decision"
                  )
                },
                error = handle_detailed_error("Topic report generation")
              )

              if (
                identical(
                  paragraph_output$status %||% NULL,
                  "decision_required"
                )
              ) {
                paragraph_output$results <- topic_assignment_results
                paragraph_output$resume_stage <- "paragraph_generation"
                paragraph_output$stage_execution_rows <-
                  .kwallm__prompt_execution_get()
                return(paragraph_output)
              }

              paragraphs <- paragraph_output$paragraphs %||% paragraph_output
            } else {
              paragraphs <- NULL
            }

            list(
              results = topic_assignment_results,
              paragraphs = paragraphs,
              stage_execution_rows = .kwallm__prompt_execution_get()
            )
          },
          .args = c(
            send_prompt_with_retries_async_globals(),
            list(
              topics = topics(),
              llm_provider = models$main,
              texts = texts$preprocessed,
              analysis_unit_ids = current_analysis_unit_ids(),
              existing_results = existing_results,
              paragraph_entries = paragraph_entries,
              start_index = as.integer(start_index),
              resume_stage = as.character(resume_stage),
              research_background = research_background(),
              style_prompt = style_prompt(),
              mode = mode(),
              assign_multiple_categories = assign_multiple_categories(),
              write_paragraphs = write_paragraphs(),
              handle_detailed_error = handle_detailed_error,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              exclusive_topics = exclusive_topics(),
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            analysis_async_topic_modelling_globals(),
            analysis_async_worker_setup_globals(),
            analysis_async_processing_globals(),
            analysis_async_tokenizer_globals(),
            log_async_globals(log_context)
          )
        )
        bind_async_result(
          promise = promise,
          setter = function(value) {
            append_stage_execution_rows(value$stage_execution_rows)

            if (identical(value$status %||% NULL, "decision_required")) {
              handle_prompt_scope_failure(
                decision = value,
                mode_label = mode(),
                on_retry = function() {
                  if (identical(value$scope_kind %||% NULL, "analysis_unit")) {
                    start_topic_assignment(
                      existing_results = value$results,
                      start_index = value$failed_index,
                      resume_stage = "analysis",
                      is_resume = TRUE
                    )
                  } else {
                    start_topic_assignment(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                },
                on_skip = function(skip_status) {
                  if (identical(value$scope_kind %||% NULL, "analysis_unit")) {
                    skip_row <- value$skip_row
                    skip_row$response_status <- skip_status
                    start_topic_assignment(
                      existing_results = rbind(value$results, skip_row),
                      start_index = value$failed_index + 1L,
                      resume_stage = "analysis",
                      is_resume = TRUE
                    )
                  } else {
                    start_topic_assignment(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index + 1L,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                }
              )
              return(invisible(NULL))
            }

            paragraph_entries_generated(value$paragraphs %||% NULL)
            results_table_pre(value$results)
          },
          when = "main processing (step 3-4) of topic modelling",
          debug_message = "Started async processing for topic modelling (step 3-4)"
        )
      }

      # Listens for topics being confirmed.
      # Starts assignment once the topic list is final.
      observeEvent(topics_definitive(), {
        if (!isTRUE(topics_definitive())) {
          return()
        }

        # Topic generation/editing ends here; assignment becomes a separate step.
        start_topic_assignment()
      })

      ## 2.6 Markeren ----------------------------------------------------------

      # Handles the marking mode.
      # This groups the marking validation and async worker launch together.

      ### 2.6.1 Worker launch --------------------------------------------------

      # Runs the marking flow, including chunking-aware analysis and optional
      # report paragraphs.
      # Starts the async worker for marking mode.
      # Kept separate so the marking validation and worker call do not clutter
      # the shared process observer.
      start_marking <- function(
        existing_results = NULL,
        paragraph_entries = NULL,
        start_index = 1L,
        resume_stage = c("marking", "paragraph_generation"),
        is_resume = FALSE
      ) {
        resume_stage <- match.arg(resume_stage)
        req(texts$preprocessed)
        if (!isTRUE(is_resume)) {
          paragraph_entries_generated(NULL)
          results_table_pre(NULL)
        }
        if (
          !processing_texts_under_maximum(
            preprocessed_texts = texts$preprocessed,
            lang = lang()
          )
        ) {
          return()
        }
        if (codes$editing()) {
          shiny::showNotification(
            lang()$t(
              "Je moet eerst de codes opslaan voordat je verder kunt gaan."
            ),
            type = "error"
          )
          return()
        }
        if (codes$unique_non_empty_count() < 1) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 1 code opgeven."),
            type = "error"
          )
          return()
        }
        if (length(unique(codes$texts())) < length(codes$texts())) {
          shiny::showNotification(
            lang()$t("Codes moeten uniek zijn."),
            type = "error"
          )
          return()
        }
        req(isFALSE(context_window$any_fit_problem))
        req(context_window$max_tokens)
        req(context_window$overlap)

        log_context <- start_processing_run()

        promise <- mirai::mirai(
          {
            log_context_apply(log_context)
            prepare_async_analysis_worker("marking")
            .kwallm__prompt_execution_reset()

            marking_output <- mark_texts(
              texts = texts,
              analysis_unit_ids = analysis_unit_ids,
              codes = codes,
              research_background = research_background,
              style_prompt = style_prompt,
              llm_provider = llm_provider,
              progress_primary = progress_primary,
              progress_secondary = progress_secondary,
              interrupter = interrupter,
              lang = lang,
              write_paragraphs = write_paragraphs,
              text_size_tokens = text_size_tokens,
              overlap_size_tokens = overlap_size_tokens,
              llm_stream_async = llm_stream_async,
              streaming_enabled = streaming_enabled,
              existing_results = existing_results,
              paragraph_entries = paragraph_entries,
              start_index = start_index,
              resume_stage = resume_stage,
              failure_action = "return_decision"
            )

            if (
              identical(marking_output$status %||% NULL, "decision_required")
            ) {
              marking_output$stage_execution_rows <-
                .kwallm__prompt_execution_get()
              return(marking_output)
            }

            marking_results <- marking_output$results %||% marking_output
            paragraphs <- marking_output$paragraphs %||%
              attr(marking_results, "paragraphs", exact = TRUE)
            attr(marking_results, "paragraphs") <- NULL

            list(
              results = marking_results,
              paragraphs = paragraphs,
              stage_execution_rows = .kwallm__prompt_execution_get()
            )
          },
          .args = c(
            list(
              llm_provider = models$main,
              texts = texts$preprocessed,
              analysis_unit_ids = current_analysis_unit_ids(),
              research_background = research_background(),
              style_prompt = style_prompt(),
              codes = codes$texts(),
              existing_results = existing_results,
              paragraph_entries = paragraph_entries,
              start_index = as.integer(start_index),
              resume_stage = as.character(resume_stage),
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              write_paragraphs = write_paragraphs(),
              text_size_tokens = context_window$max_tokens,
              overlap_size_tokens = context_window$overlap,
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            analysis_async_marking_globals(),
            analysis_async_worker_setup_globals(),
            analysis_async_tokenizer_globals(),
            log_async_globals(log_context),
            send_prompt_with_retries_async_globals()
          )
        )

        bind_async_result(
          promise = promise,
          setter = function(value) {
            append_stage_execution_rows(value$stage_execution_rows)

            if (identical(value$status %||% NULL, "decision_required")) {
              handle_prompt_scope_failure(
                decision = value,
                mode_label = mode(),
                on_retry = function() {
                  if (identical(value$scope_kind %||% NULL, "chunk_code")) {
                    start_marking(
                      existing_results = value$results,
                      start_index = value$failed_index,
                      resume_stage = "marking",
                      is_resume = TRUE
                    )
                  } else {
                    start_marking(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                },
                on_skip = function(skip_status) {
                  if (identical(value$scope_kind %||% NULL, "chunk_code")) {
                    skip_row <- value$skip_row
                    skip_row$response_status <- skip_status
                    start_marking(
                      existing_results = rbind(value$results, skip_row),
                      start_index = value$failed_index + 1L,
                      resume_stage = "marking",
                      is_resume = TRUE
                    )
                  } else {
                    start_marking(
                      existing_results = value$results,
                      paragraph_entries = value$paragraphs,
                      start_index = value$failed_index + 1L,
                      resume_stage = value$resume_stage %||%
                        "paragraph_generation",
                      is_resume = TRUE
                    )
                  }
                }
              )
              return(invisible(NULL))
            }

            paragraph_entries_generated(value$paragraphs %||% NULL)
            results_table_pre(value$results)
          },
          when = "main processing of marking",
          stop_stream = TRUE
        )
      }

      ## 2.7 Shared dispatch & result prep -------------------------------------

      # Handles the shared pieces after the mode-specific launchers.
      # This routes the process button, finalizes UI, and prepares result data.

      ### 2.7.1 Process dispatch -----------------------------------------------

      # Single process-button observer.
      # Logs the click once and dispatches to the matching mode-specific starter.
      observeEvent(input$process, {
        if (processing()) {
          return()
        }

        if (!processing_split_ready(split_in_progress(), lang())) {
          return()
        }

        if (!processing_anonymization_ready(texts, lang())) {
          return()
        }

        log_action(
          "analysis_process_clicked",
          details = sprintf(
            "mode=%s n_texts=%d",
            mode() %||% "unknown",
            length(texts$preprocessed %||% character(0))
          )
        )

        switch(
          mode(),
          "Categorisatie" = start_categorization(),
          "Scoren" = start_scoring(),
          "Onderwerpextractie" = start_topic_generation(),
          "Markeren" = start_marking()
        )
      })

      ### 2.7.2 Processing completion UI ---------------------------------------

      ### 2.7.3 Prompt snapshot ------------------------------------------------

      # Builds the prompt previews stored in the AnalysisResult.
      # Used during download preparation so the prompt-building logic stays out
      # of the final AnalysisResult assembly code.
      build_stage_prompt_previews <- function() {
        mode_display <- mode()
        stage_prompt_previews <- list()
        placeholder_text <- lang()$t("<< TEKST >>")

        if (mode_display == "Categorisatie") {
          if (isTRUE(assign_multiple_categories())) {
            stage_prompt_previews$categorization <- prompt_multi_category(
              text = placeholder_text,
              research_background = research_background(),
              categories = categories$texts(),
              exclusive_categories = categories$exclusive_texts()
            ) |>
              tidyprompt::construct_prompt_text()
          } else {
            stage_prompt_previews$categorization <- prompt_category(
              text = placeholder_text,
              research_background = research_background(),
              categories = categories$texts()
            ) |>
              tidyprompt::construct_prompt_text()
          }

          if (isTRUE(write_paragraphs())) {
            stage_prompt_previews$paragraph_generation <- prompt_write_paragraph(
              texts = placeholder_text,
              topic = "<< CATEGORIE >>",
              research_background = research_background(),
              style_prompt = style_prompt(),
              language = lang()$get_translation_language()
            ) |>
              tidyprompt::construct_prompt_text()
          }

          return(stage_prompt_previews)
        }

        if (mode_display == "Scoren") {
          stage_prompt_previews$scoring <- prompt_score(
            text = placeholder_text,
            research_background = research_background(),
            scoring_characteristic = scoring_characteristic()
          ) |>
            tidyprompt::construct_prompt_text()

          return(stage_prompt_previews)
        }

        if (mode_display == "Onderwerpextractie") {
          reduction_summary <- attr(
            reduced_topics_generated(),
            "reduction_summary",
            exact = TRUE
          ) %||%
            list()
          assignment_topics <- topics() %||%
            c("<< ONDERWERP 1 >>", "<< ONDERWERP 2 >>")

          stage_prompt_previews$topic_candidate_generation <- prompt_candidate_topics(
            text_batch = c(
              lang()$t("<< TEKST 1 >>"),
              lang()$t("<< TEKST 2 >>")
            ),
            research_background = research_background(),
            language = lang()$get_translation_language()
          ) |>
            tidyprompt::construct_prompt_text()

          stage_prompt_previews$topic_reduction <- prompt_reduce_topics(
            candidate_topics = c("<< ONDERWERP 1 >>", "<< ONDERWERP 2 >>"),
            research_background = research_background(),
            language = lang()$get_translation_language()
          ) |>
            tidyprompt::construct_prompt_text()

          if (
            isTRUE(reduction_summary$not_applicable_check_performed %||% FALSE)
          ) {
            stage_prompt_previews$topic_not_applicable_check <- prompt_topic_not_applicable_check(
              topics = c("<< ONDERWERP 1 >>", "<< ONDERWERP 2 >>"),
              language = lang()$get_translation_language()
            ) |>
              tidyprompt::construct_prompt_text()
          }

          if (isTRUE(assign_multiple_categories())) {
            stage_prompt_previews$topic_assignment <- prompt_multi_category(
              text = placeholder_text,
              research_background = research_background(),
              categories = assignment_topics,
              exclusive_categories = exclusive_topics()
            ) |>
              tidyprompt::construct_prompt_text()
          } else {
            stage_prompt_previews$topic_assignment <- prompt_category(
              text = placeholder_text,
              research_background = research_background(),
              categories = assignment_topics
            ) |>
              tidyprompt::construct_prompt_text()
          }

          if (isTRUE(write_paragraphs())) {
            stage_prompt_previews$paragraph_generation <- prompt_write_paragraph(
              texts = placeholder_text,
              topic = "<< ONDERWERP >>",
              research_background = research_background(),
              style_prompt = style_prompt(),
              language = lang()$get_translation_language()
            ) |>
              tidyprompt::construct_prompt_text()
          }

          return(stage_prompt_previews)
        }

        if (mode_display == "Markeren") {
          stage_prompt_previews$marking <- mark_text_prompt(
            text = placeholder_text,
            research_background = research_background(),
            code = "<< CODE >>"
          ) |>
            tidyprompt::construct_prompt_text()

          if (isTRUE(write_paragraphs())) {
            stage_prompt_previews$paragraph_generation <- prompt_write_paragraph(
              texts = "**<< GEMARKEERDE TEKST >>**",
              topic = "<< CODE >>",
              research_background = research_background(),
              style_prompt = style_prompt(),
              language = lang()$get_translation_language(),
              focus_on_highlighted_text = TRUE
            ) |>
              tidyprompt::construct_prompt_text()
          }

          return(stage_prompt_previews)
        }

        stage_prompt_previews
      }

      ### 2.7.4 Final result preparation ---------------------------------------

      # Builds the final AnalysisResult and starts file generation.
      # Called once the optional IRR step is finished or skipped.
      prepare_download_after_irr <- function() {
        merged_input_info <- utils::modifyList(
          upload_info() %||% list(),
          list(
            anonymization_requested_mode = texts$anonymization_requested_mode %||%
              NULL,
            anonymization_applied_mode = texts$anonymization_applied_mode %||%
              NULL,
            anonymization_completed = texts$anonymization_completed %||% NULL,
            split_enabled = split_settings()$enabled %||% NULL,
            split_chunk_size = split_settings()$chunk_size %||% NULL,
            split_overlap = split_settings()$overlap %||% NULL
          )
        )

        analysis_result <- build_analysis_result(
          texts_df = texts$df,
          results_table = results_table(),
          paragraph_entries = paragraph_entries_generated(),
          uuid = uuid,
          mode = mode(),
          research_background = research_background(),
          style_prompt = style_prompt(),
          irr_result = irr_result(),
          language = lang()$get_translation_language(),
          by_column_name = by_column_name(),
          by_column_lookup = by_column_lookup(),
          models = models,
          categories = categories$texts(),
          exclusive_categories = categories$exclusive_texts(),
          scoring_characteristic = scoring_characteristic(),
          topics = topics(),
          exclusive_topics = exclusive_topics(),
          codes = codes$texts(),
          assign_multiple_categories = assign_multiple_categories(),
          human_in_the_loop = human_in_the_loop() || topic_editor_was_used(),
          write_paragraphs = write_paragraphs(),
          context_window = context_window,
          stage_prompt_previews = build_stage_prompt_previews(),
          stage_execution_rows = stage_execution_rows_generated(),
          app_version = getOption("kwallm__app_version", NULL),
          input_info = merged_input_info,
          candidate_topics = candidate_topics_generated(),
          reduced_topics = reduced_topics_generated(),
          topics_were_edited = topics_were_edited(),
          irr_sample = irr_sample()
        )

        expected_paragraph_subjects <-
          analysis_result_expected_paragraph_subject_count(analysis_result)

        if (
          isTRUE(write_paragraphs()) &&
            expected_paragraph_subjects > 0L &&
            nrow(analysis_result@paragraphs@paragraphs) == 0
        ) {
          app_error(
            "Paragraphs were requested to be written, but no paragraphs found",
            when = "building AnalysisResult, checking paragraph presence",
            fatal = TRUE,
            lang = lang()
          )
        }

        # Abort if invalid results still made it this far.
        if (
          processing_results_have_invalid_na(
            .kwallm_report_results_df(analysis_result),
            mode()
          )
        ) {
          app_error(
            "Results contain NA values; processing failed",
            when = "after inter-rater reliability completion",
            fatal = TRUE,
            lang = lang()
          )
        }

        preparing_download(TRUE)

        log_action(
          "results_download_preparing",
          details = sprintf(
            "mode=%s n_texts=%d uuid=%s",
            mode() %||% "unknown",
            nrow(analysis_result@text_lineage@documents),
            uuid
          )
        )

        promise <- mirai::mirai(
          {
            create_analysis_result_download_bundle(
              analysis_result = analysis_result,
              temp_dir = temp_dir
            )
          },
          .args = c(
            list(
              analysis_result = analysis_result,
              temp_dir = tempdir()
            ),
            analysis_result_async_globals()
          )
        )

        bind_async_result(
          promise = promise,
          setter = zip_file,
          when = "preparing download (excel, rmarkdown, zip)",
          stop_stream = FALSE,
          hide_stream = FALSE
        )

        shinyjs::hide("process")
      }

      ## 2.8 Post-processing & downloads ---------------------------------------

      # Runs after worker results come back.
      # This joins results, handles IRR, builds files, and exposes downloads.

      ### 2.8.1 Processing results ---------------------------------------------

      # Handles the first result coming back from processing.
      # This restores raw texts, finishes the UI, and starts IRR when needed.

      observeEvent(results_table_pre(), {
        req(results_table_pre())
        log_debug(
          sprintf(
            "Results received: n_rows=%d",
            nrow(results_table_pre())
          ),
          component = "analysis"
        )

        result <- join_processing_results(
          texts_df = texts$df,
          results_table_pre = results_table_pre()
        )

        results_table(result)

        # NA results indicate a failed worker response for categorization,
        # scoring, and topic assignment. Marking allows NA snippet columns.
        if (processing_results_have_invalid_na(result, mode())) {
          log_action(
            "analysis_failed",
            details = sprintf(
              "mode=%s when=%s error_class=%s error=%s",
              mode() %||% "unknown",
              "processing results",
              "NA_results",
              "Results contain NA values"
            )
          )
          app_error(
            "Results contain NA values; processing failed",
            when = "processing results",
            fatal = TRUE,
            lang = lang()
          )
        }

        progress_primary$async$stop()
        progress_primary$set(
          100,
          paste0(
            bsicons::bs_icon("check2-circle"),
            lang()$t(" Verwerking voltooid!")
          )
        )
        progress_secondary$async$stop()
        progress_secondary$hide()
        llm_stream$async$stop()
        llm_stream$hide()

        if (interrater_reliability_toggle()) {
          irr_rating_data <- result
          if ("response_status" %in% names(irr_rating_data)) {
            irr_rating_data <- irr_rating_data[
              is.na(irr_rating_data$response_status) |
                irr_rating_data$response_status == "completed",
              ,
              drop = FALSE
            ]
          }

          if (nrow(irr_rating_data) == 0L) {
            shiny::showNotification(
              lang()$t(
                "Geen voltooide analyse-eenheden aanwezig; kan geen interrater-reliability berekenen"
              )
            )
            prepare_download_after_irr()
            return()
          }

          all_categories <- switch(
            mode(),
            "Categorisatie" = categories$texts(),
            "Onderwerpextractie" = topics(),
            NULL
          )

          if (
            mode() %in%
              c("Categorisatie", "Onderwerpextractie") &&
              length(unique(all_categories)) < 2
          ) {
            shiny::showNotification(paste0(
              lang()$t("Niet meer dan 1 categorie aanwezig in data; "),
              lang()$t(" kan geen interrater-reliability berekenen")
            ))
            prepare_download_after_irr()
            return()
          }

          irr <- interrater_server(
            id = "rater_modal",
            rating_data = irr_rating_data,
            text_col = "text",
            all_categories = all_categories,
            mode = mode(),
            assign_multiple_categories = assign_multiple_categories(),
            lang = lang
          )
          irr$start()

          observeEvent(
            irr$done,
            {
              if (!isTRUE(irr$done)) {
                return()
              }

              irr_result(irr$result)
              irr_sample(irr$sample)
              prepare_download_after_irr()
            },
            ignoreInit = TRUE,
            once = TRUE,
            autoDestroy = TRUE
          )
        } else {
          prepare_download_after_irr()
        }
      })

      ### 2.8.2 Download preparation UI ----------------------------------------

      # Shows the loading state while files are being prepared.
      # This switches to the download and restart controls once ready.

      output$download_ui <- renderUI({
        req(preparing_download())
        if (is.null(zip_file())) {
          div(
            class = "text-center",
            br(),
            tags$div(
              class = "spinner-border",
              role = "status",
              tags$span(class = "visually-hidden", "Loading...")
            ),
            br(),
            p(lang()$t("Download wordt voorbereid..."))
          )
        } else {
          # Once the bundle exists, swap the spinner for the action buttons.
          tagList(
            uiOutput(ns("download_button")),
            uiOutput(ns("restart_button"))
          )
        }
      })

      ### 2.8.4 Download ready UI ----------------------------------------------

      # Runs when the zip file is ready for the user.
      # This logs timing, exposes the download handler, and shows restart.

      observeEvent(zip_file(), {
        if (is.null(zip_file())) {
          return()
        }

        # End-to-end duration: from process click to download-ready
        started_at <- analysis_started_at()
        if (!is.null(started_at)) {
          duration_secs <- as.numeric(difftime(
            Sys.time(),
            started_at,
            units = "secs"
          ))
          n_texts <- length(texts$preprocessed %||% character(0))
          total_chars <- sum(
            nchar(texts$preprocessed %||% character(0), allowNA = TRUE),
            na.rm = TRUE
          )
          secs_per_text <- if (n_texts > 0) {
            duration_secs / n_texts
          } else {
            NA_real_
          }
          secs_per_1k_chars <- if (total_chars > 0) {
            (duration_secs / total_chars) * 1000
          } else {
            NA_real_
          }

          log_info(
            sprintf(
              paste0(
                "Analysis total duration (click->download-ready): ",
                "mode=%s, n_texts_preprocessed=%d, total_chars_preprocessed=%d, uuid=%s, ",
                "duration=%.1fs, avg=%.3fs/text, avg=%.3fs/1k_chars"
              ),
              mode() %||% "unknown",
              n_texts,
              total_chars,
              uuid,
              duration_secs,
              secs_per_text,
              secs_per_1k_chars
            ),
            component = "analysis"
          )
        }

        zip_bytes <- tryCatch(file.size(zip_file()), error = function(e) {
          NA_integer_
        })

        log_info(
          sprintf(
            "Output files ready: mode=%s, n_texts=%d, uuid=%s, zip_bytes=%s",
            mode(),
            nrow(results_table()),
            uuid,
            as.character(zip_bytes)
          ),
          component = "output"
        )

        output$download_results <- downloadHandler(
          filename = function() {
            paste0(uuid, ".zip")
          },
          content = function(file) {
            zip_bytes <- tryCatch(file.size(zip_file()), error = function(e) {
              NA_integer_
            })
            log_action(
              "results_download_started",
              details = sprintf(
                "mode=%s uuid=%s zip_bytes=%s",
                mode() %||% "unknown",
                uuid,
                as.character(zip_bytes)
              )
            )
            log_info(
              sprintf("Results downloaded: uuid=%s mode=%s", uuid, mode()),
              component = "download"
            )
            file.copy(zip_file(), file)
          },
          contentType = "application/zip; charset=utf-8"
        )

        output$download_button <- renderUI({
          div(
            class = "text-center",
            br(),
            downloadButton(
              ns("download_results"),
              label = lang()$t("Download resultaten"),
              class = "btn btn-success"
            )
          )
        })

        output$restart_button <- renderUI({
          div(
            class = "text-center",
            br(),
            actionButton(
              ns("restart"),
              label = HTML(
                paste0(
                  bsicons::bs_icon("arrow-clockwise"),
                  lang()$t(" Nieuwe analyse")
                )
              ),
              class = "btn btn-primary"
            )
          )
        })

        # Reaching download-ready state marks the full flow as successful.
        success(TRUE)
      })

      ### 2.8.5 Restart flow ---------------------------------------------------

      # Lets the user start over after a completed run.
      # This shows a confirmation modal and reloads the session if confirmed.

      observeEvent(input$restart, {
        showModal(modalDialog(
          title = lang()$t("Nieuwe analyse starten?"),
          tags$div(
            style = "display:none;",
            `data-kwallm-modal-id` = "analysis_restart_modal",
            `data-kwallm-modal-details` = sprintf(
              "mode=%s",
              mode() %||% "unknown"
            )
          ),
          lang()$t("Zorg dat je eerst de resultaten downloadt."),
          footer = modal_footer_confirm(
            cancel_label = lang()$t("Annuleren"),
            confirm_id = "confirm_restart",
            confirm_label = lang()$t("Ja, nieuwe analyse"),
            ns = ns
          )
        ))
      })

      observeEvent(input$confirm_restart, {
        log_action(
          "analysis_restart_confirmed",
          details = sprintf("mode=%s", mode() %||% "unknown")
        )
        log_action("analysis_restart", details = mode())
        removeModal()
        session$reload()
      })

      ## 2.9 UI controls -------------------------------------------------------

      # Sets up the UI controllers around the processing flow.
      # These pieces drive progress, streaming, start/cancel, and restart.

      ### 2.9.1 Progress bars --------------------------------------------------

      # Shows primary and secondary progress during processing.
      # These controllers are shared by the async workers and UI updates.

      progress_primary <- progress_bar_server("progress_primary")
      progress_secondary <- progress_bar_server(
        "progress_secondary",
        initially_hidden = TRUE
      )

      ### 2.9.2 LLM streaming --------------------------------------------------

      # Shows live paragraph streaming when supported by the model.
      # This gives the user feedback while report paragraphs are being written.

      llm_stream <- llm_streaming_server(
        "llm_stream",
        initially_hidden = TRUE
      )

      ### 2.9.3 Processing button ----------------------------------------------

      # Renders the main start button for the current mode.
      # This keeps the label and disabled state in sync with the current inputs.

      output$process_button <- renderUI({
        req(mode(), lang())

        # Once processing has started, keep the start button hidden.
        if (
          isTRUE(started()) ||
            isTRUE(processing()) ||
            isTRUE(preparing_download()) ||
            !is.null(zip_file())
        ) {
          return(NULL)
        }

        # Count shown on the button reflects the preprocessed texts that will
        # actually be sent into the active analysis flow.
        n_pre <- length(texts$preprocessed %||% character(0))

        btn_label <- switch(
          mode(),
          "Categorisatie" = paste0(lang()$t("Categoriseer"), " (", n_pre, ")"),
          "Scoren" = paste0(lang()$t("Scoreer"), " (", n_pre, ")"),
          "Onderwerpextractie" = paste0(
            lang()$t("Extraheer"),
            " (",
            n_pre,
            ")"
          ),
          "Markeren" = paste0(lang()$t("Markeer"), " (", n_pre, ")"),
          paste0(lang()$t("Verwerk"), " (", n_pre, ")")
        )

        # Disable if no texts OR if there is a context-window fit problem
        disable_flag <- (n_pre == 0) ||
          isTRUE(context_window$any_fit_problem) ||
          isTRUE(context_window$too_many_batches) ||
          isTRUE(processing_has_pending_gliner_anonymization(texts)) ||
          isTRUE(split_in_progress())

        actionButton(
          ns("process"),
          label = btn_label,
          class = "btn btn-primary btn-lg snake-btn",
          disabled = disable_flag
        )
      })

      ### 2.9.4 Interruption & cancel ------------------------------------------

      # Handles cancellation and session shutdown while processing runs.
      # This makes sure async work can be stopped cleanly when needed.

      # Shared interrupter for cancellation and session shutdown.
      interrupter <- ipc::AsyncInterruptor$new()

      shiny::onStop(function() {
        try(
          {
            interrupter$interrupt(
              "Shiny session was stopped (`shiny::onStop()`)"
            )
            interrupter$destroy()
          },
          silent = TRUE
        )
      })

      output$cancel_button <- renderUI({
        req(isTRUE(processing()))
        # Cancellation is only available while analysis is still running.
        req(!isTRUE(preparing_download()))
        req(is.null(zip_file()))

        actionButton(
          inputId = ns("cancel"),
          label = lang()$t("Annuleer"),
          class = "btn",
          style = "
            color: #000;
            background-color: transparent;
            border: 1px solid rgba(0,0,0,0.3);
          "
        )
      })

      observeEvent(input$cancel, {
        req(isTRUE(processing()))

        log_action(
          "analysis_cancel_clicked",
          details = sprintf("mode=%s", mode() %||% "unknown")
        )

        removeModal()
        showModal(modalDialog(
          title = lang()$t("Annuleren?"),
          tags$div(
            style = "display:none;",
            `data-kwallm-modal-id` = "analysis_cancel_modal",
            `data-kwallm-modal-details` = sprintf(
              "mode=%s",
              mode() %||% "unknown"
            )
          ),
          lang()$t("Weet je zeker dat je de analyse wilt annuleren?"),
          footer = modal_footer_confirm(
            cancel_label = lang()$t("Nee, niet annuleren"),
            confirm_id = "confirm_cancel",
            confirm_label = lang()$t("Ja, annuleren"),
            ns = ns
          )
        ))
      })

      observeEvent(input$confirm_cancel, {
        req(isTRUE(processing()))

        log_action(
          "analysis_cancel_confirmed",
          details = sprintf("mode=%s", mode() %||% "unknown")
        )

        log_analysis_interrupted(mode = mode(), reason = "user cancelled")
        removeModal()
        session$reload()
      })

      ## 2.10 Return value -----------------------------------------------------

      # Returns a small processing interface to other modules.
      # The function reports current state and exposes simple reactive flags.

      # These attributes expose coarse milestones without leaking internals.
      attr(processing, "has_started") <- reactive({
        isTRUE(started()) ||
          isTRUE(processing()) ||
          !is.null(results_table_pre()) ||
          isTRUE(preparing_download()) ||
          !is.null(zip_file())
      })

      attr(processing, "has_results") <- reactive({
        !is.null(zip_file())
      })

      return(processing)
    }
  )
}

# 3 Helpers --------------------------------------------------------------------

# See R/utils_processing_helpers.R for helpers related to the processing logic
