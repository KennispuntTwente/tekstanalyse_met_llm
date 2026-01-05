# Module responsible for launching the process & showing progress
# Will also stop the app and return the results once done
# See start of moduleServer for more details about the process

# 1 UI ----------------------------------------------------------------------

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


# 2 Server ----------------------------------------------------------------

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
  lang = default_lang()
) {
  ns <- NS(id)

  moduleServer(
    id,
    function(input, output, session) {
      # Processing state management ------------------------------------
      # Basic overview of the process:
      #   > Listen for start button click
      #   > Start processing based on mode (categorization/scoring/topic modelling)
      #     Processing happens async
      #     Async because we don't want to block the Shiny app for other users
      #     Because it is async, progress is written to a file
      #   > Main process reads progress from the file and updates the progress bar
      #   > When processing is done (i.e., value stored in 'results_df()'),
      #     we join results with the original texts and store as 'final_results_df()'
      #     (because we sent pre-processed, anonymized texts to the LLM)
      #   > If interrater reliability is toggled, we run the interrater reliability
      #     module
      #   > Create result list with all the results, including metadata.
      #     Also create Excel and Rmarkdown files with the results
      #   > Make download available

      # Processing state management with reactive values:
      #   processing: reactiveVal to keep track of processing state
      #     This is used to disable input fields during processing
      #   uuid: reactiveVal to store the UUID of the current processing task
      #   results_df: reactiveVal to store the raw results of the processing task
      #     (Has preprocessed texts instead of original texts)
      #   final_results_df: reactiveVal to store the final results df
      #     (Has original texts instead of preprocessed texts)
      #   irr_done: reactiveVal to keep track of interrater reliability state
      #       Goes to TRUE when interrater reliability is done or not needed
      #   irr_result: reactiveVal to store the interrater reliability result
      #   preparing_download: reactiveVal to indicate we are preparing the download
      #     Enables showing a loading animation
      #   zip_file: reactiveVal to store the path to the zip file
      #     Is populated when the files are ready, and passed to the download handler
      #   topics: reactiveVal to store topics
      #   topics_definitive: reactiveVal to keep track of whether the topics are definitive
      #     Is false when the user is editing the topics, otherwise true
      #   succes: reactiveVal to keep track of whether the processing
      #     has been finished succesfully. Used for automated testing
      processing <- reactiveVal(FALSE)
      started <- reactiveVal(FALSE)
      results_df <- reactiveVal(NULL)
      final_results_df <- reactiveVal(NULL)
      irr_done <- reactiveVal(FALSE)
      irr_result <- reactiveVal(NULL)
      preparing_download <- reactiveVal(NULL)
      zip_file <- reactiveVal(NULL)
      topics <- reactiveVal(NULL)
      exclusive_topics <- reactiveVal(NULL)
      topics_definitive <- reactiveVal(FALSE)
      success <- reactiveVal(NULL)

      # Timestamp for end-to-end duration (click -> download-ready)
      analysis_started_at <- reactiveVal(NULL)

      shiny::exportTestValues(
        processing = processing(),
        started = started(),
        success = success(),
        final_results_df = final_results_df()
      )

      # UUID for the current processing task
      uuid <- uuid::UUIDgenerate()

      # Launch processing --------------------------------------------
      # Launch processing when button is clicked;
      #   set reactive values to keep track of processing state
      #   and store the UUID of the current processing task
      #   Prevent multiple processing tasks from running at the same time
      #   Run asynchronous processing using future_promise
      #     (to prevent blocking the Shiny app)

      # Helper to check if number of texts is under maximum
      # TODO: set different maximum, or set limits per user?
      number_of_texts_under_maximum <- function(
        maximum = getOption("processing__max_texts", 3000)
      ) {
        if (length(texts$preprocessed) > maximum) {
          shiny::showNotification(
            paste0(
              lang()$t("Je mag maximaal "),
              maximum,
              lang()$t(" teksten analyseren.")
            ),
            type = "error"
          )
          return(FALSE)
        }
        return(TRUE)
      }

      ## Categorisation & scoring ------------------------------------
      # Listen for process button click when in categorization or scoring mode
      # Launch processing
      observeEvent(input$process, {
        if (processing()) {
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

        req(texts$preprocessed)
        if (!mode() %in% c("Categorisatie", "Scoren")) {
          return()
        }
        if (!number_of_texts_under_maximum()) {
          return()
        }
        if (mode() == "Categorisatie" && !categories_are_valid()) {
          return()
        }
        if (mode() == "Scoren" && !scoring_characteristic_is_valid()) {
          return()
        }
        req(isFALSE(context_window$any_fit_problem))

        started(TRUE)
        # Set processing state
        processing(TRUE)
        analysis_started_at(Sys.time())
        log_analysis_start(
          mode = mode(),
          n_texts = length(texts$preprocessed),
          model = models$main$parameters$model %||% "unknown"
        )
        # Set initial progress
        progress_primary$set_with_total(0, length(texts$preprocessed), "...")

        # Disable button
        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        log_ctx <- log_context_capture(
          is_async = TRUE,
          mode = getOption("app__mode", "unknown")
        )

        future_promise(
          {
            log_context_apply(log_ctx)

            results <- vector("list", length(texts))

            for (i in seq_along(texts)) {
              interrupter$execInterrupts()
              text <- texts[[i]]

              prompt <- if (mode == "Categorisatie") {
                if (!assign_multiple_categories) {
                  prompt_category(
                    text = text,
                    research_background = research_background,
                    categories = categories
                  )
                } else {
                  prompt_multi_category(
                    text = text,
                    research_background = research_background,
                    categories = categories,
                    exclusive_categories = exclusive_categories
                  )
                }
              } else {
                prompt_score(
                  text,
                  research_background,
                  scoring_characteristic
                )
              }

              result <- send_prompt_with_retries(
                prompt,
                llm_provider = llm_provider
              )
              results[[i]] <- result

              progress_primary$set_with_total(i, length(texts), text)
              if (i == 1 || i %% 5 == 0 || i == length(texts)) {
                log_info(
                  sprintf(
                    "Categorization/Scoring progress: %d/%d",
                    i,
                    length(texts)
                  ),
                  component = "analysis"
                )
              }

              if (is.na(result)) break
            }

            # Turn into data frame
            results <- unlist(results)
            if (anyNA(results)) {
              results <- rep(NA, length(texts))
            }
            results <- data.frame(
              text = texts,
              result = results,
              stringsAsFactors = FALSE
            )

            # If multiple categories, convert from JSON array string to
            #   multiple binary columns
            if (mode == "Categorisatie" && assign_multiple_categories) {
              # Parse the JSON array strings into vector-column
              results <- results |>
                dplyr::mutate(
                  result = purrr::map(result, jsonlite::fromJSON)
                )

              # Copy to avoid collision with the original df
              results_new <- results |>
                dplyr::select(-result)

              # For each category, create a binary column
              for (cat in categories) {
                results_new[[cat]] <- purrr::map_lgl(
                  results$result,
                  ~ cat %in% .x
                )
              }

              results <- results_new
            }

            # If categorization, write paragraphs
            if (mode == "Categorisatie" && write_paragraphs) {
              # Make list per category, with all texts assigned to that category
              categories_texts <- list()
              if (!assign_multiple_categories) {
                # Can simply group by 'result'
                for (cat in categories) {
                  categories_texts[[cat]] <- results$text[
                    results$result == cat
                  ]
                }
              } else {
                # If multiple categories, we have a binary column per category
                # For each category, get the texts assigned to that category
                for (cat in categories) {
                  categories_texts[[cat]] <- results$text[
                    results[[cat]]
                  ]
                }
              }

              # Exclude categories which don't have supporting texts
              categories_texts <- categories_texts[
                purrr::map_lgl(categories_texts, ~ isTRUE(length(.x) > 0))
              ]

              # Write paragraphs, per category
              progress_secondary$show()
              progress_secondary$set_with_total(
                0,
                length(categories_texts),
                "..."
              )

              # Show streaming panel for paragraph writing (if enabled)
              stream_callback <- NULL
              if (streaming_enabled) {
                llm_stream_async$show()
                stream_callback <- function(token, meta) {
                  llm_stream_async$set(meta$partial_response %||% "")
                  invisible(TRUE)
                }
              }

              paragraphs <- purrr::map(
                seq_along(categories_texts),
                function(i) {
                  # Get category name
                  cat_name <- names(categories_texts)[[i]]
                  cat_texts <- categories_texts[[i]]

                  progress_secondary$set_with_total(
                    i,
                    length(categories_texts),
                    paste0(lang$t("Schrijven over '"), cat_name, "'...")
                  )

                  # Clear streaming panel before this paragraph
                  if (streaming_enabled) {
                    llm_stream_async$clear()
                  }

                  # Write paragraph about the category
                  write_paragraph(
                    texts = cat_texts,
                    topic = cat_name,
                    research_background = research_background,
                    style_prompt = style_prompt,
                    llm_provider = llm_provider,
                    language = lang$get_translation_language(),
                    stream_callback = stream_callback
                  )
                }
              )
              progress_secondary$hide()

              # Add as attribute to the results
              attr(results, "paragraphs") <- paragraphs
            }

            results
          },
          globals = c(
            list(
              llm_provider = models$main,
              texts = texts$preprocessed,
              research_background = research_background(),
              style_prompt = style_prompt(),
              mode = mode(),
              categories = categories$texts(),
              exclusive_categories = categories$exclusive_texts(),
              scoring_characteristic = scoring_characteristic(),
              prompt_category = prompt_category,
              prompt_multi_category = prompt_multi_category,
              prompt_score = prompt_score,
              assign_multiple_categories = assign_multiple_categories(),
              write_paragraph = write_paragraph,
              write_paragraphs = write_paragraphs(),
              get_context_window_size_in_tokens = get_context_window_size_in_tokens,
              tiktoken_load_tokenizer = tiktoken_load_tokenizer,
              count_tokens = count_tokens,
              async_message_printer = async_message_printer,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            log_async_globals(log_ctx),
            send_prompt_with_retries_async_globals()
          ),
          packages = c("tidyprompt", "tidyverse", "glue", "fs", "uuid"),
          seed = NULL
        ) %...>%
          results_df() %...!%
          {
            # Clean up async controllers before error handling
            llm_stream$async$stop()
            llm_stream$hide()

            err_msg <- tryCatch(conditionMessage(.), error = function(e) {
              as.character(.)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(.), collapse = "|")
            log_action(
              "analysis_failed",
              details = sprintf(
                "mode=%s when=%s error_class=%s error=%s",
                mode() %||% "unknown",
                "main processing of categorization/scoring",
                err_class,
                err_msg
              )
            )
            app_error(
              .,
              when = "main processing of categorization/scoring",
              fatal = TRUE,
              lang = lang()
            )
          }
        log_debug(
          "Started async processing for categorization/scoring",
          component = "analysis"
        )
      })

      # Helper function to check if categories are valid
      categories_are_valid <- function() {
        # User must be done editing categories
        if (categories$editing()) {
          shiny::showNotification(
            lang()$t(
              "Je moet eerst de categorieen opslaan voordat je verder kunt gaan."
            ),
            type = "error"
          )
          return(FALSE)
        }

        # User must have at least 2 non-empty categories
        if (categories$unique_non_empty_count() < 2) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 2 categorieen opgeven."),
            type = "error"
          )
          return(FALSE)
        }

        return(TRUE)
      }

      # Helper function to check if scoring characteristic is valid
      scoring_characteristic_is_valid <- function() {
        # User must have at least 1 non-empty scoring characteristic
        if (isTRUE(nchar(scoring_characteristic()) < 1)) {
          shiny::showNotification(
            lang()$t("Geef een karakteristiek op."),
            type = "error"
          )
          return(FALSE)
        }

        return(TRUE)
      }

      ## Topic modelling (onderwerpextractie) ------------------------
      ## >> Topic generation -------------------------------------------
      # Listen for process button click when in topic modelling mode
      # Launch processing
      observeEvent(input$process, {
        if (processing()) {
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

        req(texts$preprocessed)
        req(context_window$text_chunks)
        if (!mode() %in% c("Onderwerpextractie")) {
          return()
        }
        if (!number_of_texts_under_maximum()) {
          return()
        }
        req(isFALSE(context_window$any_fit_problem))
        req(isFALSE(context_window$too_many_chunks))

        started(TRUE)
        # Set processing state
        processing(TRUE)
        analysis_started_at(Sys.time())
        log_analysis_start(
          mode = mode(),
          n_texts = length(texts$preprocessed),
          model = models$main$parameters$model %||% "unknown"
        )

        # Disable button
        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        log_ctx <- log_context_capture(
          is_async = TRUE,
          mode = getOption("app__mode", "unknown")
        )

        future_promise(
          {
            log_context_apply(log_ctx)

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
              length(text_chunks),
              lang$t("...")
            )

            candidate_topics <- tryCatch(
              {
                results <- c()
                for (i in seq_along(text_chunks)) {
                  interrupter$execInterrupts()
                  text_chunk <- text_chunks[[i]]

                  result <- create_candidate_topics(
                    list(text_chunk),
                    research_background,
                    llm_provider_main,
                    language = lang$get_translation_language()
                  )

                  progress_secondary$set_with_total(
                    i,
                    length(text_chunks),
                    paste(result, collapse = ",")
                  )

                  results <- c(results, result)
                }

                results
              },
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
            topics
          },
          globals = c(
            log_async_globals(log_ctx),
            send_prompt_with_retries_async_globals(),
            list(
              create_candidate_topics = create_candidate_topics,
              prompt_candidate_topics = prompt_candidate_topics,
              reduce_topics = reduce_topics,
              prompt_category = prompt_category,
              prompt_multi_category = prompt_multi_category,
              assign_topics = assign_topics,
              llm_provider_main = models$main,
              llm_provider_large = models$large,
              texts = texts$preprocessed,
              research_background = research_background(),
              mode = mode(),
              handle_detailed_error = handle_detailed_error,
              text_chunks = context_window$text_chunks,
              get_context_window_size_in_tokens = get_context_window_size_in_tokens,
              tiktoken_load_tokenizer = tiktoken_load_tokenizer,
              count_tokens = count_tokens,
              async_message_printer = async_message_printer,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter
            )
          ),
          packages = c(
            "tidyprompt",
            "tidyverse",
            "glue",
            "fs",
            "uuid",
            "rlang",
            "stringr"
          ),
          seed = NULL
        ) %...>%
          topics() %...!%
          {
            llm_stream$hide()

            err_msg <- tryCatch(conditionMessage(.), error = function(e) {
              as.character(.)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(.), collapse = "|")
            log_action(
              "analysis_failed",
              details = sprintf(
                "mode=%s when=%s error_class=%s error=%s",
                mode() %||% "unknown",
                "main processing (step 1-2) of topic modelling",
                err_class,
                err_msg
              )
            )
            app_error(
              .,
              when = "main processing (step 1-2) of topic modelling",
              fatal = TRUE,
              lang = lang()
            )
          }

        log_debug(
          "Started async processing for topic modelling (step 1-2)",
          component = "analysis"
        )
      })

      ## >> Topics generated; editing --------------------------------
      # Listen for topics completion
      # Present modal dialog to edit/confirm topics
      ## Updated observeEvent for topics() with Add/Remove buttons in the modal
      observeEvent(topics(), {
        req(topics())
        req(!topics_definitive())

        # Set 'Onbekend/niet van toepassing' as exclusive topic (if present)
        if (lang()$t("Onbekend/niet van toepassing") %in% topics()) {
          exclusive_topics(c(
            exclusive_topics(),
            lang()$t("Onbekend/niet van toepassing")
          ))
        }
        # Remove any exclusive topics which may ont be present in the topics
        exclusive_topics(
          exclusive_topics()[exclusive_topics() %in% topics()]
        )

        # If no human in the loop, auto-confirm
        if (!isTRUE(human_in_the_loop())) {
          topics_definitive(TRUE)
          return()
        }

        # Human in the loop, so launch topic editing...
        progress_primary$set_with_total(
          2.5,
          5,
          lang()$t("Onderwerpen bewerken...")
        )

        # Launch modal dialog to edit topics
        edited_topics <- edit_topics_server(
          "edit_topics",
          topics = topics,
          exclusive_topics = exclusive_topics,
          llm_provider = models$large,
          research_background = research_background,
          assign_multiple_categories = assign_multiple_categories,
          lang = lang
        )

        # When edited_topics is no longer NULL,
        #   set the topics and mark them as definitive
        edited_topics_observer <- observe(
          {
            req(edited_topics())
            topics(edited_topics())
            topics_definitive(TRUE)
            edited_topics_observer$suspend()
          },
          suspended = FALSE,
          autoDestroy = TRUE
        )
      })

      ## >> Topics definitive; assigning -----------------------------
      # Listen for definitive topics
      # Launch processing for assigning topics to paragraphs
      observeEvent(topics_definitive(), {
        if (!isTRUE(topics_definitive())) {
          return()
        }
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

        log_ctx <- log_context_capture(
          is_async = TRUE,
          mode = getOption("app__mode", "unknown")
        )

        future_promise(
          {
            log_context_apply(log_ctx)

            # Step 4: Assign topics
            # Writing progress on secondary progress file
            texts_with_topics <- tryCatch(
              {
                results <- tibble::tibble(
                  text = character(),
                  result = character()
                )

                for (i in seq_along(texts)) {
                  interrupter$execInterrupts()
                  text <- texts[[i]]
                  result <- assign_topics(
                    c(text),
                    topics,
                    research_background,
                    llm_provider,
                    assign_multiple_categories = assign_multiple_categories,
                    exclusive_topics = exclusive_topics
                  )
                  result <- result$result

                  progress_secondary$set_with_total(
                    i,
                    length(texts),
                    text
                  )

                  # Append to results
                  results <- dplyr::bind_rows(
                    results,
                    tibble::tibble(text = text, result = as.character(result))
                  )
                }
                progress_secondary$hide()

                # If multiple categories, convert from JSON array string to
                #   multiple binary columns
                if (assign_multiple_categories) {
                  # Parse the JSON array strings into vector-column
                  results <- results |>
                    dplyr::mutate(
                      result = purrr::map(result, jsonlite::fromJSON)
                    )

                  # Copy to avoid collision with the original df
                  results_new <- results |>
                    dplyr::select(-result)

                  # For each category, create a binary column
                  for (cat in topics) {
                    results_new[[cat]] <- purrr::map_lgl(
                      results$result,
                      ~ cat %in% .x
                    )
                  }

                  results <- results_new
                }

                results
              },
              error = handle_detailed_error("Topic assignment")
            )

            ## Step 5: Write paragraphs about the topics
            log_info("Step 5/5: Writing paragraphs...", component = "analysis")
            progress_primary$set_with_total(
              4,
              5,
              lang$t("Rapport schrijven...")
            )

            if (write_paragraphs) {
              paragraphs <- tryCatch(
                {
                  # Make list per topic, with all texts assigned to that topic
                  topics_texts_list <- list()
                  if (!assign_multiple_categories) {
                    # Can simply group by 'result'
                    for (topic in texts_with_topics$result) {
                      topics_texts_list[[topic]] <- texts_with_topics$text[
                        texts_with_topics$result == topic
                      ]
                    }
                  } else {
                    # If multiple categories, we have a binary column per category
                    # For each category, get the texts assigned to that category
                    for (cat in topics) {
                      topics_texts_list[[cat]] <- texts_with_topics$text[
                        texts_with_topics[[cat]]
                      ]
                    }
                  }

                  # Exclude topics which don't have supporting texts
                  topics_texts_list <- topics_texts_list[
                    purrr::map_lgl(topics_texts_list, ~ isTRUE(length(.x) > 0))
                  ]

                  # Write paragraphs, per topic
                  progress_secondary$show()
                  progress_secondary$set_with_total(
                    0,
                    length(topics_texts_list),
                    "..."
                  )

                  # Show streaming panel for paragraph writing (if enabled)
                  stream_callback <- NULL
                  if (streaming_enabled) {
                    llm_stream_async$show()
                    stream_callback <- function(token, meta) {
                      llm_stream_async$set(meta$partial_response %||% "")
                      invisible(TRUE)
                    }
                  }

                  paragraphs <- purrr::map(
                    seq_along(topics_texts_list),
                    function(i) {
                      interrupter$execInterrupts()
                      topic_name <- names(topics_texts_list)[[i]]
                      topic_texts <- topics_texts_list[[i]]

                      progress_secondary$set_with_total(
                        i,
                        length(topics_texts_list),
                        paste0(
                          lang$t("Schrijven over '"),
                          topic_name,
                          "'..."
                        )
                      )

                      # Clear streaming panel before this paragraph
                      if (streaming_enabled) {
                        llm_stream_async$clear()
                      }

                      paragraph <- write_paragraph(
                        texts = topic_texts,
                        topic = topic_name,
                        research_background = research_background,
                        style_prompt = style_prompt,
                        llm_provider = llm_provider,
                        language = lang$get_translation_language(),
                        stream_callback = stream_callback
                      )

                      paragraph
                    }
                  )

                  paragraphs
                },
                error = handle_detailed_error("Topic report generation")
              )

              progress_secondary$hide()

              # Add as attribute to the result
              attr(texts_with_topics, "paragraphs") <- paragraphs
            }

            texts_with_topics
          },
          globals = c(
            send_prompt_with_retries_async_globals(),
            list(
              topics = topics(),
              create_candidate_topics = create_candidate_topics,
              prompt_candidate_topics = prompt_candidate_topics,
              reduce_topics = reduce_topics,
              prompt_category = prompt_category,
              prompt_multi_category = prompt_multi_category,
              assign_topics = assign_topics,
              write_paragraph = write_paragraph,
              llm_provider = models$main,
              texts = texts$preprocessed,
              research_background = research_background(),
              style_prompt = style_prompt(),
              mode = mode(),
              assign_multiple_categories = assign_multiple_categories(),
              write_paragraphs = write_paragraphs(),
              handle_detailed_error = handle_detailed_error,
              get_context_window_size_in_tokens = get_context_window_size_in_tokens,
              tiktoken_load_tokenizer = tiktoken_load_tokenizer,
              count_tokens = count_tokens,
              async_message_printer = async_message_printer,
              lang = lang(),
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              exclusive_topics = exclusive_topics(),
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            log_async_globals(log_ctx)
          ),
          packages = c("tidyprompt", "tidyverse", "glue", "fs", "uuid"),
          seed = NULL
        ) %...>%
          results_df() %...!%
          {
            # Clean up async controllers before error handling
            llm_stream$async$stop()
            llm_stream$hide()

            err_msg <- tryCatch(conditionMessage(.), error = function(e) {
              as.character(.)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(.), collapse = "|")
            log_action(
              "analysis_failed",
              details = sprintf(
                "mode=%s when=%s error_class=%s error=%s",
                mode() %||% "unknown",
                "main processing (step 3-4) of topic modelling",
                err_class,
                err_msg
              )
            )
            app_error(
              .,
              when = "main processing (step 3-4) of topic modelling",
              fatal = TRUE,
              lang = lang()
            )
          }
        log_debug(
          "Started async processing for topic modelling (step 3-4)",
          component = "analysis"
        )
      })

      ## Markeren ----------------------------------------------------
      codes_are_valid <- function() {
        # User must be done editing codes
        if (codes$editing()) {
          shiny::showNotification(
            lang()$t(
              "Je moet eerst de codes opslaan voordat je verder kunt gaan."
            ),
            type = "error"
          )
          return(FALSE)
        }

        # User must have at least 1 non-empty code
        if (codes$unique_non_empty_count() < 1) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 1 code opgeven."),
            type = "error"
          )
          return(FALSE)
        }

        # Codes must not be duplicated
        if (length(unique(codes$texts())) < length(codes$texts())) {
          shiny::showNotification(
            lang()$t("Codes moeten uniek zijn."),
            type = "error"
          )
          return(FALSE)
        }

        return(TRUE)
      }

      # Listen for process button click when in marking mode
      # Launch processing
      observeEvent(input$process, {
        if (processing()) {
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

        req(texts$preprocessed)
        if (!mode() %in% c("Markeren")) {
          return()
        }
        if (!number_of_texts_under_maximum()) {
          return()
        }
        if (!codes_are_valid()) {
          return()
        }
        req(isFALSE(context_window$any_fit_problem))
        req(context_window$max_tokens)
        req(context_window$overlap)

        started(TRUE)
        # Set processing state
        processing(TRUE)
        analysis_started_at(Sys.time())
        log_analysis_start(
          mode = mode(),
          n_texts = length(texts$preprocessed),
          model = models$main$parameters$model %||% "unknown"
        )
        # Set initial progress
        progress_primary$set_with_total(0, length(texts$preprocessed), "...")

        # Disable button
        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        log_ctx <- log_context_capture(
          is_async = TRUE,
          mode = getOption("app__mode", "unknown")
        )

        future_promise(
          {
            log_context_apply(log_ctx)

            mark_texts(
              texts = texts,
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
              streaming_enabled = streaming_enabled
            )
          },
          globals = c(
            list(
              llm_provider = models$main,
              texts = texts$preprocessed,
              research_background = research_background(),
              style_prompt = style_prompt(),
              codes = codes$texts(),
              mark_texts = mark_texts,
              mark_text_prompt = mark_text_prompt,
              get_context_window_size_in_tokens = get_context_window_size_in_tokens,
              tiktoken_load_tokenizer = tiktoken_load_tokenizer,
              count_tokens = count_tokens,
              async_message_printer = async_message_printer,
              lang = lang(),
              semchunk_load_chunker = semchunk_load_chunker,
              progress_primary = progress_primary$async,
              progress_secondary = progress_secondary$async,
              interrupter = interrupter,
              write_paragraph = write_paragraph,
              write_paragraphs = write_paragraphs(),
              text_size_tokens = context_window$max_tokens,
              overlap_size_tokens = context_window$overlap,
              find_matches = find_matches,
              normalize_with_map = normalize_with_map,
              best_literal_substring = best_literal_substring,
              fuzzy_threshold = fuzzy_threshold,
              normalize_for_dist = normalize_for_dist,
              llm_stream_async = llm_stream$async,
              streaming_enabled = getOption("paragraph_streaming", TRUE) &&
                isTRUE(models$main$parameters$stream)
            ),
            log_async_globals(log_ctx),
            send_prompt_with_retries_async_globals()
          ),
          packages = c(
            "tidyprompt",
            "glue",
            "rlang",
            "stringr",
            "dplyr",
            "purrr"
          ),
          seed = NULL
        ) %...>%
          results_df() %...!%
          {
            # Clean up async controllers before error handling
            llm_stream$async$stop()
            llm_stream$hide()

            err_msg <- tryCatch(conditionMessage(.), error = function(e) {
              as.character(.)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(.), collapse = "|")
            log_action(
              "analysis_failed",
              details = sprintf(
                "mode=%s when=%s error_class=%s error=%s",
                mode() %||% "unknown",
                "main processing of marking",
                err_class,
                err_msg
              )
            )
            app_error(
              .,
              when = "main processing of marking",
              fatal = TRUE,
              lang = lang()
            )
          }

        NULL # Avoid blocking the app with the promise
      })

      # Post-processing: interrater-reliability, download files ------
      # Listen for processing completion
      # Join results with original texts
      # Launch interrater reliability module if required
      observeEvent(results_df(), {
        req(results_df())
        log_debug(
          sprintf("Results received: n_rows=%d", nrow(results_df())),
          component = "analysis"
        )

        # Join results of preprocessed texts to raw texts
        df <- texts$df
        df <- df |>
          dplyr::left_join(
            results_df(),
            by = dplyr::join_by("preprocessed" == "text")
          ) |>
          # Remove preprocessed column
          dplyr::select(-preprocessed) |>
          # Rename 'raw' to 'text'
          dplyr::rename(text = raw)
        # Transfer attribute containing paragraphs
        attr(df, "paragraphs") <- attr(results_df(), "paragraphs")

        # Store final results df
        final_results_df(df)

        # Verify that df actually has results
        # (sometimes we have API failure, then result/topic contains NA values)
        if (anyNA(df$result)) {
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

        # Update UI to show finished processing
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
          all_categories <-
            if (exists("all_categories")) {
              all_categories
            } else if (mode() == "Categorisatie") {
              categories$texts()
            } else if (mode() == "Onderwerpextractie") {
              topics()
            } else {
              NULL
            }

          if (
            mode() %in%
              c("Categorisatie", "Onderwerpextractie") &&
              length(unique(all_categories)) < 2
          ) {
            shiny::showNotification(paste0(
              lang()$t("Niet meer dan 1 categorie aanwezig in data; "),
              lang()$t(" kan geen interrater-reliability berekenen")
            ))
            irr_done(TRUE)
            return()
          }

          irr <- interrater_server(
            id = "rater_modal",
            rating_data = df, # Use the prepared data
            text_col = "text",
            all_categories = all_categories,
            mode = mode(),
            assign_multiple_categories = assign_multiple_categories(),
            lang = lang
          )
          irr$start()

          irr_completion_observer <- observe(
            {
              if (irr$done) {
                irr_done(TRUE)
                irr_result(irr$result)
              }
            },
            suspended = FALSE,
            autoDestroy = TRUE
          )
        } else {
          irr_done(TRUE)
        }
      })

      # Listen for inter-rater reliability completion
      # Prepare files for download
      observeEvent(
        irr_done(),
        {
          if (!isTRUE(irr_done())) {
            return()
          }
          result_list <- create_result_list()

          # If any in 'result_list$df$result' are NA, show a warning
          error <- anyNA(result_list$df$result)
          if (error) {
            app_error(
              "Results contain NA values; processing failed",
              when = "after inter-rater reliability completion",
              fatal = TRUE,
              lang = lang()
            )
          }

          # Set preparing download state (to show loading animation)
          preparing_download(TRUE)

          log_action(
            "results_download_preparing",
            details = sprintf(
              "mode=%s n_texts=%d uuid=%s",
              mode() %||% "unknown",
              nrow(result_list$df %||% data.frame()),
              uuid
            )
          )

          # Generate files async
          future(
            {
              # Generate files
              excel_file <- create_result_excel(result_list)

              if (
                mode %in%
                  c(
                    "Categorisatie",
                    "Scoren",
                    "Onderwerpextractie",
                    "Markeren"
                  )
              ) {
                rmarkdown <- create_result_rmarkdown(result_list)
              }

              # Check if all files exist, if not,
              #   wait up to 10s
              #   and then throw an error
              for (i in 1:10) {
                if (
                  mode %in%
                    c(
                      "Categorisatie",
                      "Scoren",
                      "Onderwerpextractie",
                      "Markeren"
                    )
                ) {
                  if (file.exists(excel_file) && file.exists(rmarkdown)) {
                    break
                  }
                } else {
                  if (file.exists(excel_file)) {
                    break
                  }
                }
                Sys.sleep(1)
              }

              if (!file.exists(excel_file)) {
                stop("Excel file not found, no error available")
              }
              if (grepl("\\.txt$", excel_file)) {
                stop(paste0(
                  "Excel file generation error: ",
                  readLines(excel_file, warn = FALSE)
                ))
              }

              if (
                mode %in%
                  c(
                    "Categorisatie",
                    "Scoren",
                    "Onderwerpextractie",
                    "Markeren"
                  )
              ) {
                if (!file.exists(rmarkdown)) {
                  stop("Rmarkdown file not found, no error available")
                }
                if (grepl("\\.txt$", rmarkdown)) {
                  stop(paste0(
                    "Rmarkdown file generation error: ",
                    readLines(rmarkdown, warn = FALSE)
                  ))
                }
              }

              files <- c(excel_file)
              if (
                mode %in%
                  c(
                    "Categorisatie",
                    "Scoren",
                    "Onderwerpextractie",
                    "Markeren"
                  )
              ) {
                files <- c(files, rmarkdown)
              }

              # Zip them
              zip_path <- file.path(
                tempdir,
                paste0(uuid::UUIDgenerate(), "_results.zip")
              )
              zip::zipr(
                zipfile = zip_path,
                files = files,
                root = dirname(excel_file)
              )
              zip_path
            },
            globals = list(
              mode = mode(),
              create_result_excel = create_result_excel,
              create_result_rmarkdown = create_result_rmarkdown,
              result_list = result_list,
              tempdir = tempdir(),
              lang = lang()
            ),
            seed = NULL
          ) %...>%
            zip_file() %...!%
            {
              app_error(
                .,
                when = "preparing download (excel, rmarkdown, zip)",
                fatal = TRUE,
                lang = lang()
              )
            }

          shinyjs::hide("process")
        },
        suspended = FALSE,
        autoDestroy = TRUE
      )

      # Loading animation during download preparation
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
          # Download & restart button
          tagList(
            uiOutput(ns("download_button")),
            uiOutput(ns("restart_button"))
          )
        }
      })

      # Listen for when download (zip file) is ready
      # Present download button
      # Present restart button
      observeEvent(zip_file(), {
        if (is.null(zip_file())) {
          return()
        }

        # End-to-end duration: from process click to download-ready
        started_at <- analysis_started_at()
        if (!is.null(started_at)) {
          duration_total_secs <- as.numeric(difftime(
            Sys.time(),
            started_at,
            units = "secs"
          ))

          preprocessed_texts <- tryCatch(
            {
              txt <- texts$preprocessed %||% character(0)
              as.character(txt)
            },
            error = function(e) character(0)
          )

          n_texts_total <- length(preprocessed_texts)

          total_chars <- tryCatch(
            sum(
              nchar(preprocessed_texts, type = "chars", allowNA = TRUE),
              na.rm = TRUE
            ),
            error = function(e) NA_real_
          )

          secs_per_text <- if (
            isTRUE(!is.na(n_texts_total)) && isTRUE(n_texts_total > 0)
          ) {
            duration_total_secs / n_texts_total
          } else {
            NA_real_
          }

          secs_per_1k_chars <- if (
            isTRUE(!is.na(total_chars)) && isTRUE(total_chars > 0)
          ) {
            (duration_total_secs / total_chars) * 1000
          } else {
            NA_real_
          }

          log_info(
            sprintf(
              paste0(
                "Analysis total duration (click->download-ready): ",
                "mode=%s, n_texts_preprocessed=%s, total_chars_preprocessed=%s, uuid=%s, ",
                "duration=%.1fs, avg=%.3fs/text, avg=%.3fs/1k_chars"
              ),
              mode() %||% "unknown",
              as.character(n_texts_total),
              as.character(total_chars),
              uuid,
              duration_total_secs,
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
            nrow(results_df()),
            uuid,
            as.character(zip_bytes)
          ),
          component = "output"
        )

        # Create download handler
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

        # Render download button (this triggers the spinner while rendering)
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

        # Render restart button
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

        # Set 'success' to TRUE; process is done
        success(TRUE)
      })

      # Restart button listener
      # Launches modal dialog to confirm restart
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

      # Confirm restart button listener
      # Reloads the app
      observeEvent(input$confirm_restart, {
        log_action(
          "analysis_restart_confirmed",
          details = sprintf("mode=%s", mode() %||% "unknown")
        )
        log_action("analysis_restart", details = mode())
        removeModal()
        session$reload()
      })

      # Helper functions ---------------------------------------------
      # Helper function to create a list with all the results, including metadata
      create_result_list <- function(df) {
        result_list <- list(
          df = final_results_df(),
          time = Sys.time(),
          uuid = uuid,
          mode = mode(),
          research_background = research_background(),
          style_prompt = style_prompt(),
          irr = irr_result(),
          language = lang()$get_translation_language()
        )

        if (mode() == "Categorisatie") {
          result_list$model <- models$main$parameters$model
          result_list$categories <- categories$texts()
          result_list$exclusive_categories <- categories$exclusive_texts()
          result_list$assign_multiple_categories <- assign_multiple_categories()
          result_list$prompt <- prompt_category(
            text = lang()$t("<< TEKST >>"),
            research_background = research_background(),
            categories = categories$texts()
          ) |>
            tidyprompt::construct_prompt_text()
          result_list$human_in_the_loop <- human_in_the_loop()
          result_list$write_paragraphs <- write_paragraphs()
        }

        if (mode() == "Scoren") {
          result_list$model <- models$main$parameters$model
          result_list$scoring_characteristic <- scoring_characteristic()
          result_list$prompt <- prompt_score(
            text = lang()$t("<< TEKST >>"),
            research_background = research_background(),
            scoring_characteristic = scoring_characteristic()
          ) |>
            tidyprompt::construct_prompt_text()
        }

        if (mode() == "Onderwerpextractie") {
          result_list$model <- models$main$parameters$model
          result_list$model_reductie <- models$large$parameters$model
          result_list$topics <- topics()
          result_list$exclusive_topics <- exclusive_topics()
          result_list$assign_multiple_categories <- assign_multiple_categories()
          result_list$write_paragraphs <- write_paragraphs()

          # Add chunking information
          chunking_parameters <- tibble::tibble(
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
          result_list$chunking_parameters <- chunking_parameters
        }

        if (mode() == "Markeren") {
          result_list$model <- models$main$parameters$model
          result_list$codes <- codes$texts()
          result_list$write_paragraphs <- write_paragraphs()
          result_list$prompt <- mark_text_prompt(
            text = lang()$t("<< TEKST >>"),
            research_background = research_background(),
            code = "<< CODE >>"
          ) |>
            tidyprompt::construct_prompt_text()
          result_list$text_size_tokens <- context_window$max_tokens
          result_list$overlap_size_tokens <- context_window$overlap
        }

        # Transfer paragraphs if present as attribute
        if (!is.null(attr(final_results_df(), "paragraphs"))) {
          result_list$paragraphs <- attr(final_results_df(), "paragraphs")
        }
        # Verify that paragraphs are present if write_paragraphs is TRUE
        if (isTRUE(write_paragraphs()) && is.null(result_list$paragraphs)) {
          app_error(
            "Paragraphs were requested to be written, but no paragraphs found",
            when = "creating result list, checking paragraph presence",
            fatal = TRUE,
            lang = lang()
          )
        }

        return(result_list)
      }

      # Helper function to turn result_list into Excel file
      create_result_excel <- function(result_list) {
        excel_file <- file.path(
          tempdir(),
          paste0("data_", result_list$uuid, ".xlsx")
        )

        error_file <- file.path(
          tempdir(),
          paste0("data_", result_list$uuid, "_error.txt")
        )

        safe_write_xlsx <- function(result_list, excel_file) {
          sheets <- lapply(result_list, function(x) {
            if (is.null(x)) {
              return(NULL)
            } else if (length(x) == 1 && is.atomic(x) && is.na(x)) {
              return(data.frame(value = NA, stringsAsFactors = FALSE))
            } else if (is.data.frame(x)) {
              return(x)
            } else if (is.atomic(x) || is.character(x)) {
              return(data.frame(value = x, stringsAsFactors = FALSE))
            } else if (is.list(x)) {
              df <- tryCatch(as.data.frame(x), error = function(e) NULL)
              if (is.null(df)) {
                warning(
                  "List could not be coerced to data.frame; capturing printed output instead."
                )
                captured <- capture.output(print(x))
                return(data.frame(
                  captured_output = captured,
                  stringsAsFactors = FALSE
                ))
              } else {
                return(df)
              }
            } else {
              warning(
                "Unsupported element type; capturing printed output instead."
              )
              captured <- capture.output(print(x))
              return(data.frame(
                captured_output = captured,
                stringsAsFactors = FALSE
              ))
            }
          })

          names(sheets) <- names(result_list)
          # Remove NULL elements
          sheets <- Filter(Negate(is.null), sheets)

          writexl::write_xlsx(x = sheets, path = excel_file)
        }

        result <- tryCatch(
          {
            safe_write_xlsx(result_list, excel_file)
            excel_file # Success: return path to .xlsx
          },
          error = function(e) {
            # Error: write message into .txt file
            writeLines(
              paste("Error during Excel creation:", conditionMessage(e)),
              con = error_file
            )
            error_file # Return path to .txt
          }
        )

        return(result)
      }

      # Helper function to turn result_list into HTML output via Rmarkdown
      create_result_rmarkdown <- function(result_list) {
        output_file_html <- file.path(
          tempdir(),
          paste0("report_", result_list$uuid, ".html")
        )

        output_file_txt <- file.path(
          tempdir(),
          paste0("report_", result_list$uuid, "_error.txt")
        )

        result <- tryCatch(
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
            # Capture detailed stack trace and message
            error_details <- paste(
              "Error during rendering:",
              conditionMessage(e),
              "\n\n--- Traceback ---\n",
              paste(capture.output(traceback()), collapse = "\n"),
              "\n\n--- Full Error Object ---\n",
              paste(capture.output(print(e)), collapse = "\n")
            )

            # Write error details to file
            writeLines(error_details, con = output_file_txt)

            output_file_txt
          }
        )

        return(result)
      }

      # Progress bars ------------------------------------------------
      progress_primary <- progress_bar_server("progress_primary")
      progress_secondary <- progress_bar_server(
        "progress_secondary",
        initially_hidden = TRUE
      )

      # LLM Streaming ------------------------------------------------
      llm_stream <- llm_streaming_server(
        "llm_stream",
        initially_hidden = TRUE
      )

      # Processing button --------------------------------------------
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

        # Count how many preprocessed texts we have
        preproc <- texts$preprocessed
        n_pre <- if (is.null(preproc)) 0 else length(preproc)

        # Build the label based on mode
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
          # fallback
          paste0(lang()$t("Verwerk"), " (", n_pre, ")")
        )

        # Disable if no texts OR if there is a context-window fit problem
        disable_flag <- (n_pre == 0) ||
          isTRUE(context_window$any_fit_problem) ||
          isTRUE(context_window$too_many_chunks)

        actionButton(
          ns("process"),
          label = btn_label,
          class = "btn btn-primary btn-lg snake-btn",
          disabled = disable_flag
        )
      })

      # Process button update ----------------------------------------
      # Upon selecting mode, change text in process button
      observe({
        req(mode())
        if (mode() == "Categorisatie") {
          updateActionButton(
            session,
            "process",
            label = paste0(
              lang()$t("Categoriseer"),
              " (",
              length(texts$preprocessed),
              ")"
            )
          )
        } else if (mode() == "Scoren") {
          updateActionButton(
            session,
            "process",
            label = paste0(
              lang()$t("Scoreer"),
              " (",
              length(texts$preprocessed),
              ")"
            )
          )
        } else if (mode() == "Onderwerpextractie") {
          updateActionButton(
            session,
            "process",
            label = paste0(
              lang()$t("Extraheer"),
              " (",
              length(texts$preprocessed),
              ")"
            )
          )
        }
      })

      # Interruption -------------------------------------------------
      # Interrupter can stop async processing if user quits
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
        # Not preparing download
        req(!isTRUE(preparing_download()))
        # Not zip file ready
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

      # Cancel button observer
      observeEvent(input$cancel, {
        req(isTRUE(processing()))

        log_action(
          "analysis_cancel_clicked",
          details = sprintf("mode=%s", mode() %||% "unknown")
        )

        # Show modal dialog to confirm cancellation
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

      # Confirm cancel button observer
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

      processing_fn <- function() {
        processing()
      }

      attr(processing_fn, "has_started") <- reactive({
        isTRUE(started()) ||
          isTRUE(processing()) ||
          !is.null(results_df()) ||
          isTRUE(preparing_download()) ||
          !is.null(zip_file())
      })

      attr(processing_fn, "has_results") <- reactive({
        !is.null(zip_file())
      })

      return(processing_fn)
    }
  )
}


# 3 Helpers ---------------------------------------------------------------

# In other places this helper function can be called to efficiently
# disable multiple inputs when processing is active

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


# 4 Example/development usage ---------------------------------------------

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(tidyprompt)
  library(bslib)
  library(bsicons)

  ui <- bslib::page_fluid(
    shinyjs::useShinyjs(),
    css_js_head(),
    mode_ui("mode"),
    human_in_the_loop_toggle_ui("hitl_toggle"),
    interrater_toggle_ui("interrater_toggle"),
    processing_ui("processing")
  )

  server <- function(input, output, session) {
    mode <- mode_server(
      "mode",
      processing
    )
    human_in_the_loop <- human_in_the_loop_toggle_server(
      "hitl_toggle",
      processing,
      mode
    )
    interrater_reliability_toggle <- interrater_toggle_server(
      "interrater_toggle",
      processing
    )
    llm_provider_rv <- llm_provider_server(
      "llm_provider",
      processing,
      preconfigured_llm_provider = tidyprompt::llm_provider_openai(),
      preconfigured_main_models = c("gpt-4o-mini", "gpt-3.5-turbo"),
      preconfigured_large_models = c("gpt-4o", "o3")
    )
    models <- model_server(
      "model",
      llm_provider = llm_provider
    )
    processing <- processing_server(
      "processing",
      mode = mode,
      interrater_reliability_toggle = reactiveVal(FALSE),
      texts = reactiveValues(
        preprocessed = c(
          "i hate this product",
          "i love this yellow product!!",
          "no opinion. but it has a red colour"
        ),
        df = data.frame(
          raw = c(
            "i hate this product, call me: +31 6 12345678",
            "i love this yellow product!!",
            "no opinion. but it has a red colour"
          ),
          preprocessed = c(
            "i hate this product",
            "i love this yellow product!!",
            "no opinion. but it has a red colour"
          )
        )
      ),
      llm_provider_rv = llm_provider_rv,
      models = models,
      categories = list(
        texts = reactiveVal(c(
          "positive review",
          "negative review",
          "neutral review",
          "mentions colour"
        )),
        editing = reactiveVal(FALSE),
        unique_non_empty_count = reactiveVal(3)
      ),
      scoring_characteristic = reactiveVal("positive sentiment"),
      research_background = reactiveVal(
        "We have collected consumer reviews of our product."
      ),
      human_in_the_loop = human_in_the_loop
    )
  }

  shiny::shinyApp(ui, server)
}
