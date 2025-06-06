#### 1 Processing UI & server ####

# Responsible for launching the process & showing progress
# Will also stop the app and return the results once done
# See start of moduleServer for more details about the process

processing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    progress_bar_ui("progress_primary", visible = TRUE),
    progress_bar_ui("progress_secondary", visible = FALSE),
    br(),
    div(
      class = "text-center",
      uiOutput(ns("process_button"))
    ),
    uiOutput(ns("download_ui"))
  )
}


processing_server <- function(
  id,
  mode,
  interrater_reliability_toggle,
  texts,
  llm_provider_rv,
  models,
  categories,
  scoring_characteristic,
  research_background,
  human_in_the_loop = reactiveVal(TRUE),
  assign_multiple_categories = reactiveVal(TRUE),
  write_paragraphs = reactiveVal(TRUE),
  context_window,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  ns <- NS(id)

  moduleServer(
    id,
    function(input, output, session) {
      #### Processing state management ####

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
      #   topics: reactiveVal to store the topics
      #   topics_table_data: reactiveVal to store the topics table data;
      #     shown during editing of topics
      #   topics_definitive: reactiveVal to keep track of whether the topics are definitive
      #     Is false when the user is editing the topics, otherwise true
      #   reduction_in_progress: reactiveVal to keep track of whether the topic reduction
      #     is in progress; happens when the user clicks the "Reduce again" button
      #     during the topic editing modal
      #   rereduced_topics: reactiveVal to store the re-reduced topics
      #     This is used to update the topics table when the re-reduction is done
      processing <- reactiveVal(FALSE)
      results_df <- reactiveVal(NULL)
      final_results_df <- reactiveVal(NULL)
      irr_done <- reactiveVal(FALSE)
      irr_result <- reactiveVal(NULL)
      preparing_download <- reactiveVal(NULL)
      zip_file <- reactiveVal(NULL)
      topics <- reactiveVal(NULL)
      topics_table_data <- reactiveVal(NULL)
      topics_definitive <- reactiveVal(FALSE)
      reduction_in_progress <- reactiveVal(FALSE)
      rereduced_topics <- reactiveVal(NULL)

      # UUID for the current processing task
      uuid <- uuid::UUIDgenerate()

      # Progress files for primary and secondary progress bars
      dir.create("progress", showWarnings = FALSE)
      dir.create("progress_secondary", showWarnings = FALSE)
      progress_file <- file.path("progress", paste0(uuid, ".txt"))
      progress_file_secondary <- file.path(
        "progress_secondary",
        paste0(uuid, ".txt")
      )

      #### Launch processing ####

      # Launch processing when button is clicked;
      #   set reactive values to keep track of processing state
      #   and store the UUID of the current processing task
      #   Prevent multiple processing tasks from running at the same time
      #   Run asynchronous processing using future_promise
      #     (to prevent blocking the Shiny app)

      # Helper to check if number of texts is under maximum
      # TODO: set different maximum, or set limits per user?
      number_of_texts_under_maximum <- function(
        maximum = getOption("processing___max_texts", 3000)
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

      ###### Categorisation & scoring ######

      # Listen for process button click when in categorization or scoring mode
      # Launch processing
      observeEvent(input$process, {
        if (processing()) return()
        req(texts$preprocessed)
        if (!mode() %in% c("Categorisatie", "Scoren")) return()
        if (!number_of_texts_under_maximum()) return()
        if (mode() == "Categorisatie" && !categories_are_valid()) return()
        if (mode() == "Scoren" && !scoring_characteristic_is_valid()) return()
        req(isFALSE(context_window$any_fit_problem))

        # Set processing state
        processing(TRUE)
        # Set initial progress
        write_progress(
          0,
          length(texts$preprocessed),
          "...",
          progress_file
        )
        # Set model
        llm_provider <- llm_provider_rv$llm_provider$clone()
        llm_provider$parameters$model <- models$main
        # Disable button
        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        future_promise(
          {
            results <- vector("list", length(texts))

            for (i in seq_along(texts)) {
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
                    categories = categories
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

              write_progress(
                i,
                length(texts),
                text,
                progress_file
              )

              if (is.na(result)) break
            }

            write_progress(
              length(texts),
              length(texts),
              lang$t("Alle teksten zijn geanalyseerd..."),
              progress_file
            )

            # Turn into data frame
            results <- unlist(results)
            if (any(is.na(results))) results <- rep(NA, length(texts))
            results <- data.frame(
              text = texts,
              result = results,
              stringsAsFactors = FALSE
            )

            # If multiple categories, convert from JSON array string to
            #   multiple binary columns
            if (mode == "Categorisatie" & assign_multiple_categories) {
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
            if (mode == "Categorisatie" & write_paragraphs) {
              write_progress(
                0,
                0,
                lang$t("Rapport schrijven..."),
                progress_file_secondary
              )

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
                    results[[cat]] == TRUE
                  ]
                }
              }

              # Exclude categories which don't have supporting texts
              categories_texts <- categories_texts[
                purrr::map_lgl(categories_texts, ~ isTRUE(length(.x) > 0))
              ]

              # Write paragraphs, per category
              paragraphs <- purrr::map(
                seq_along(categories_texts),
                function(i) {
                  # Get category name
                  cat_name <- names(categories_texts)[[i]]
                  cat_texts <- categories_texts[[i]]

                  write_progress(
                    i,
                    length(categories_texts),
                    paste0(lang$t("Schrijven over '"), cat_name, "'..."),
                    progress_file_secondary
                  )

                  # Write paragraph about the category
                  write_paragraph(
                    texts = cat_texts,
                    topic = cat_name,
                    research_background = research_background,
                    llm_provider = llm_provider,
                    language = lang$get_translation_language()
                  )
                }
              )

              # Add as attribute to the results
              attr(results, "paragraphs") <- paragraphs
            }

            results
          },
          globals = list(
            write_progress = write_progress,
            llm_provider = llm_provider,
            texts = texts$preprocessed,
            research_background = research_background(),
            mode = mode(),
            categories = categories$texts(),
            scoring_characteristic = scoring_characteristic(),
            progress_file = progress_file,
            progress_file_secondary = progress_file_secondary,
            prompt_category = prompt_category,
            prompt_multi_category = prompt_multi_category,
            prompt_score = prompt_score,
            assign_multiple_categories = assign_multiple_categories(),
            write_paragraph = write_paragraph,
            send_prompt_with_retries = send_prompt_with_retries,
            write_paragraphs = write_paragraphs(),
            get_context_window_size_in_tokens = get_context_window_size_in_tokens,
            lang = lang()
          ),
          packages = c("tidyprompt", "tidyverse", "glue", "fs", "uuid")
        ) %...>%
          results_df() %...!%
          {
            app_error(
              .,
              when = "main processing of categorization/scoring",
              fatal = TRUE,
              lang = lang()
            )
          }
        print("Started async processing for categorization/scoring")
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

      ###### Topic modelling (onderwerpextractie) #####

      ####### >> Topic generation ####

      # Listen for process button click when in topic modelling mode
      # Launch processing
      observeEvent(input$process, {
        if (processing()) return()
        req(texts$preprocessed)
        req(context_window$text_chunks)
        if (!mode() %in% c("Onderwerpextractie")) return()
        if (!number_of_texts_under_maximum()) return()
        req(isFALSE(context_window$any_fit_problem))
        req(isFALSE(context_window$too_many_chunks))

        # Set processing state
        processing(TRUE)
        # Set models
        llm_provider_main <- llm_provider_rv$llm_provider$clone()
        llm_provider_main$parameters$model <- models$main
        llm_provider_large <- llm_provider_rv$llm_provider$clone()
        llm_provider_large$parameters$model <- models$large
        # Disable button
        shinyjs::disable("process")
        shinyjs::addClass("process", "loading")

        future_promise(
          {
            # Step 1: Generate candidate topics
            write_progress(
              1,
              5,
              lang$t("Onderwerpen genereren..."),
              progress_file
            )
            candidate_topics <- tryCatch(
              {
                results <- c()
                for (i in seq_along(text_chunks)) {
                  text_chunk <- text_chunks[[i]]

                  result <- create_candidate_topics(
                    list(text_chunk),
                    research_background,
                    llm_provider_main,
                    language = lang$get_translation_language()
                  )

                  write_progress(
                    i,
                    length(text_chunks),
                    paste(result, collapse = ","),
                    progress_file_secondary
                  )

                  results <- c(results, result)
                }

                write_progress(
                  length(text_chunks),
                  length(text_chunks),
                  lang$t("Alle chunks zijn geanalyseerd."),
                  progress_file_secondary
                )

                results
              },
              error = handle_detailed_error("Candidate topic generation")
            )

            # Step 2: Reduce topics
            write_progress(
              2,
              5,
              lang$t("Onderwerpen reduceren..."),
              progress_file
            )
            topics <- tryCatch(
              reduce_topics(
                candidate_topics,
                research_background,
                llm_provider_large
              ),
              error = handle_detailed_error("Topic reduction")
            )

            # Make intermediate results available
            topics
          },
          globals = list(
            write_progress = write_progress,
            send_prompt_with_retries = send_prompt_with_retries,
            create_candidate_topics = create_candidate_topics,
            reduce_topics = reduce_topics,
            prompt_category = prompt_category,
            prompt_multi_category = prompt_multi_category,
            assign_topics = assign_topics,
            llm_provider_main = llm_provider_main,
            llm_provider_large = llm_provider_large,
            texts = texts$preprocessed,
            research_background = research_background(),
            mode = mode(),
            progress_file = progress_file,
            progress_file_secondary = progress_file_secondary,
            handle_detailed_error = handle_detailed_error,
            text_chunks = context_window$text_chunks,
            get_context_window_size_in_tokens = get_context_window_size_in_tokens,
            lang = lang()
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
            app_error(
              .,
              when = "main processing (step 1-2) of topic modelling",
              fatal = TRUE,
              lang = lang()
            )
          }
        print("Started async processing for topic modelling (step 1-2)")
      })

      ####### >> Topics generated ####

      initial_topics <- reactiveVal(NULL)

      # Listen for topics completion
      # Present modal dialog to edit/confirm topics
      ## Updated observeEvent for topics() with Add/Remove buttons in the modal
      observeEvent(topics(), {
        req(topics())
        req(!topics_definitive())

        # Stash the originals
        initial_topics(isolate(topics()))

        # Initialize the topics table data
        topics_table_data(data.frame(
          topic = topics(),
          stringsAsFactors = FALSE
        ))

        # If no human in the loop, auto-confirm
        if (!isTRUE(human_in_the_loop())) {
          topics_definitive(TRUE)
          return()
        }

        # Write progress
        write_progress(
          2.5,
          5,
          lang()$t("Onderwerpen bewerken..."),
          progress_file
        )

        # Show the editable modal with Add/Remove buttons
        showModal(modalDialog(
          title = lang()$t("Onderwerpen"),
          size = "l",
          easyClose = FALSE,
          tagList(
            shinyjs::useShinyjs(),
            lang()$t("Controleer de onderwerpen en pas ze aan waar nodig."),
            br(),
            HTML(lang()$t(
              "<i>Dubbel-klik op een onderwerp om het te bewerken.</i>"
            )),
            hr(),
            fluidRow(
              column(
                12,
                div(
                  class = "d-flex flex-column flex-md-row justify-content-center",
                  # Left-aligned on md+, centered on xs
                  div(
                    class = "d-flex justify-content-center justify-content-md-start mb-2 mb-md-0 me-md-auto",
                    actionButton(
                      ns("add_topic"),
                      lang()$t("Voeg onderwerp toe"),
                      icon = icon("plus")
                    )
                  ),
                  # Center-aligned always
                  div(
                    class = "d-flex justify-content-center mb-2 mb-md-0",
                    actionButton(
                      ns("reduce_again"),
                      lang()$t("Reduceer opnieuw"),
                      icon = icon("robot")
                    )
                  ),
                  # Right-aligned on md+, centered on xs
                  div(
                    class = "d-flex justify-content-center justify-content-md-end ms-md-auto",
                    actionButton(
                      ns("remove_topic"),
                      lang()$t("Verwijder geselecteerd"),
                      icon = icon("trash")
                    )
                  )
                )
              )
            ),
            hr(),
            DT::dataTableOutput(ns("topics_table")),
            hr(),
            div(
              class = "clearfix",
              # float reset all the way left
              div(
                style = "float: left; margin: 0;",
                actionButton(
                  ns("reset_topics"),
                  "Reset",
                  icon = icon("undo"),
                  class = "btn-warning"
                )
              ),
              # float confirm all the way right
              div(
                style = "float: right; margin: 0;",
                actionButton(
                  ns("confirm_topics"),
                  lang()$t("Bevestig"),
                  class = "btn btn-primary",
                  # Continue icon/arrow
                  icon = icon("arrow-right"),
                )
              )
            ),
          ),
          footer = NULL
        ))

        shinyjs::disable("reset_topics")
      })

      ####### >> Edit & confirm topics ####

      # Render editable table
      output$topics_table <- DT::renderDataTable(
        {
          req(topics_table_data())
          DT::datatable(
            topics_table_data(),
            options = list(
              paging = FALSE,
              ordering = TRUE,
              searching = FALSE
              # dom = "t"
            ),
            rownames = FALSE,
            editable = list(target = "cell"),
            # ← allow row selection
            selection = list(mode = "multiple", target = "row"),
            colnames = setNames(
              c(lang()$t("Onderwerp")),
              "topic"
            )
          )
        },
        server = TRUE
      )

      # Proxy for efficient updates
      proxy <- DT::dataTableProxy("topics_table")

      # Handle edits
      observeEvent(input[["topics_table_cell_edit"]], {
        info <- input[["topics_table_cell_edit"]]
        str(info) # optional: view edit info
        topics_df <- topics_table_data()
        topics_df[info$row, info$col + 1] <- info$value # +1 because DT uses 0-based indexing
        topics_table_data(topics_df)
      })

      # Add topic button
      observeEvent(input$add_topic, {
        df <- topics_table_data()
        topics_table_data(rbind(df, data.frame(topic = "")))
        # push updated data back without resetting paging
        replaceData(proxy, topics_table_data(), resetPaging = FALSE)
      })

      # Remove selected rows
      observeEvent(input$remove_topic, {
        sel <- input$topics_table_rows_selected
        if (length(sel)) {
          df <- topics_table_data()
          df <- df[-sel, , drop = FALSE]
          topics_table_data(df)
          replaceData(proxy, df, resetPaging = FALSE)
        }
      })

      # Reset topics button
      observeEvent(input$reset_topics, {
        # pull back the original topics
        orig <- initial_topics()
        df <- data.frame(topic = orig, stringsAsFactors = FALSE)

        # update your reactiveVal and the DT proxy
        topics_table_data(df)
        replaceData(proxy, df, resetPaging = FALSE)
      })

      # Disable reset when no changes (or enable)
      observe({
        req(initial_topics(), topics_table_data())
        curr <- topics_table_data()$topic
        orig <- initial_topics()

        if (identical(curr, orig)) {
          shinyjs::disable("reset_topics")
        } else {
          shinyjs::enable("reset_topics")
        }
      })

      # Confirm topics button
      observeEvent(input$confirm_topics, {
        req(topics_table_data())
        req(reduction_in_progress() == FALSE)
        final_df <- topics_table_data()
        updated_topics <- final_df$topic[final_df$topic != ""]

        # Topics must be unique
        if (anyDuplicated(updated_topics)) {
          shiny::showNotification(
            lang()$t("Onderwerpen moeten uniek zijn."),
            type = "error"
          )
          return()
        }

        # There must be at least 2 unique topics
        if (length(unique(updated_topics)) < 2) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 2 onderwerpen opgeven."),
            type = "error"
          )
          return()
        }

        # 1) prevent any further modal re‑opens
        topics_definitive(TRUE)
        # 2) close the existing modal
        removeModal()
        # 3) finally update the reactive — now it won't re‑show the modal
        topics(updated_topics)
      })

      # Listen for re-reduce topics button
      observeEvent(input$reduce_again, {
        req(isolate(topics_table_data()))
        req(isolate(reduction_in_progress()) == FALSE)
        updated_topics <- topics_table_data()$topic
        updated_topics <- updated_topics[updated_topics != ""]

        if (length(updated_topics) < 2) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 2 onderwerpen opgeven om te reduceren."),
            type = "error"
          )
          return()
        }

        # Randomize order of updated topics
        updated_topics <- sample(updated_topics)

        shiny::showNotification(
          lang()$t("Onderwerpen re-reduceren..."),
          type = "message"
        )
        reduction_in_progress(TRUE)
        rereduced_topics(NULL)

        llm_provider_large <- llm_provider_rv$llm_provider$clone()
        llm_provider_large$parameters$model <- models$large

        future_promise(
          {
            reduce_topics(
              updated_topics,
              research_background,
              llm_provider_large,
              language = lang$get_translation_language()
            )
          },
          packages = c("tidyprompt", "tidyverse"),
          globals = list(
            send_prompt_with_retries = send_prompt_with_retries,
            reduce_topics = reduce_topics,
            updated_topics = updated_topics,
            research_background = research_background(),
            llm_provider_large = llm_provider_large
          )
        ) %...>%
          (function(reduced_topics) {
            # Only update if the result is valid
            if (length(reduced_topics) < 2 || anyDuplicated(reduced_topics)) {
              app_error(
                lang()$t(
                  "Re-reductie mislukt of ongeldige onderwerpen gegenereerd"
                ),
                when = "re-reducing topics",
                fatal = FALSE,
                lang = lang()
              )
              reduction_in_progress(FALSE)
              return()
            }
            # Update topics
            rereduced_topics(reduced_topics)
          }) %...!%
          {
            app_error(
              .,
              when = "re-reducing topics",
              fatal = FALSE,
              lang = lang()
            )
            reduction_in_progress(FALSE)
          }
      })

      # Listen for re-reduced topics completion
      observe({
        req(rereduced_topics())
        print(paste0(
          "Received rereduced topics: ",
          paste(rereduced_topics(), collapse = ", ")
        ))
        # Update the topics table with the new topics
        updated_topics <- rereduced_topics()
        df <- data.frame(topic = updated_topics, stringsAsFactors = FALSE)
        topics_table_data(df)
        # Reset the reduction_in_progress flag
        reduction_in_progress(FALSE)
      })

      # Disable inputs during re-reduction
      observe({
        if (reduction_in_progress()) {
          shinyjs::disable("add_topic")
          shinyjs::disable("remove_topic")
          shinyjs::disable("reset_topics")
          shinyjs::disable("confirm_topics")
          shinyjs::disable("reduce_again")
          shinyjs::disable("topics_table")
        } else {
          shinyjs::enable("add_topic")
          shinyjs::enable("remove_topic")
          shinyjs::enable("reset_topics")
          shinyjs::enable("confirm_topics")
          shinyjs::enable("reduce_again")
          shinyjs::enable("topics_table")
        }
      })

      ####### >> Assign topics ######

      # Listen for definitive topics
      # Launch processing for assigning topics to paragraphs
      observeEvent(topics_definitive(), {
        if (!isTRUE(topics_definitive())) return()
        req(topics())

        print(paste0(
          "Starting topic assignment with topics: ",
          paste(topics(), collapse = ", ")
        ))

        # Set model
        llm_provider <- llm_provider_rv$llm_provider$clone()
        llm_provider$parameters$model <- models$main

        # Write progress
        write_progress(
          3,
          5,
          lang()$t("Onderwerpen toekennen..."),
          progress_file
        )

        future_promise(
          {
            # Step 4: Assign topics
            # Writing progress on secondary progress file
            texts_with_topics <- tryCatch(
              {
                results <- tibble::tibble(
                  text = character(),
                  result = character()
                )

                for (i in seq_along(texts)) {
                  text <- texts[[i]]
                  result <- assign_topics(
                    c(text),
                    topics,
                    research_background,
                    llm_provider,
                    assign_multiple_categories = assign_multiple_categories
                  )
                  result <- result$result

                  write_progress(
                    i,
                    length(texts),
                    text,
                    progress_file_secondary
                  )

                  # Append to results
                  results <- dplyr::bind_rows(
                    results,
                    tibble::tibble(text = text, result = as.character(result))
                  )
                }

                write_progress(
                  length(texts),
                  length(texts),
                  lang$t("Alle teksten zijn geanalyseerd..."),
                  progress_file_secondary
                )

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
            write_progress(
              4,
              5,
              lang$t("Rapport schrijven..."),
              progress_file
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
                        texts_with_topics[[cat]] == TRUE
                      ]
                    }
                  }

                  # Exclude topics which don't have supporting texts
                  topics_texts_list <- topics_texts_list[
                    purrr::map_lgl(topics_texts_list, ~ isTRUE(length(.x) > 0))
                  ]

                  # Write paragraphs, per topic
                  purrr::map(seq_along(topics_texts_list), function(i) {
                    topic_name <- names(topics_texts_list)[[i]]
                    topic_texts <- topics_texts_list[[i]]

                    # Write progress to secondary progress file
                    write_progress(
                      i,
                      length(topics_texts_list),
                      paste0(
                        lang$t("Schrijven over '"),
                        topic_name,
                        "'..."
                      ),
                      progress_file_secondary
                    )

                    write_paragraph(
                      texts = topic_texts,
                      topic = topic_name,
                      research_background = research_background,
                      llm_provider = llm_provider,
                      language = lang$get_translation_language()
                    )
                  })
                },
                error = handle_detailed_error("Topic report generation")
              )

              # Add as attribute to the result
              attr(texts_with_topics, "paragraphs") <- paragraphs
            }

            write_progress(
              4.5,
              5,
              lang$t("Afronden..."),
              progress_file
            )

            texts_with_topics
          },
          globals = list(
            topics = topics(),
            write_progress = write_progress,
            send_prompt_with_retries = send_prompt_with_retries,
            create_candidate_topics = create_candidate_topics,
            reduce_topics = reduce_topics,
            prompt_category = prompt_category,
            prompt_multi_category = prompt_multi_category,
            assign_topics = assign_topics,
            write_paragraph = write_paragraph,
            llm_provider = llm_provider,
            texts = texts$preprocessed,
            research_background = research_background(),
            mode = mode(),
            progress_file = progress_file,
            progress_file_secondary = progress_file_secondary,
            assign_multiple_categories = assign_multiple_categories(),
            write_paragraphs = write_paragraphs(),
            handle_detailed_error = handle_detailed_error,
            get_context_window_size_in_tokens = get_context_window_size_in_tokens,
            lang = lang()
          ),
          packages = c("tidyprompt", "tidyverse", "glue", "fs", "uuid"),
          seed = NULL
        ) %...>%
          results_df() %...!%
          {
            app_error(
              .,
              when = "main processing (step 3-4) of topic modelling",
              fatal = TRUE,
              lang = lang()
            )
          }
        print("Started async processing for topic modelling (step 3-4)")
      })

      #### Post-processing: interrater-reliability, download files ####

      # Listen for processing completion
      # Join results with original texts
      # Launch interrater reliability module if required
      observeEvent(results_df(), {
        req(results_df())
        print(results_df())

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

        # Delete progress files
        unlink(progress_file, force = TRUE)
        unlink(progress_file_secondary, force = TRUE)

        # Verify that df actually has results
        # (sometimes we have API failure, then result/topic contains NA values)
        if (any(is.na(df$result))) {
          app_error(
            "Results contain NA values; processing failed",
            when = "processing results",
            fatal = TRUE,
            lang = lang()
          )
        }

        # Update UI to show finished processing
        progress_primary$set_progress(100)
        progress_secondary$hide()
        progress_primary$set_text(paste0(
          bsicons::bs_icon("check2-circle"),
          lang()$t(" Verwerking voltooid!")
        ))

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
              c("Categorisatie", "Onderwerpextractie") &
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
            assign_multiple_categories = assign_multiple_categories()
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
          if (!isTRUE(irr_done())) return()
          result_list <- create_result_list()

          # If any in 'result_list$df$result' are NA, show a warning
          error <- any(is.na(result_list$df$result))
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

          # Generate files async
          future(
            {
              # Generate files
              excel_file <- create_result_excel(result_list)
              rmarkdown <- create_result_rmarkdown(result_list)

              # Check if all files exist, if not,
              #   wait up to 10s
              #   and then throw an error
              for (i in 1:10) {
                if (file.exists(excel_file) && file.exists(rmarkdown)) {
                  break
                }
                Sys.sleep(1)
              }
              if (!file.exists(rmarkdown)) {
                stop("Rmarkdown file not found, no error available")
              }
              if (!file.exists(excel_file)) {
                stop("Excel file not found, no error available")
              }

              # If rmarkdown or excel_file are .txt (i.e., error files),
              #   throw an error with their contents
              if (grepl("\\.txt$", rmarkdown)) {
                stop(paste0(
                  "Rmarkdown file generation error: ",
                  readLines(rmarkdown)
                ))
              }
              if (grepl("\\.txt$", excel_file)) {
                stop(paste0(
                  "Excel file generation error: ",
                  readLines(excel_file)
                ))
              }

              # Zip them
              zip_path <- file.path(
                tempdir,
                paste0(uuid::UUIDgenerate(), "_results.zip")
              )
              zip::zipr(
                zipfile = zip_path,
                files = c(excel_file, rmarkdown),
                root = dirname(excel_file)
              )
              zip_path
            },
            globals = list(
              create_result_excel = create_result_excel,
              create_result_rmarkdown = create_result_rmarkdown,
              result_list = result_list,
              tempdir = tempdir()
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
        if (is.null(zip_file())) return()

        # Create download handler
        output$download_results <- downloadHandler(
          filename = function() {
            paste0(uuid, ".zip")
          },
          content = function(file) {
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
      })

      # Restart button listener
      # Launches modal dialog to confirm restart
      observeEvent(input$restart, {
        showModal(modalDialog(
          title = lang()$t("Nieuwe analyse starten?"),
          lang()$t("Zorg dat je eerst de resultaten downloadt."),
          footer = tagList(
            modalButton(lang()$t("Annuleren")),
            actionButton(
              ns("confirm_restart"),
              lang()$t("Ja, nieuwe analyse"),
              class = "btn btn-danger"
            )
          )
        ))
      })

      # Confirm restart button listener
      # Reloads the app
      observeEvent(input$confirm_restart, {
        removeModal()
        session$reload()
      })

      #### Helper functions ####

      # Handle error details
      handle_detailed_error <- function(context = "An operation") {
        function(e) {
          error_message <- paste0(
            context,
            " failed:\n",
            "Message: ",
            conditionMessage(e)
          )
          stop(error_message)
        }
      }

      # Helper function to create a list with all the results, including metadata
      create_result_list <- function(df) {
        result_list <- list(
          df = final_results_df(),
          time = Sys.time(),
          uuid = uuid,
          mode = mode(),
          research_background = research_background(),
          url = llm_provider_rv$llm_provider$url,
          irr = irr_result()
        )

        if (mode() == "Categorisatie") {
          result_list$model <- models$main
          result_list$categories <- categories$texts()
          result_list$assign_multiple_categories <- assign_multiple_categories()
          result_list$prompt <- prompt_category(
            text = lang()$t("<< TEKST >>"),
            research_background = research_background(),
            categories = categories$texts()
          ) |>
            tidyprompt::construct_prompt_text()
        }

        if (mode() == "Scoren") {
          result_list$model <- models$main
          result_list$scoring_characteristic <- scoring_characteristic()
          result_list$prompt <- prompt_score(
            text = lang()$t("<< TEKST >>"),
            research_background = research_background(),
            scoring_characteristic = scoring_characteristic()
          ) |>
            tidyprompt::construct_prompt_text()
        }

        if (mode() == "Onderwerpextractie") {
          result_list$model <- models$main
          result_list$model_reductie <- models$large
          result_list$topics <- topics()
          result_list$assign_multiple_categories <- assign_multiple_categories()

          # Add chunking information
          chunking_parameters <- tibble::tibble(
            parameter = c(
              "max_chunk_size",
              "max_redrawing",
              "n_tokens_context_window",
              "n_chunks"
            ),
            value = c(
              context_window$max_chunk_size,
              context_window$max_redrawing,
              context_window$n_tokens_context_window,
              context_window$n_chunks
            )
          )

          result_list$chunking_parameters <- chunking_parameters
        }

        # Transfer paragraphs if present as attribute
        if (!is.null(attr(final_results_df(), "paragraphs"))) {
          result_list$paragraphs <- attr(final_results_df(), "paragraphs")
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

        # Try rendering
        result <- tryCatch(
          {
            rmarkdown::render(
              input = paste0("R/report_", result_list$mode, ".Rmd"),
              output_file = output_file_html,
              params = list(result_list = result_list),
              envir = new.env()
            )
            output_file_html # On success, return HTML path
          },
          error = function(e) {
            # On error, write the error message to a text file
            writeLines(
              paste("Error during rendering:", conditionMessage(e)),
              con = output_file_txt
            )
            output_file_txt # Return TXT path instead
          }
        )

        return(result)
      }

      # Helper function to write progress to file
      write_progress <- function(i, total, text, file) {
        lines <- paste0(
          i,
          "/",
          total,
          "\n",
          "<br><i>",
          stringr::str_trunc(text, 50),
          "</i>"
        )
        tryCatch(
          {
            writeLines(lines, file)
          },
          error = function(e) {
            app_error(
              e,
              when = "writing progress to file",
              fatal = FALSE,
              lang = lang()
            )
          }
        )
      }

      # Helper function to read progress from file
      # (returns list with '$progress_value' and '$progress_text')
      read_progress <- function(file) {
        tryCatch(
          {
            if (file.exists(file)) {
              lines <- readLines(file)
              progress <- as.numeric(strsplit(lines, "/")[[1]])
              progress_value <- progress[1] / progress[2] * 100
              progress_text <- paste(lines, collapse = "\n")

              list(
                progress_value = progress_value,
                progress_text = progress_text
              )
            } else {
              NULL
            }
          },
          error = function(e) {
            app_error(
              e,
              when = "reading progress from file",
              fatal = FALSE,
              lang = lang()
            )
            return(NULL)
          }
        )
      }

      #### Progress bar update ####

      # We check progress/... every 250ms;
      #  asynchronous processes write their progress to a file,
      #  here (main process) we read the progress from the file
      #  and update the progress bar

      progress_primary <- progress_bar_server("progress_primary")
      progress_secondary <- progress_bar_server("progress_secondary")

      # Poll progress file every 250ms
      observe({
        invalidateLater(250, session)
        if (!processing()) return()

        # Find file using uuid,
        # read progress from file and update progress bar
        if (!is.null(uuid)) {
          if (file.exists(progress_file)) {
            progress <- read_progress(progress_file)
            progress_primary$set_progress(progress$progress_value)
            progress_primary$set_text(progress$progress_text)
          }

          if (file.exists(progress_file_secondary)) {
            progress <- read_progress(progress_file_secondary)
            progress_secondary$set_progress(progress$progress_value)
            progress_secondary$set_text(progress$progress_text)

            if (isTRUE(progress$progress_value == 100)) {
              progress_secondary$hide()
            } else {
              progress_secondary$show()
            }
          } else {
            progress_secondary$hide()
          }
        }
      })

      #### Processing button ####

      output$process_button <- renderUI({
        req(mode(), lang())

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

      #### Process button update ####

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

      return(processing)
    }
  )
}


##### 2 Example/development usage ####

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
