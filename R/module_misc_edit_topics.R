# 1 Server -------------------------------------------------------------

#' Modal for editing/reducing topics.
#'
#' @param topics Reactive returning a character vector of topics.
#' @param exclusive_topics Reactive returning a character vector of exclusive topics.
#' @param research_background Reactive returning a character scalar.
#' @param assign_multiple_categories Reactive returning a logical scalar.
#' @param llm_provider A tidyprompt provider used for reduction calls.
edit_topics_server <- function(
  id,
  topics,
  exclusive_topics,
  research_background,
  assign_multiple_categories,
  llm_provider,
  assignment_texts = reactive(character()),
  assignment_llm_provider = reactive(NULL),
  lang = default_lang()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## ── reactive stores ─────────────────────────────────────────
      started <- reactiveVal(NULL)
      initial_topics <- topics
      initial_exclusive <- exclusive_topics
      edited_topics <- reactiveVal(NULL)
      reduction_in_progress <- reactiveVal(FALSE)
      rereduced_topics <- reactiveVal(NULL)
      topics_table_data <- reactiveVal(NULL)

      build_df <- function(t, excl) {
        data.frame(
          topic = t,
          exclusive = t %in% excl,
          stringsAsFactors = FALSE
        )
      }

      normalize_topic_values <- function(values, unique_only = FALSE) {
        normalized <- trimws(as.character(values %||% character()))
        normalized <- normalized[!is.na(normalized) & nzchar(normalized)]
        if (isTRUE(unique_only)) {
          normalized <- unique(normalized)
        }
        normalized
      }

      topic_assignment_fit_info <- reactive({
        req(topics_table_data())

        current_topics <- normalize_topic_values(topics_table_data()$topic)
        if (length(current_topics) < 2) {
          return(list(
            fits = TRUE,
            prompt_tokens = NA_integer_,
            context_window_tokens = NA_integer_
          ))
        }

        assignment_provider <- assignment_llm_provider()
        assignment_text_values <- assignment_texts()
        if (is.null(assignment_provider) || !length(assignment_text_values)) {
          return(list(
            fits = TRUE,
            prompt_tokens = NA_integer_,
            context_window_tokens = NA_integer_
          ))
        }

        current_exclusive <- if ("exclusive" %in% names(topics_table_data())) {
          normalize_topic_values(
            topics_table_data()$topic[topics_table_data()$exclusive %in% TRUE],
            unique_only = TRUE
          )
        } else {
          character()
        }

        topic_assignment_prompt_context_window_check(
          texts = assignment_text_values,
          topics = current_topics,
          research_background = research_background(),
          llm_provider = assignment_provider,
          assign_multiple_categories = assign_multiple_categories(),
          exclusive_topics = current_exclusive
        )
      })

      topic_assignment_fit_message <- function(fit_info) {
        sprintf(
          lang()$t(
            "De huidige onderwerpenlijst past niet binnen het context-window van het toekenningsmodel (%d > %d tokens). Verminder het aantal of de lengte van de onderwerpen voordat je doorgaat."
          ),
          fit_info$prompt_tokens,
          fit_info$context_window_tokens
        )
      }

      shiny::exportTestValues(
        started = started(),
        reduction_in_progress = reduction_in_progress(),
        rereduced_topics = rereduced_topics(),
        topics_table_data = topics_table_data(),
        topic_assignment_prompt_fits = topic_assignment_fit_info()$fits
      )

      ## ── first-run modal ────────────────────────────────────────
      observe({
        req(topics())
        req(is.null(edited_topics()))

        topics_table_data(build_df(topics(), exclusive_topics()))

        showModal(modalDialog(
          title = lang()$t("Onderwerpen"),
          size = "l",
          easyClose = FALSE,
          tagList(
            shinyjs::useShinyjs(),
            tags$div(
              style = "display:none;",
              `data-kwallm-modal-id` = "edit_topics_modal",
              `data-kwallm-modal-details` = sprintf(
                "module_id=%s, n_topics=%d",
                id,
                length(topics() %||% character(0))
              )
            ),
            lang()$t("Controleer de onderwerpen en pas ze aan waar nodig."),
            br(),
            HTML(lang()$t("<i>Dubbel-klik op een cel om te bewerken.</i>")),
            uiOutput(ns("topic_assignment_fit_warning")),
            hr(),
            fluidRow(
              column(
                12,
                div(
                  class = "d-flex flex-column flex-md-row justify-content-center",
                  div(
                    class = "d-flex justify-content-center justify-content-md-start mb-2 mb-md-0 me-md-auto",
                    actionButton(
                      ns("add_topic"),
                      lang()$t("Voeg onderwerp toe"),
                      icon = icon("plus")
                    )
                  ),
                  div(
                    class = "d-flex justify-content-center mb-2 mb-md-0",
                    actionButton(
                      ns("reduce_again"),
                      lang()$t("Reduceer opnieuw"),
                      icon = icon("robot")
                    )
                  ),
                  div(
                    class = "d-flex justify-content-center justify-content-md-end ms-md-auto",
                    actionButton(
                      ns("delete_empty"),
                      lang()$t("Verwijder lege regels"),
                      icon = icon("eraser")
                    )
                  )
                )
              )
            ),
            hr(),
            rhandsontable::rHandsontableOutput(
              ns("topics_table"),
              width = "100%"
            ),
            hr(),
            modal_footer_buttons(
              left = actionButton(
                ns("reset_topics"),
                lang()$t("Reset"),
                icon = icon("undo"),
                class = "btn-warning"
              ),
              right = actionButton(
                ns("confirm_topics"),
                lang()$t("Bevestig"),
                icon = icon("arrow-right"),
                class = "btn btn-primary"
              )
            )
          ),
          footer = NULL
        ))

        shinyjs::delay(
          250,
          shinyjs::disable("reset_topics")
        )

        started(TRUE)
      })

      ### ── rhandsontable with narrow checkbox & _always_ full-width ──────────
      output$topics_table <- rhandsontable::renderRHandsontable({
        req(topics_table_data())

        colHeaders <- c(lang()$t("Onderwerp"))
        if (assign_multiple_categories()) {
          colHeaders <- c(colHeaders, lang()$t("Exclusief"))
        }

        data <- topics_table_data()
        if (!assign_multiple_categories()) {
          data <- data |> dplyr::select(topic)
        }

        table <- rhandsontable::rhandsontable(
          data,
          rowHeaders = NULL,
          colHeaders = colHeaders,
          width = "100%" # container 100 %
        ) |>
          rhandsontable::hot_table(
            stretchH = "all", # ← full-width **always**
            manualColumnResize = TRUE,
            manualRowResize = TRUE,
            contextMenu = TRUE
          )

        if (assign_multiple_categories()) {
          table <- table |>
            rhandsontable::hot_col(
              lang()$t("Exclusief"),
              type = "checkbox",
              width = 80 # keep checkbox column narrow
            )
        }

        table
      })

      observeEvent(input$topics_table, {
        topics_table_data(rhandsontable::hot_to_r(input$topics_table))
      })

      output$topic_assignment_fit_warning <- renderUI({
        fit_info <- topic_assignment_fit_info()
        if (isTRUE(fit_info$fits)) {
          return(NULL)
        }

        div(
          class = "alert alert-danger d-flex align-items-center mt-2",
          bsicons::bs_icon("exclamation-triangle-fill"),
          span(
            class = "ms-2",
            topic_assignment_fit_message(fit_info)
          )
        )
      })

      # add / delete-empty / reset rows -------------------------------
      observeEvent(input$add_topic, {
        df <- topics_table_data()
        log_action(
          "topic_added",
          details = sprintf("n_rows_before=%d", nrow(df))
        )
        topics_table_data(dplyr::bind_rows(
          df,
          data.frame(topic = "", exclusive = FALSE)
        ))
      })

      observeEvent(input$delete_empty, {
        df <- topics_table_data()
        n_before <- nrow(df)
        df$topic <- trimws(df$topic)
        df2 <- df[df$topic != "", , drop = FALSE]
        log_action(
          "topic_empty_rows_deleted",
          details = sprintf(
            "n_rows_before=%d n_rows_after=%d",
            n_before,
            nrow(df2)
          )
        )
        topics_table_data(df2)
      })

      observeEvent(input$reset_topics, {
        log_action(
          "topics_reset",
          details = sprintf(
            "n_topics_initial=%d",
            length(initial_topics() %||% character(0))
          )
        )
        topics_table_data(build_df(initial_topics(), initial_exclusive()))
      })

      observe({
        orig <- build_df(initial_topics(), initial_exclusive())
        shinyjs::toggleState(
          "reset_topics",
          !identical(topics_table_data(), orig)
        )
      })

      # confirm -------------------------------------------------------
      observeEvent(input$confirm_topics, {
        req(!reduction_in_progress())
        df <- topics_table_data()

        updated_topics <- normalize_topic_values(df$topic)
        updated_exclusive <- if ("exclusive" %in% names(df)) {
          normalize_topic_values(
            df$topic[df$exclusive %in% TRUE],
            unique_only = TRUE
          )
        } else {
          character()
        }

        log_action(
          "topics_confirmed",
          details = sprintf(
            "n_topics=%d n_exclusive=%d",
            length(updated_topics),
            length(updated_exclusive)
          )
        )

        if (anyDuplicated(updated_topics)) {
          shiny::showNotification(
            lang()$t("Onderwerpen moeten uniek zijn."),
            type = "error"
          )
          return()
        }
        if (length(updated_topics) < 2) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 2 onderwerpen opgeven."),
            type = "error"
          )
          return()
        }

        fit_info <- topic_assignment_fit_info()
        if (!isTRUE(fit_info$fits)) {
          shiny::showNotification(
            topic_assignment_fit_message(fit_info),
            type = "error",
            duration = NULL
          )
          return()
        }

        exclusive_topics(updated_exclusive)
        removeModal()
        edited_topics(updated_topics)

        # Log topic editing result
        log_info(
          sprintf(
            "Topics edited (human-in-loop): n_original=%d, n_final=%d, n_exclusive=%d",
            length(initial_topics()),
            length(updated_topics),
            length(updated_exclusive)
          ),
          component = "topics"
        )
      })

      # re-reduce  ----------------------------------------------------
      observeEvent(input$reduce_again, {
        req(!reduction_in_progress())
        updated_topics <- normalize_topic_values(topics_table_data()$topic)

        if (length(updated_topics) < 2) {
          shiny::showNotification(
            lang()$t("Je moet minimaal 2 onderwerpen opgeven om te reduceren."),
            type = "error"
          )
          return()
        }

        updated_topics <- sample(updated_topics)
        shiny::showNotification(
          lang()$t("Onderwerpen re-reduceren..."),
          type = "message"
        )
        reduction_in_progress(TRUE)
        rereduced_topics(NULL)

        # Log topic re-reduction start
        log_info(
          sprintf(
            "Topics re-reduction started: n_topics=%d",
            length(updated_topics)
          ),
          component = "topics"
        )

        log_context <- log_context_capture(is_async = TRUE)

        mirai::mirai(
          {
            kwallm_worker_bootstrap(
              task = "topic_reduction",
              app_root = app_root,
              worker_options = worker_options,
              log_context = log_context
            )

            reduce_topics(
              updated_topics,
              research_background,
              llm_provider,
              language = lang$get_translation_language()
            )
          },
          .args = c(
            list(
              app_root = kwallm_worker_app_root(),
              worker_options = kwallm_worker_capture_options(),
              log_context = log_context,
              updated_topics = updated_topics,
              research_background = research_background(),
              llm_provider = llm_provider,
              lang = lang()
            ),
            kwallm_worker_bootstrap_globals()
          )
        ) %...>%
          (function(reduced_topics) {
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

      # apply re-reduced topics --------------------------------------
      observeEvent(rereduced_topics(), ignoreNULL = TRUE, ignoreInit = TRUE, {
        new_topics <- rereduced_topics()

        # isolate so this read doesn’t create a dependency
        df_current <- isolate(topics_table_data())
        current_exclusive <- trimws(df_current$topic[df_current$exclusive])

        current_exclusive <- intersect(current_exclusive, new_topics)
        special <- lang()$t("Onbekend/niet van toepassing")
        if (special %in% new_topics) {
          current_exclusive <- union(current_exclusive, special)
        }

        topics_table_data(build_df(new_topics, current_exclusive))
        reduction_in_progress(FALSE)

        # Log topic re-reduction result
        log_info(
          sprintf(
            "Topics re-reduction complete: n_final=%d",
            length(new_topics)
          ),
          component = "topics"
        )
      })

      # global enable/disable during re-reduce ----------------------
      observe({
        ids <- c(
          "add_topic",
          "delete_empty",
          "reset_topics",
          "confirm_topics",
          "reduce_again"
        )
        lapply(
          ids,
          function(btn) shinyjs::toggleState(btn, !reduction_in_progress())
        )
      })

      # output -------------------------------------------------------
      return(edited_topics)
    }
  )
}

# 2 Example/development usage ----------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(rhandsontable)
  library(tidyprompt)
  library(mirai)
  library(promises)

  ui <- bslib::page(
    useShinyjs(),
    textOutput("selected_topics"),
    textOutput("exclusive_topics")
  )

  server <- function(input, output, session) {
    topics <- reactiveVal(c("Onderwerp 1", "Onderwerp 2", "Onderwerp 3"))
    exclusive_topics <- reactiveVal(c("Onderwerp 2"))
    research_background <- reactiveVal("My research background")
    assign_multiple_categories <- reactiveVal(TRUE)
    llm_provider <- tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4o-mini"
    ))

    edited_topics <- edit_topics_server(
      "edit_topics",
      topics = topics,
      exclusive_topics = exclusive_topics,
      research_background = research_background,
      assign_multiple_categories = assign_multiple_categories,
      llm_provider = llm_provider
    )

    output$selected_topics <- renderText({
      paste("Topics:", paste(edited_topics(), collapse = ", "))
    })
    output$exclusive_topics <- renderText({
      paste("Exclusive:", paste(exclusive_topics(), collapse = ", "))
    })
  }

  shinyApp(ui, server)
}
