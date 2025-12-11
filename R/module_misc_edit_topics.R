# 1 Server -------------------------------------------------------------
edit_topics_server <- function(
  id,
  topics = reactiveVal(NULL),
  exclusive_topics = reactiveVal(NULL),
  research_background = reactiveVal("this is my research"),
  assign_multiple_categories = reactiveVal(TRUE),
  llm_provider = tidyprompt::llm_provider_openai(),
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

      shiny::exportTestValues(
        started = started()
      )

      ## ── first-run modal ────────────────────────────────────────
      observe({
        req(topics())
        req(is.null(edited_topics()))

        topics_table_data(build_df(topics(), exclusive_topics()))
        
        log_action("edit_topics_modal_opened", details = sprintf("n_topics=%d", length(topics())))

        showModal(modalDialog(
          title = lang()$t("Onderwerpen"),
          size = "l",
          easyClose = FALSE,
          tagList(
            shinyjs::useShinyjs(),
            lang()$t("Controleer de onderwerpen en pas ze aan waar nodig."),
            br(),
            HTML(lang()$t("<i>Dubbel-klik op een cel om te bewerken.</i>")),
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
                "Reset",
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

      # add / delete-empty / reset rows -------------------------------
      observeEvent(input$add_topic, {
        df <- topics_table_data()
        topics_table_data(dplyr::bind_rows(
          df,
          data.frame(topic = "", exclusive = FALSE)
        ))
      })

      observeEvent(input$delete_empty, {
        df <- topics_table_data()
        df$topic <- trimws(df$topic)
        topics_table_data(df[df$topic != "", , drop = FALSE])
      })

      observeEvent(input$reset_topics, {
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

        updated_topics <- trimws(df$topic[df$topic != ""])
        updated_exclusive <- trimws(df$topic[df$exclusive & df$topic != ""])

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

        exclusive_topics(updated_exclusive)
        log_action("edit_topics_modal_closed")
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
        updated_topics <- trimws(topics_table_data()$topic)
        updated_topics <- updated_topics[updated_topics != ""]

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
          sprintf("Topics re-reduction started: n_topics=%d", length(updated_topics)),
          component = "topics"
        )

        log_opts <- list(
          level = getOption("logger__level", "INFO"),
          dir = getOption("logger__dir", "logs"),
          retention = getOption("logger__retention")
        )

        future_promise(
          {
            options(
              logger__level = log_opts$level,
              logger__dir = log_opts$dir,
              logger__retention = log_opts$retention
            )
            try(log_init(), silent = TRUE)
            
            reduce_topics(
              updated_topics,
              research_background,
              llm_provider,
              language = lang$get_translation_language()
            )
          },
          packages = c("tidyprompt", "tidyverse"),
          globals = list(
            send_prompt_with_retries = send_prompt_with_retries,
            reduce_topics = reduce_topics,
            updated_topics = updated_topics,
            research_background = research_background(),
            llm_provider = llm_provider,
            lang = lang(),
            get_context_window_size_in_tokens = get_context_window_size_in_tokens,
            tiktoken_load_tokenizer = tiktoken_load_tokenizer,
            count_tokens = count_tokens,
            count_tokens = count_tokens,
            async_message_printer = async_message_printer,
            log_opts = log_opts,
            log_init = log_init,
            get_session_id = get_session_id
          ),
          seed = NULL
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
          sprintf("Topics re-reduction complete: n_final=%d", length(new_topics)),
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
  library(future)
  library(promises)

  ui <- bslib::page(
    useShinyjs(),
    textOutput("selected_topics"),
    textOutput("exclusive_topics")
  )

  server <- function(input, output, session) {
    topics <- reactiveVal(c("Onderwerp 1", "Onderwerp 2", "Onderwerp 3"))
    exclusive_topics <- reactiveVal(c("Onderwerp 2"))

    edited_topics <- edit_topics_server(
      "edit_topics",
      topics = topics,
      exclusive_topics = exclusive_topics
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
