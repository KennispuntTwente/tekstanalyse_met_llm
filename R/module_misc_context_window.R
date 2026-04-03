# Module for context window and chunking parameters
# Ensures that the texts fit within the context window of the LLM

# 1 UI ---------------------------------------------------------------
context_window_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "card-container",
      uiOutput(ns("card"))
    )
  )
}


# 2 Server ---------------------------------------------------------

#' Manage context window and chunking parameters.
#'
#' @param mode Reactive returning current analysis mode (character scalar).
#' @param models reactiveValues with `$main` and `$large` tidyprompt providers (must have `$parameters$model`).
#' @param categories List with reactives `$texts()`, `$editing()`, `$unique_non_empty_count()`.
#' @param scoring_characteristic Reactive returning a character scalar.
#' @param codes List with reactives `$texts()`, `$editing()`, `$unique_non_empty_count()`.
#' @param research_background Reactive returning a character scalar.
#' @param assign_multiple_categories Reactive returning a logical scalar.
#' @param texts reactiveValues with at least `$preprocessed` (character vector).
context_window_server <- function(
  id,
  mode,
  models,
  categories,
  scoring_characteristic,
  codes,
  research_background,
  assign_multiple_categories = reactiveVal(FALSE),
  texts = reactiveValues(
    preprocessed = character(),
    raw = character()
  ),
  processing = reactiveVal(FALSE),
  lang = default_lang(),
  chunk_size_default = getOption(
    "topic_modelling__chunk_size_default",
    25
  ),
  chunk_size_limit = getOption(
    "topic_modelling__chunk_size_limit",
    50
  ),
  number_of_chunks_limit = getOption(
    "topic_modelling__number_of_chunks_limit",
    100
  ),
  draws_default = getOption(
    "topic_modelling__draws_default",
    1
  ),
  draws_limit = getOption(
    "topic_modelling__draws_limit",
    5
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Context-window"),
            paste0(
              lang()$t(
                "Het context-window is de hoeveelheid tekst die het taalmodel kan verwerken in één keer."
              ),
              lang()$t(
                " Er moet voor worden gezorgd dat de onderzoeksachtergrond met de (langste) tekst die je invoert binnen het context-window van het model past."
              ),
              lang()$t(
                " Daarnaast worden bij de eerste stap van onderwerpextractie de teksten in chunks verdeeld; deze chunks moeten ook binnen het context-window passen."
              ),
              lang()$t(
                " Met parameters kan je de grootte van de chunks en het aantal trekkingen per tekst instellen."
              )
            )
          ),
          card_body(
            div(
              class = "d-flex flex-column align-items-center",
              uiOutput(ns("context_window_ui")),
              uiOutput(ns("fit_context_window_warning")),
              uiOutput(ns("too_many_chunks_warning")),
              uiOutput(ns("n_chunks_display")),
            )
          )
        )
      })

      # Reactive values ----------------------------------------------
      rv <- reactiveValues(
        chunk_size = chunk_size_default,
        draws = draws_default,
        n_tokens_context_window = NULL,
        max_tokens = 256,
        overlap = 0,
        base_prompt_text = NULL,
        fit_context_window_assigning = NULL,
        fit_context_window_chunks = NULL,
        text_chunks = NULL,
        n_chunks = NULL
      )

      prev_params <- reactiveVal(NULL)
      prev_states <- reactiveVal(list(
        any_fit_problem = NULL,
        too_many_chunks = NULL,
        n_chunks = NULL,
        fit_context_window_assigning = NULL,
        fit_context_window_chunks = NULL
      ))

      # Keep rv in sync with numeric inputs
      observe({
        if (is_valid_number(input$chunk_size)) {
          rv$chunk_size <- input$chunk_size
        }

        if (
          is_valid_number(input$draws) &&
            input$draws <= draws_limit
        ) {
          rv$draws <- input$draws
        }

        if (is_valid_number(input$context_window)) {
          rv$n_tokens_context_window <- input$context_window
        }

        if (is_valid_number(input$max_tokens)) {
          rv$max_tokens <- input$max_tokens
        }

        if (is_valid_number(input$overlap)) {
          rv$overlap <- input$overlap
        }
      })

      # Log parameter changes (debounced by diffing previous values)
      observe({
        req(!isTRUE(processing()))
        req(mode())

        current <- list(
          mode = mode() %||% "unknown",
          context_window = rv$n_tokens_context_window,
          chunk_size = rv$chunk_size,
          draws = rv$draws,
          max_tokens = rv$max_tokens,
          overlap = rv$overlap
        )

        prev <- prev_params()
        if (is.null(prev)) {
          prev_params(current)
          return()
        }

        if (!identical(current, prev)) {
          log_action(
            "context_window_params_changed",
            details = sprintf(
              "mode=%s context_window=%s chunk_size=%s draws=%s max_tokens=%s overlap=%s",
              as.character(current$mode),
              as.character(current$context_window),
              as.character(current$chunk_size),
              as.character(current$draws),
              as.character(current$max_tokens),
              as.character(current$overlap)
            )
          )
          prev_params(current)
        }
      })

      # Enforce limit on chunk_size
      observe({
        req(input$chunk_size)
        if (input$chunk_size > chunk_size_limit) {
          updateNumericInput(session, "chunk_size", value = chunk_size_limit)
        } else if (input$chunk_size < 1) {
          updateNumericInput(session, "chunk_size", value = 1)
        }
      })

      # Enforce limit on draws
      observe({
        req(input$draws)
        if (input$draws > draws_limit) {
          updateNumericInput(session, "draws", value = draws_limit)
        } else if (input$draws < 1) {
          updateNumericInput(session, "draws", value = 1)
        }
      })

      # Enforce limit on context window size
      observe({
        req(input$context_window)
        if (input$context_window < 0) {
          updateNumericInput(session, "context_window", value = 0)
        }
      })

      # Obtain context window size based on model --------------------
      observe({
        req(models$main)
        size <- get_context_window_size_in_tokens(models$main$parameters$model)
        context_window_known <- !is.null(size)

        size <- ifelse(
          is.null(size),
          2048,
          size
        )

        rv$n_tokens_context_window <- size
        rv$context_window_known <- context_window_known
      })

      # Enable/disable input based on if context window is known -----
      # observe({
      #   req(models$main)
      #
      #   shinyjs::delay(
      #     250,
      #     shinyjs::toggleState(
      #       "context_window",
      #       condition = isTRUE(rv$context_window_known)
      #     )
      #   )
      # })

      # Obtain number of characters in the base prompt, based on parameters ----
      # This is for categorization & scoring
      # (not candidate topic generation or writing paragraphs)
      observe({
        req(mode())
        req(!is.null(research_background()))
        rv$base_prompt_text <- NULL

        prompt <- switch(
          mode(),
          "Categorisatie" = {
            req(categories$texts())
            if (assign_multiple_categories()) {
              prompt_multi_category(
                text = "",
                research_background = research_background(),
                categories = categories$texts(),
                exclusive_categories = categories$texts()[
                  seq_along(categories$texts()) %% 2 == 0
                ]
              )
            } else {
              prompt_category(
                text = "",
                research_background = research_background(),
                categories = categories$texts()
              )
            }
          },
          "Onderwerpextractie" = {
            # Approximate categories (as they are not known yet; assume a long list of 50)
            prompt_multi_category(
              text = "",
              research_background = research_background(),
              categories = paste0("Category ", seq(1, 50)),
              exclusive_categories = paste0("Category ", seq(2, 50, by = 2))
            )
          },
          "Scoren" = {
            req(scoring_characteristic())
            prompt_score(
              text = "",
              research_background = research_background(),
              scoring_characteristic = scoring_characteristic()
            )
          },
          "Markeren" = {
            req(codes$texts())
            longest_code <- codes$texts()[
              which.max(count_tokens(codes$texts()))
            ]

            mark_text_prompt(
              text = "",
              code = longest_code,
              research_background = research_background()
            )
          },
          NULL
        )

        if (!is.null(prompt)) {
          rv$base_prompt_text <- prompt |> tidyprompt::construct_prompt_text()
        } else {
          rv$base_prompt_text <- NULL
        }
      })

      # Check if longest text + base prompt fit ----------------------
      observe({
        req(
          mode() %in%
            c("Categorisatie", "Scoren", "Onderwerpextractie", "Markeren")
        )
        req(texts$preprocessed)
        req(rv$base_prompt_text)
        req(rv$n_tokens_context_window)

        texts <- texts$preprocessed
        base_prompt_text <- rv$base_prompt_text

        # Check if the longest text + base prompt fits in the context window
        # Ensure only one longest text is selected
        longest_text <- texts[which.max(count_tokens(texts))]
        longest_text_tokens <- count_tokens(longest_text)
        if (mode() %in% c("Markeren")) {
          req(rv$max_tokens)
          # If longest text is longer than max_tokens, use max_tokens
          if (longest_text_tokens > rv$max_tokens) {
            longest_text_tokens <- rv$max_tokens
          }
        }

        total_length <- longest_text_tokens +
          count_tokens(base_prompt_text)

        if (total_length > (rv$n_tokens_context_window)) {
          rv$fit_context_window_assigning <- FALSE
        } else {
          rv$fit_context_window_assigning <- TRUE
        }
      })

      # Make chunks & check if they fit ------------------------------
      observe({
        req(mode() == "Onderwerpextractie")
        req(texts$preprocessed)
        req(rv$n_tokens_context_window)
        req(rv$chunk_size)
        req(rv$draws)

        texts <- texts$preprocessed

        # Based on prompt for candidate topic generation; 600 characters + background
        base_prompt_text <- prompt_candidate_topics(
          text_chunk = c(""),
          research_background = research_background(),
          language = lang()$get_translation_language()
        ) |>
          tidyprompt::construct_prompt_text()

        rv$text_chunks <- create_text_chunks(
          texts = texts,
          chunk_size = rv$chunk_size,
          draws = rv$draws,
          n_tokens_context_window = rv$n_tokens_context_window,
          base_prompt_text = base_prompt_text
        )

        if (is.null(rv$text_chunks)) {
          rv$fit_context_window_chunks <- FALSE
        } else {
          rv$fit_context_window_chunks <- TRUE
        }

        if (length(rv$text_chunks) > number_of_chunks_limit) {
          rv$too_many_chunks <- TRUE
        } else {
          rv$too_many_chunks <- FALSE
        }

        rv$n_chunks <- length(rv$text_chunks)
      })

      # Check for presence of any fit problem ------------------------
      observe({
        if (isTRUE(mode() == "Onderwerpextractie")) {
          if (
            isFALSE(rv$fit_context_window_chunks) ||
              isFALSE(rv$fit_context_window_assigning)
          ) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }

        if (isTRUE(mode() %in% c("Categorisatie", "Scoren", "Markeren"))) {
          if (isFALSE(rv$fit_context_window_assigning)) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }
      })

      # Log warning state transitions (once per change)
      observe({
        req(mode())

        current_states <- list(
          any_fit_problem = rv$any_fit_problem,
          too_many_chunks = rv$too_many_chunks,
          n_chunks = rv$n_chunks,
          fit_context_window_assigning = rv$fit_context_window_assigning,
          fit_context_window_chunks = rv$fit_context_window_chunks
        )

        prev <- prev_states()
        if (
          !identical(current_states$any_fit_problem, prev$any_fit_problem) &&
            !is.null(current_states$any_fit_problem)
        ) {
          log_action(
            "context_window_fit_problem_changed",
            details = sprintf(
              "mode=%s any_fit_problem=%s fit_assigning=%s fit_chunks=%s",
              mode() %||% "unknown",
              as.character(current_states$any_fit_problem),
              as.character(current_states$fit_context_window_assigning),
              as.character(current_states$fit_context_window_chunks)
            )
          )
        }

        if (
          !identical(current_states$too_many_chunks, prev$too_many_chunks) &&
            !is.null(current_states$too_many_chunks)
        ) {
          log_action(
            "too_many_chunks_changed",
            details = sprintf(
              "mode=%s too_many_chunks=%s n_chunks=%s limit=%d",
              mode() %||% "unknown",
              as.character(current_states$too_many_chunks),
              as.character(current_states$n_chunks),
              number_of_chunks_limit
            )
          )
        }

        if (
          !identical(current_states$n_chunks, prev$n_chunks) &&
            !is.null(current_states$n_chunks)
        ) {
          log_action(
            "n_chunks_changed",
            details = sprintf(
              "mode=%s n_chunks=%s",
              mode() %||% "unknown",
              as.character(current_states$n_chunks)
            )
          )
        }

        prev_states(current_states)
      })

      # Show inputs (context window, chunking parameters), based on mode ----
      output$context_window_ui <- renderUI({
        req(
          mode() %in%
            c("Categorisatie", "Scoren", "Onderwerpextractie", "Markeren")
        )
        return(div(
          class = "d-flex flex-column align-items-center",
          numericInput(
            ns("context_window"),
            label = span(
              HTML(paste0(lang()$t("Context-window grootte (tokens)"))),
              tooltip(
                bsicons::bs_icon("info-circle"),
                paste0(
                  lang()$t(
                    "Dit is de context-window grootte waarmee gerekend wordt om te bepalen of de verschillende prompts & teksten in het context-window passen."
                  ),
                  lang()$t(
                    " Je kan de waarde zelf aanpassen indien de app niet geconfigureerd is om hier de juiste grootte voor jouw model & LLM-provider te tonen."
                  ),
                  lang()$t(
                    " Het aanpassen van deze waarde enkel effect op de berekening (niet op de daadwerkelijke context-window grootte die het model hanteert)."
                  )
                )
              )
            ),
            value = rv$n_tokens_context_window,
            min = 0
          ),
          if (mode() == "Onderwerpextractie") {
            list(
              # Add subtle text to explain chunking parameters
              description_box(
                lang()$t(
                  "Onderstaande parameters bepalen hoe de teksten worden verdeeld in chunks voor het genereren van onderwerpen in de 'onderwerpextractie'-modus."
                )
              ),
              numericInput(
                ns("chunk_size"),
                lang()$t("Maximaal aantal teksten per chunk"),
                value = rv$chunk_size,
                min = 1,
                max = chunk_size_limit
              ),
              numericInput(
                ns("draws"),
                lang()$t("Aantal trekkingen per tekst"),
                value = rv$draws,
                min = 1,
                max = 5
              )
            )
          },
          if (mode() == "Markeren") {
            list(
              # Add subtle text to explain chunking parameters
              description_box(
                lang()$t(
                  "Onderstaande parameters bepalen of en hoe de teksten worden gesplitst naar kleinere stukken in de 'markeren'-modus."
                )
              ),
              # Ask text split size & allowed overlap size (in tokens)
              numericInput(
                ns("max_tokens"),
                label = lang()$t("Maximale lengte per tekst (tokens)"),
                value = isolate(rv$max_tokens),
                min = 1,
                step = 1
              ),
              # Overlap
              # `overlap` argument to overlap chunks by a ratio (if < 1) or
              #   an absolute number of tokens (if >= 1)'
              numericInput(
                ns("overlap"),
                label = span(
                  lang()$t("Overlap tussen teksten (tokens)"),
                  tooltip(
                    bsicons::bs_icon("info-circle"),
                    paste0(
                      lang()$t(
                        "Waarde die de toegestane overlap tussen de teksten bepaalt."
                      ),
                      lang()$t(
                        " Een waarde tussen 0 en 1 wordt geïnterpreteerd als een ratio van de tekstlengte; een waarde groter dan 1 wordt geïnterpreteerd als een absoluut aantal tokens."
                      )
                    ),
                    placement = "bottom"
                  )
                ),
                value = isolate(rv$overlap),
                min = 0,
                step = 1
              )
            )
          }
        ))
      })

      # Show number of chunks and warnings, based on mode ------------
      # Show number of chunks
      output$n_chunks_display <- renderUI({
        req(mode() == "Onderwerpextractie")
        req(rv$n_chunks)
        return(div(
          class = "alert alert-info d-flex align-items-center mt-2",
          bsicons::bs_icon("blockquote-left"),
          span(
            class = "ms-2 fw",
            paste(lang()$t("Aantal chunks:"), rv$n_chunks)
          )
        ))
      })

      # Show warning if too many chunks
      output$too_many_chunks_warning <- renderUI({
        req(isTRUE(mode() == "Onderwerpextractie"))
        req(isTRUE(rv$too_many_chunks))
        return(div(
          class = "alert alert-danger d-flex align-items-center mt-2",
          bsicons::bs_icon("exclamation-triangle-fill"),
          span(
            class = "ms-2",
            paste0(
              lang()$t("Te veel chunks"),
              " (> ",
              number_of_chunks_limit,
              ")"
            )
          )
        ))
      })

      # Show warning if context window is too small for the texts
      output$fit_context_window_warning <- renderUI({
        req(length(texts$preprocessed) > 0)
        req(
          (!is.null(rv$base_prompt_text) |
            isTRUE(mode() == "Onderwerpextractie"))
        )

        if (isTRUE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-danger d-flex align-items-center mt-2",
            bsicons::bs_icon("exclamation-triangle-fill"),
            span(
              class = "ms-2",
              lang()$t("Sommige teksten zijn te lang voor het context-window")
            )
          ))
        }

        if (isFALSE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-success d-flex align-items-center mt-2",
            bsicons::bs_icon("check-circle-fill"),
            span(
              class = "ms-2",
              lang()$t("Alle teksten passen binnen het context-window")
            )
          ))
        }
      })

      # Disable when processing
      disable_when_processing(
        processing,
        c("context_window", "chunk_size", "draws", "max_tokens", "overlap")
      )

      return(rv)
    }
  )
}

# 3 Helper functions -----------------------------------------------

# 4 Example/development usage ----------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(tidyverse)
  library(bsicons)

  ui <- bslib::page(
    shinyjs::useShinyjs(),
    if (exists("css_js_head")) css_js_head(),
    div(
      class = "card-container",
      mode_ui("mode"),
      model_ui("models"),
      context_window_ui("context_window")
    )
  )

  server <- function(input, output, session) {
    mode <- reactiveVal("Categorisatie")

    models <- reactiveValues(
      main = tidyprompt::llm_provider_openai()$set_parameters(list(
        model = "gpt-4o-mini"
      )),
      large = tidyprompt::llm_provider_openai()$set_parameters(list(
        model = "gpt-4o-mini"
      ))
    )

    categories <- list(
      texts = reactiveVal(c(
        "positive review",
        "negative review",
        "neutral review"
      )),
      editing = reactiveVal(FALSE),
      unique_non_empty_count = reactiveVal(3)
    )
    codes <- list(
      texts = reactiveVal(c("positive", "negative", "neutral")),
      editing = reactiveVal(FALSE),
      unique_non_empty_count = reactiveVal(3)
    )
    scoring_characteristic <- reactiveVal("positive sentiment")
    research_background <- reactiveVal(
      "We have collected consumer reviews of our product."
    )
    assign_multiple_categories <- reactiveVal(FALSE)
    texts <- reactiveValues(
      preprocessed = c(
        "This is a positive review.",
        "This is a negative review.",
        "This is a neutral review."
      ),
      raw = c(
        "Dit is een positieve review.",
        "Dit is een negatieve review.",
        "Dit is een neutrale review."
      )
    )

    context_window_server(
      "context_window",
      mode = mode,
      models = models,
      categories = categories,
      scoring_characteristic = scoring_characteristic,
      codes = codes,
      research_background = research_background,
      assign_multiple_categories = assign_multiple_categories,
      texts = texts,
      processing = reactiveVal(FALSE),
      lang = default_lang()
    )
  }

  shinyApp(ui, server)
}
