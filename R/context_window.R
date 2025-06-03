# Module for context window and chunking parameters
# Ensures that the texts fit within the context window of the LLM

#### 1 UI ####

context_window_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "card-container",
      bslib::card(
        class = "card",
        card_header(
          "Context-window",
          tooltip(
            bs_icon("info-circle"),
            paste0(
              "Het context-window is de hoeveelheid tekst die het taalmodel kan verwerken in één keer.",
              " Er moet voor worden gezorgd dat de onderzoeksachtergrond met de (langste) tekst die je invoert binnen het context-window van het model past.",
              " Daarnaast worden bij de eerste stap van onderwerpextractie de teksten in chunks verdeeld; deze chunks moeten ook binnen het context-window passen.",
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
    )
  )
}


#### 2 Server ####

context_window_server <- function(
  id,
  mode = reactiveVal("Categorisatie"),
  models = reactiveValues(
    main = "gpt-4o-mini",
    large = "gpt-4o-mini"
  ),
  categories = list(
    texts = reactiveVal(c(
      "positive review",
      "negative review",
      "neutral review"
    )),
    editing = reactiveVal(FALSE),
    unique_non_empty_count = reactiveVal(3)
  ),
  scoring_characteristic = reactiveVal("positive sentiment"),
  research_background = reactiveVal(
    "We have collected consumer reviews of our product."
  ),
  assign_multiple_categories = reactiveVal(FALSE),
  texts = reactiveValues(
    preprocessed = c(
      "This is a positive review.",
      "This is a negative review.",
      "This is a neutral review. Very longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg texttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttVery longggggggggggggggggggggggggggggg textttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt"
    ),
    raw = c(
      "Dit is een positieve review.",
      "Dit is een negatieve review.",
      "Dit is een neutrale review."
    )
  ),
  processing = reactiveVal(FALSE)
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #### Reactive values ####
      rv <- reactiveValues(
        # State
        text_chunks = NULL,
        n_chunks = NULL,
        context_window_known = NULL,

        # Validity
        too_many_chunks = NULL,
        any_fit_problem = NULL,
        fit_context_window_chunks = NULL,
        fit_context_window_assigning = NULL,

        # Chunking parameters
        max_chunk_size = 50,
        max_redrawing = 1,
        n_tokens_context_window = 2048,
        n_char_base_prompt = NULL
      )

      #### Sync user input to internal state ####
      observe({
        is_valid_number <- function(input) {
          if (is.null(input)) {
            return(FALSE)
          }

          if (!is.numeric(input)) {
            return(FALSE)
          }

          if (input != round(input)) {
            return(FALSE)
          }

          if (input <= 0) {
            return(FALSE)
          }

          return(TRUE)
        }

        if (
          is_valid_number(input$chunk_size)
          && input$chunk_size <= 100
        ) {
          rv$max_chunk_size <- input$chunk_size
        }

        if (
          is_valid_number(input$redrawing)
          && input$redrawing <= 10
        ) {
          rv$max_redrawing <- input$redrawing
        }

        if (is_valid_number(input$context_window)) {
          rv$n_tokens_context_window <- input$context_window
        }
      })

      #### Obtain context window size based on model ####
      observe({
        req(models$main)
        size <- get_context_window_size_in_tokens(models$main)
        context_window_known <- is.null(size)

        size <- ifelse(
          is.null(size),
          2048,
          size
        )

        rv$n_tokens_context_window <- size
        rv$context_window_known <- context_window_known
      })

      #### Enable/disable input based on if context window is known ####
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

      #### Obtain number of characters in the base prompt, based on parameters ####
      # This is for categorization & scoring
      # (not candidate topic generation or writing paragraphs)
      observe({
        req(mode())
        req(!is.null(research_background()))
        prompt <- NULL

        if (mode() %in% c("Categorisatie", "Onderwerpextractie")) {
          if (mode() %in% c("Categorisatie")) {
            req(categories$texts())
            categories <- categories$texts()
          } else {
            # Onderwerpextractie, categories are defined by LLM
            # Going to assume ~ 500 characters, as a guess
            categories <- paste0("Category ", seq(1, 50))
          }

          if (assign_multiple_categories()) {
            prompt <- prompt_multi_category(
              text = "",
              research_background = research_background(),
              categories = categories
            )
          } else {
            prompt <- prompt_category(
              text = "",
              research_background = research_background(),
              categories = categories
            )
          }
        }

        if (mode() %in% c("Scoren")) {
          req(scoring_characteristic())

          prompt <- prompt_score(
            text = "",
            research_background = research_background(),
            scoring_characteristic = scoring_characteristic()
          )
        }

        if (!is.null(prompt)) {
          rv$n_char_base_prompt <-
            prompt |> tidyprompt::construct_prompt_text() |> nchar()
        } else {
          rv$n_char_base_prompt <- NULL
        }
      })

      #### Check if longest text + base prompt fit ####
      # (for categorizing, scoring)
      observe({
        req(mode() %in% c("Categorisatie", "Scoren", "Onderwerpextractie"))
        req(texts$preprocessed)
        req(rv$n_char_base_prompt)

        texts <- texts$preprocessed

        # Check if the longest text + base prompt fits in the context window
        # Ensure only one longest text is selected
        longest_text <- texts[which.max(nchar(texts))]
        total_length <- nchar(longest_text) + rv$n_char_base_prompt

        if (total_length > (rv$n_tokens_context_window * 4)) {
          rv$fit_context_window_assigning <- FALSE
        } else {
          rv$fit_context_window_assigning <- TRUE
        }
      })

      #### Make chunks & check if they fit ####
      observe({
        req(mode() == "Onderwerpextractie")
        req(texts$preprocessed)
        req(rv$n_tokens_context_window)
        req(rv$max_chunk_size)
        req(rv$max_redrawing)

        texts <- texts$preprocessed

        # Based on prompt for candidate topic generation; 600 characters + background
        n_char_base_prompt <- 600 + nchar(research_background())

        rv$text_chunks <- create_text_chunks(
          texts = texts,
          max_chunk_size = rv$max_chunk_size,
          max_redrawing = rv$max_redrawing,
          n_tokens_context_window = rv$n_tokens_context_window,
          n_char_base_prompt = n_char_base_prompt
        )

        if (is.null(rv$text_chunks)) {
          rv$fit_context_window_chunks <- FALSE
        } else {
          rv$fit_context_window_chunks <- TRUE
        }

        if (length(rv$text_chunks) > 100) {
          rv$too_many_chunks <- TRUE
        } else {
          rv$too_many_chunks <- FALSE
        }

        rv$n_chunks <- length(rv$text_chunks)
      })

      #### Check for presence of any fit problem ####
      observe({
        if (isTRUE(mode() == "Onderwerpextractie")) {
          if (
            isFALSE(rv$fit_context_window_chunks)
            || isFALSE(rv$fit_context_window_assigning)
          ) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }

        if (isTRUE(mode() %in% c("Categorisatie", "Scoren"))) {
          if (isFALSE(rv$fit_context_window_assigning)) {
            rv$any_fit_problem <- TRUE
          } else {
            rv$any_fit_problem <- FALSE
          }
        }
      })

      #### Show inputs (context window, chunking parameters), based on mode ####
      output$context_window_ui <- renderUI({
        req(mode() %in% c("Categorisatie", "Scoren", "Onderwerpextractie"))
        return(div(
          class = "d-flex flex-column align-items-center",
          numericInput(ns("context_window"), "Context-window grootte (# tokens)", value = rv$n_tokens_context_window, min = 0),
          if (mode() == "Onderwerpextractie") {
            list(
              numericInput(ns("chunk_size"), "Maximaal aantal teksten per chunk", value = rv$max_chunk_size, min = 1, max = 100),
              numericInput(ns("redrawing"), "Aantal trekkingen per tekst", value = rv$max_redrawing, min = 1, max = 5)
            )
          }
        ))
      })

      #### Show number of chunks and warnings, based on mode ####

      # Show number of chunks
      output$n_chunks_display <- renderUI({
        req(mode() == "Onderwerpextractie")
        req(rv$n_chunks)
        return(div(
          class = "alert alert-info d-flex align-items-center mt-2",
          bs_icon("blockquote-left"),
          span(class = "ms-2 fw", paste("Aantal chunks:", rv$n_chunks))
        ))
      })

      # Show warning if too many chunks
      output$too_many_chunks_warning <- renderUI({
        req(isTRUE(mode() == "Onderwerpextractie"))
        req(isTRUE(rv$too_many_chunks))
        return(div(
          class = "alert alert-danger d-flex align-items-center mt-2",
          bs_icon("exclamation-triangle-fill"),
          span(class = "ms-2", "Te veel chunks (> 100)")
        ))
      })

      # Show warning if context window is too small for the texts
      output$fit_context_window_warning <- renderUI({
        req(length(texts$preprocessed) > 0)
        req(
          (
            !is.null(rv$n_char_base_prompt)
            | isTRUE(mode() == "Onderwerpextractie")
          )
        )

        if (isTRUE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-danger d-flex align-items-center mt-2",
            bs_icon("exclamation-triangle-fill"),
            span(class = "ms-2", "Sommige teksten zijn te lang voor het context-window")
          ))
        }

        if (isFALSE(rv$any_fit_problem)) {
          return(div(
            class = "alert alert-success d-flex align-items-center mt-2",
            bs_icon("check-circle-fill"),
            span(class = "ms-2", "Alle teksten passen binnen het context-window")
          ))
        }
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "context_window",
            condition = !processing()
          )
          shinyjs::toggleState(
            "chunk_size",
            condition = !processing()
          )
          shinyjs::toggleState(
            "redrawing",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(rv)
    }
  )
}

#' Create text chunks
#'
#' @param texts A vector of texts to be chunked.
#' @param max_chunk_size Maximum number of texts in a chunk
#' @param max_redrawing Maximum number of times each text can be drawn into a chunk
#' @param n_tokens_context_window Number of tokens in the context window of the LLM
#' @param n_char_base_prompt Number of characters in the base prompt
#'
#' @return A list of text chunks, where each chunk is a vector of texts.
#' @export
create_text_chunks <- function(
    texts,
    max_chunk_size = 50,
    max_redrawing = 1, # new parameter: maximum number of times each text can be used,
    n_tokens_context_window = 2056,
    n_char_base_prompt = 600
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.numeric(max_chunk_size),
    max_chunk_size > 0,
    is.numeric(max_redrawing),
    max_redrawing > 0,
    is.numeric(n_tokens_context_window),
    n_tokens_context_window > 0,
    is.numeric(n_char_base_prompt),
    n_char_base_prompt > 0
  )

  n_char_context_window <- n_tokens_context_window * 4
  allowed_chars <- n_char_context_window - n_char_base_prompt

  # First check that each individual text does not exceed allowed_chars
  if (any(nchar(texts) > allowed_chars)) {
    # warning("One or more texts exceed the maximum allowed characters")
    return(NULL)
  }

  # If max_redrawing > 1, replicate each text accordingly so it can be redrawn.
  texts <- rep(texts, times = max_redrawing)

  # Randomize the order
  texts <- sample(texts)

  chunks <- list()
  current_chunk <- character(0)
  # current_total stores the effective character count (includes an extra 1 for each subsequent text added)
  current_total <- 0

  for (txt in texts) {
    # For texts that fit, consider whether to add to the current chunk or start a new one.
    # additional_cost is 1 if the current chunk is non-empty (to account for a newline)
    additional_cost <- if (length(current_chunk) > 0) 1 else 0
    new_total <- current_total + additional_cost + nchar(txt)

    # If adding the new text does not exceed allowed_chars and chunk size, append it.
    if (
      (new_total <= allowed_chars) && (length(current_chunk) < max_chunk_size)
    ) {
      current_chunk <- c(current_chunk, txt)
      current_total <- new_total
    } else {
      # Otherwise, flush the current chunk and start a new one with the new text.
      if (length(current_chunk) > 0) {
        chunks <- c(chunks, list(current_chunk))
      }
      current_chunk <- c(txt)
      current_total <- nchar(txt)
    }
  }

  # Flush any remaining texts in the current chunk
  if (length(current_chunk) > 0) {
    chunks <- c(chunks, list(current_chunk))
  }

  return(chunks)
}


#### 3 Helper functions ####

# Helper function with some hardcoded context window sizes for common models
# Will default to 2048 if the model is not recognized
# Better approach may be to retrieve via API or configuration file
get_context_window_size_in_tokens <- function(model) {
  if (model %in% c(
    "gpt-4.1-mini-2025-04-14",
    "gpt-4.1-2025-04-14",
    "gpt-4.1",
    "gpt-4.1-mini"
  ) ) {
    return(1047576)
  }

  if (model %in% c(
    "o4-mini-2025-04-16",
    "o3-2025-04-16",
    "o3-mini-2025-01-31",
    "o1-2024-12-17",
    "o1-pro-2025-03-19",
    "o4-mini",
    "o3",
    "o3-mini",
    "o1",
    "o1-pro"
  )) {
    return(200000)
  }

  if (model %in% c(
    "gpt-4o-2024-08-06",
    "chatgpt-4o-latest",
    "gpt-4o-mini-2024-07-18",
    "gpt-4o-mini",
    "gpt-4o"

  )) {
    return(128000)
  }

  if (model %in% c(
    "gpt-3.5-turbo-0125"
  )) {
    return(4096)
  }

  return(NULL)
}

#### 4 Example/development usage ####

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
    mode <- mode_server("mode", processing = reactiveVal(FALSE))
    models <- model_server(
      "models",
      processing = reactiveVal(FALSE),
      mode = mode,
      llm_provider = tidyprompt::llm_provider_openai(),
      available_main_models = c(
        "gpt-4o-mini-2024-07-18", "gpt-4.1-mini-2025-04-14", "some model"
      ),
      available_large_models = c(
        "gpt-4o-mini-2024-07-18", "gpt-4.1-mini-2025-04-14", "some model"
      )
    )
    context_window_server(
      "context_window",
      mode = mode,
      models = models
    )
  }

  shinyApp(ui, server)
}
