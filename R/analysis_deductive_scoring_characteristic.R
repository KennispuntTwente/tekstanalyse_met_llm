# This script contains the prompt builder for scoring a text based on a characteristic,
#   as well as the UI and server logic for the scoring characteristic input in
#   the Shiny app

#### 1 Prompt builder ####

# Function to build a prompt for scoring a text based on a characteristic

#' Build prompt for scoring a text
#'
#' @param text Text to score
#' @param research_background Background information about the research
#' @param scoring_characteristic Characteristic to score the text on
#' (e.g., "emotional load", "clarity")
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_score <- function(
  text,
  research_background,
  scoring_characteristic
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(scoring_characteristic),
    length(text) == 1,
    length(research_background) == 1,
    length(scoring_characteristic) == 1
  )

  instruction <- glue::glue(
    "You need to score a text for a research project.\n\n",
    "Research background:\n  {research_background}\n\n",
    "Text:\n  '{text}'",
    "\n\n",
    "Characteristic to score the text on:\n  {scoring_characteristic}",
    "\n\n",
    "Respond with a score (0-100) which tells how well the text fits the characteristic.",
    "\n",
    "(Where 0 means the text does not fit the characteristic at all and 100 means it fits perfectly.)",
    "\n",
    "(Use no other words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(x)
        score <- suppressWarnings(as.numeric(normalized))
        if (!is.na(score) && score >= 0 && score <= 100) {
          return(score)
        }
        return(tidyprompt::llm_feedback(instruction))
      }
    )

  return(prompt)
}


#### 2 Scoring characteristic UI & server ####

# User interface & server logic for entering scoring characteristic input
# Shows depending on the mode selected by the user

score_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("scoring")))
}

score_server <- function(id, mode, processing) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      # Reactive values to store the scoring characteristic
      scoring_characteristic <- reactiveVal("")

      # Render scoring UI
      output$scoring <- renderUI({
        if (mode() == "Scoren") {
          bslib::card(
            class = "card",
            card_header("Karakteristiek"),
            card_body(
              paste0(
                "Geef de karakteristiek op waarop de tekst gescoord moet worden (bijv. 'emotionele lading', 'duidelijkheid')."
              ),
              HTML("<br>"),
              paste0(
                "De LLM zal een score tussen 0 en 100 geven, welke aangeeft hoe goed de tekst past bij de karakteristiek."
              ),
              textAreaInput(
                ns("scoring_characteristic"),
                NULL,
                value = "",
                rows = 1,
                width = "100%"
              )
            )
          )
        }
      })

      # Update scoring characteristic when input changes
      observeEvent(input$scoring_characteristic, {
        scoring_characteristic(input$scoring_characteristic)
      })

      # When processing, disable all input fields
      observe({
        if (processing()) {
          shinyjs::disable("scoring_characteristic")
        }
      })

      # Return reactive value with the scoring characteristic
      return(scoring_characteristic)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(tidyprompt)
  library(glue)

  # Example usage in a Shiny app
  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    score_ui("scoring"),
    textOutput("scoring_characteristic_value")
  )

  server <- function(input, output, session) {
    mode <- reactiveVal("Scoren")
    processing <- reactiveVal(FALSE)

    scoring_characteristic <- score_server("scoring", mode, processing)

    output$scoring_characteristic_value <- renderText({
      scoring_characteristic()
    })
  }

  shinyApp(ui, server)
}
