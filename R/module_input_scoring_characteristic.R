# User interface & server logic for entering scoring characteristic input
# Shows depending on the analysis mode selected by the user
# Is input for `analysis_deductive_scoring_characteristic.R`

# 1 UI --------------------------------------------------------------------

score_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("scoring")))
}


# 2 Server ----------------------------------------------------------------

score_server <- function(
  id,
  mode,
  processing,
  lang = default_lang()
) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      # Reactive values to store the scoring characteristic
      scoring_characteristic <- reactiveVal("")
      last_logged_scoring_characteristic <- reactiveVal(NULL)
      shiny::exportTestValues(
        scoring_characteristic = scoring_characteristic()
      )

      # Render scoring UI
      output$scoring <- renderUI({
        if (isTRUE(mode() == "Scoren")) {
          bslib::card(
            class = "card",
            card_header_with_tooltip(
              lang()$t("Karakteristiek"),
              lang()$t("Voer hier een karakteristiek (kenmerk) in.")
            ),
            card_body(
              paste0(lang()$t(
                "Geef de karakteristiek op waarop de tekst gescoord moet worden (bijv. 'emotionele lading', 'duidelijkheid')."
              )),
              HTML("<br>"),
              paste0(lang()$t(
                "De LLM zal een score tussen 0 en 100 geven, welke aangeeft hoe goed de tekst past bij de karakteristiek."
              )),
              textAreaInput(
                ns("scoring_characteristic"),
                NULL,
                value = isolate(input$scoring_characteristic) %||%
                  scoring_characteristic(),
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

      # Log changes, but avoid log spam: debounce and only log length
      scoring_characteristic_debounced <- shiny::debounce(
        reactive({
          if (is.null(input$scoring_characteristic)) {
            ""
          } else {
            input$scoring_characteristic
          }
        }),
        millis = 800
      )

      observeEvent(scoring_characteristic_debounced(), ignoreInit = TRUE, {
        if (!isTRUE(mode() == "Scoren")) {
          return()
        }

        val <- scoring_characteristic_debounced()
        if (is.null(val)) {
          val <- ""
        }

        if (identical(val, last_logged_scoring_characteristic())) {
          return()
        }
        last_logged_scoring_characteristic(val)

        log_action(
          "scoring_characteristic_set",
          details = sprintf("length=%d", nchar(val))
        )
      })

      # Disable input when processing
      disable_when_processing(processing, "scoring_characteristic")

      # Return reactive value with the scoring characteristic
      return(scoring_characteristic)
    }
  )
}


# 3 Example/development usage ---------------------------------------------

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
