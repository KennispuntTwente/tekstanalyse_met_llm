# Toggle for having LLM write paragraphs about categorized texts
# Input of this toggle is used in categorization & topic modelling modes,
#   to determine if the LLM should write summarizing texts about each category

#### 1 UI ####

write_paragraphs_toggle_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_toggle"))
}


##### 2 Server ####

write_paragraphs_toggle_server <- function(id, processing, mode) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      toggle <- reactiveVal(FALSE)

      # Only show in Categorisatie/Onderwerpextractie
      output$ui_toggle <- renderUI({
        req(mode() %in% c("Onderwerpextractie", "Categorisatie"))
        tagList(
          shinyjs::useShinyjs(),
          bslib::card(
            class = "card",
            card_header("Rapport schrijven"),
            card_body(
              # Toggle for inter-rater reliability
              p(
                "Rapport schrijven over categorieën?",
                class = "mb-2 text-center"
              ),
              div(
                class = "d-flex justify-content-center",
                shinyWidgets::radioGroupButtons(
                  ns("toggle"),
                  NULL,
                  choices = c("Nee", "Ja"),
                  selected = "Ja",
                  size = "sm"
                )
              )
            )
          )
        )
      })

      # Observe the toggle input and update the reactive value
      observeEvent(input$toggle, {
        toggle(input$toggle == "Ja")
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "toggle",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(toggle)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    write_paragraphs_toggle_ui("write_paragraphs_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Categorisatie")

    write_paragraphs_toggle_server("write_paragraphs_toggle", processing, mode)
  }

  shinyApp(ui, server)
}
