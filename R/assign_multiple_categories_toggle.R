# Module for toggling if multiple categories can be assigned to a text

#### 1 Functions

assign_multiple_categories_toggle_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_toggle"))
}

assign_multiple_categories_toggle_server <- function(id, processing, mode) {
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
            card_header("Meerdere categorieën"),
            card_body(
              # Toggle for inter-rater reliability
              p("Meerdere categorieën per tekst toegestaan?", class = "mb-2 text-center"),
              div(
                class = "d-flex justify-content-center",
                shinyWidgets::radioGroupButtons(
                  ns("toggle"),
                  NULL,
                  choices = c("Nee", "Ja"),
                  selected = "Nee",
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

#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    assign_multiple_categories_toggle_ui("toggle_module")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Categorisatie")

    assign_multiple_categories_toggle_server("toggle_module", processing, mode)
  }

  shinyApp(ui, server)
}
