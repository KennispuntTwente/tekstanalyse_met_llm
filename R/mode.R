# Module for selecting mode; categorization/scoring/topic modelling

#### 1 UI ####

mode_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      class = "card",
      card_header(
        "Modus",
        tooltip(
          bs_icon("info-circle"),
          "Kies de gewenste analysemethode: categoriseren, scoren of onderwerpen extraheren."
        )
      ),
      card_body(
        div(
          class = "d-flex justify-content-center",
          shinyWidgets::radioGroupButtons(
            ns("mode"),
            NULL,
            choices = c("Categorisatie", "Scoren", "Onderwerpextractie"),
            selected = "Categorisatie",
            size = "sm"
          )
        )
      )
    )
  )
}


#### 2 Server ####

mode_server <- function(id, processing) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      mode <- reactiveVal("Categorisatie")

      # When selecting input, update reactiveVal
      observeEvent(input$mode, {
        mode(input$mode)
      })

      # When processing, disable the mode selection
      observeEvent(processing(), {
        if (processing()) {
          shinyjs::disable("mode")
        } else {
          shinyjs::enable("mode")
        }
      })

      return(mode)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(bsicons)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    mode_ui("mode_module")
  )

  server <- function(input, output, session) {
    mode_server("mode_module", reactiveVal(FALSE))
  }

  shinyApp(ui, server)
}
