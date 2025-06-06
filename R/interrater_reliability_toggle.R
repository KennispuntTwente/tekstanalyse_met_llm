# Module for toggling inter-rater reliability

#### 1 UI ####

interrater_toggle_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::card(
      class = "card",
      card_header(lang$t("Inter-rater reliability")),
      card_body(
        # Toggle for inter-rater reliability
        p(lang$t("Zelf steekproef beoordelen?"), class = "mb-2 text-center"),
        div(
          class = "d-flex justify-content-center",
          shinyWidgets::radioGroupButtons(
            ns("interrater_reliability"),
            NULL,
            choices = c(
              lang$t("Nee"),
              lang$t("Ja")
            ),
            selected = lang$t("Nee"),
            size = "sm"
          )
        )
      )
    )
  )
}


#### 2 Server ####

interrater_toggle_server <- function(
  id,
  processing,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      interrater_reliability_toggle <- reactiveVal(FALSE)

      observeEvent(input$interrater_reliability, {
        interrater_reliability_toggle(
          input$interrater_reliability == lang$t("Ja")
        )
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "interrater_reliability",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(interrater_reliability_toggle)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)

  ui <- bslib::page(
    css_js_head(),
    interrater_toggle_ui("interrater_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    interrater_toggle_server("interrater_toggle", processing)
  }

  shinyApp(ui, server)
}
