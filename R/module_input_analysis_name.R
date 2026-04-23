# Module for entering an analysis name
# The analysis name is an optional label for this run, included in the report,
# the download zip filename, metadata.json, and the Excel export.

# 1 UI ---------------------------------------------------------------
analysis_name_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
}


# 2 Server ---------------------------------------------------------
analysis_name_server <- function(
  id,
  processing,
  lang = default_lang()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      analysis_name <- reactiveVal("")
      shiny::exportTestValues(
        analysis_name = analysis_name()
      )

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Naam van analyse"),
            lang()$t(
              "Geef een naam aan deze analyse. De naam wordt opgenomen in het rapport en de bestandsnaam van de download."
            )
          ),
          card_body(
            textInput(
              ns("analysis_name"),
              NULL,
              value = isolate(input$analysis_name) %||% analysis_name(),
              width = "100%",
              placeholder = lang()$t(
                "Vul een naam in (optioneel)..."
              )
            )
          )
        )
      })

      # Observe input
      observeEvent(input$analysis_name, {
        analysis_name(trimws(input$analysis_name))
        log_action(
          "analysis_name_changed",
          details = sprintf("length=%d", nchar(input$analysis_name))
        )
      })

      # Disable when processing
      disable_when_processing(processing, "analysis_name")

      return(analysis_name)
    }
  )
}
