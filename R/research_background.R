# Module for entering research background
# Research background is a short description of the research context,
#   which will be used to provide context to the LLM in the prompts

#### 1 UI ####

research_background_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
}


#### 2 Server ####

research_background_server <- function(
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
      ns <- session$ns

      research_background <- reactiveVal("")
      shiny::exportTestValues(
        research_background = research_background()
      )

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Onderzoeksachtergrond"),
            paste0(
              lang()$t("Hier kan je een korte beschrijving van je onderzoek geven, zodat het LLM de teksten beter kan begrijpen."),
              lang()$t(" De beschrijving wordt meegegeven in alle prompts die naar het LLM worden gestuurd."),
              lang()$t(" Je kan bijvoorbeeld aangeven wat je onderzoekt en waar de teksten uit voortkomen (bijv., welke vraag hebben respondenten hebben beantwoord).")
            )
          ),
          card_body(
            p(paste0(
              lang()$t(
                "Beschrijf kort je onderzoek zodat het LLM wat context heeft."
              ),
              lang()$t(" Wat onderzoek je & hoe?")
            )),
            textAreaInput(
              ns("research_background"),
              NULL,
              value = "",
              rows = 3,
              width = "100%"
            )
          )
        )
      })

      # Observe input
      observeEvent(input$research_background, {
        research_background(input$research_background)
      })

      # Disable when processing
      disable_when_processing(processing, "research_background")

      return(research_background)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    research_background_ui("research_background_module")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    research_background <- research_background_server(
      "research_background_module",
      processing
    )

    observe({
      print(research_background())
    })
  }

  shinyApp(ui, server)
}
