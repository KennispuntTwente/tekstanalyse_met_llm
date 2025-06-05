# Module for entering research background
# Research background is a short description of the research context,
#   which will be used to provide context to the LLM in the prompts

#### 1 UI ####

research_background_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    bslib::card(
      class = "card",
      card_header(lang$t("Onderzoeksachtergrond")),
      card_body(
        p(paste0(
          lang$t(
            "Beschrijf kort je onderzoek zodat het LLM wat context heeft."
          ),
          lang$t(" Wat onderzoek je & hoe? Wie gaf antwoord?")
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
  )
}


#### 2 Server ####

research_background_server <- function(id, processing) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      research_background <- reactiveVal("")

      # Observe input
      observeEvent(input$research_background, {
        research_background(input$research_background)
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            id = ns("research_background"),
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

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
