# Toggle for having LLM write paragraphs about categorized texts
# Input of this toggle is used in categorization & topic modelling modes,
#   to determine if the LLM should write summarizing texts about each category

#### 1 UI ####

language_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("language_ui"))
}


##### 2 Server ####

language_server <- function(
  id,
  processing
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Initialize translator object
      init_lang <- shiny.i18n::Translator$new(
        translation_json_path = "language/language.json"
      )
      init_lang$set_translation_language(
        getOption("language", "nl")
      )
      lang <- reactiveVal(init_lang)

      # Only show in Categorisatie/Onderwerpextractie
      output$language_ui <- renderUI({
        tagList(
          shinyjs::useShinyjs(),
          div(
            class = "d-flex justify-content-center",
            shinyWidgets::radioGroupButtons(
              ns("toggle"),
              NULL,
              choices = setNames(
                c("en", "nl"),
                c("English", "Nederlands")
              ),
              selected = isolate(lang()$get_translation_language()),
              size = "sm"
            )
          )
        )
      })

      # Observe the toggle input and update the reactive value
      observeEvent(input$toggle, {
        req(input$toggle)
        req(isTRUE(input$toggle %in% c("en", "nl")))
        new_lang <- lang()$clone()
        new_lang$set_translation_language(input$toggle)
        lang(new_lang)
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

      return(lang)
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
    language_ui("language"),
    textOutput("language_text")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    lang <- language_server("language", processing)

    output$language_text <- renderText({
      req(lang())
      lang()$get_translation_language()
    })
  }

  shinyApp(ui, server)
}
