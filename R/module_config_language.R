# Language toggle module and default translator factory
# Used across all modules for internationalization

# 1 Default translator ----------------------------------------------------

#' Create default language translator reactiveVal
#'
#' Factory function to create a reactiveVal with the default translator.
#' Use this instead of repeating the full reactiveVal(...) pattern.
#'
#' @param reactive Logical, whether the returned Translator object should be reactive
#'
#' @return A reactiveVal containing a shiny.i18n::Translator
#' @export
#'
#' @examples
#' # In module server signature:
#' my_module_server <- function(id, lang = default_lang()) { ... }
default_lang <- function(reactive = TRUE) {
  lang <- shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  )

  if (reactive) {
    return(shiny::reactiveVal(lang))
  } else {
    return(lang)
  }
}


# 1 UI --------------------------------------------------------------------

language_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("language_ui"))
}


# 2 Server ----------------------------------------------------------------

language_server <- function(
  id,
  processing,
  can_toggle = getOption("language__can_toggle", TRUE)
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Store current language code separately
      current_lang_code <- reactiveVal(getOption("language", "nl"))

      # Initialize Translator object (updated on language change)
      lang <- reactive({
        translator <- shiny.i18n::Translator$new(
          translation_json_path = "language/language.json"
        )
        translator$set_translation_language(current_lang_code())
        translator
      })

      # UI rendering
      output$language_ui <- renderUI({
        req(isTRUE(can_toggle))
        tagList(
          shinyjs::useShinyjs(),
          div(
            class = "d-flex justify-content-center",
            shinyWidgets::radioGroupButtons(
              ns("toggle"),
              NULL,
              choices = setNames(c("en", "nl"), c("English", "Nederlands")),
              selected = current_lang_code(), # Use separate reactive value here
              size = "sm"
            )
          )
        )
      })

      # Update language code based on toggle
      observeEvent(input$toggle, {
        req(isTRUE(can_toggle))
        req(input$toggle %in% c("en", "nl"))
        current_lang_code(input$toggle)
        log_action("language_changed", details = input$toggle)
      })

      # Disable during processing
      disable_when_processing(processing, "toggle")

      return(lang)
    }
  )
}


# 3 Example/development usage ---------------------------------------------

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
