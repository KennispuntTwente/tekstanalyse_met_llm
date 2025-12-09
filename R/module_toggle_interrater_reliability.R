# Module for toggling inter-rater reliability
#
# This module uses the reusable yes_no_toggle_card component

#### 1 UI ####

interrater_toggle_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    yes_no_toggle_card_ui(id)
  )
}


#### 2 Server ####

interrater_toggle_server <- function(
  id,
  processing,
  mode,
  lang = default_lang()
) {
  yes_no_toggle_card_server(
    id = id,
    title = lang()$t("Inter-rater reliability"),
    tooltip_text = paste0(
      lang()$t(
        "Wil je een steekproef trekken van de teksten om interrater-reliability te berekenen?"
      ),
      lang()$t(
        " Nadat het model de teksten heeft geanalyseerd, zal een venster openen waarin je zelf teksten kunt beoordelen."
      ),
      lang()$t(
        " Je beoordelingen worden vergeleken met die van het taalmodel (bij categorisatie/onderwerpextractie wordt Cohen's Kappa berekend; bij scoren wordt een paired t-test uitgevoerd)."
      )
    ),
    question_text = lang()$t("Zelf steekproef beoordelen?"),
    default_value = FALSE,
    show_when = reactive(
      isTRUE(mode() %in% c("Categorisatie", "Scoren", "Onderwerpextractie"))
    ),
    processing = processing,
    lang = lang
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
    mode <- reactiveVal("Categorisatie")

    interrater_toggle_server("interrater_toggle", processing, mode)
  }

  shinyApp(ui, server)
}
