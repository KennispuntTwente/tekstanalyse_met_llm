# Module for toggling if multiple categories can be assigned to a text
#
# This module uses the reusable yes_no_toggle_card component

#### 1 UI ####

assign_multiple_categories_toggle_ui <- function(id) {
  yes_no_toggle_card_ui(id)
}


#### 2 Server ####

assign_multiple_categories_toggle_server <- function(
  id,
  processing,
  mode,
  lang = default_lang()
) {
  yes_no_toggle_card_server(
    id = id,
    title = lang()$t("Meerdere categorieën"),
    tooltip_text = paste0(
      lang()$t("Mag het model meerdere categorieën toekennen aan een tekst, of slechts één categorie?"),
      lang()$t(" Indien je het model meerdere categorieën laat toewijzen, kan je alsnog specifieke categorieën als 'exclusief' aanmerken."),
      lang()$t(" Als een exlusieve categorie wordt toegewezen aan een tekst, mogen daarnaast geen andere categorieën worden toegewezen aan de tekst."),
      lang()$t(" Je kunt categorieën exclusief maken in de categorie-editor (modus 'categorisatie') of bij het bewerken van de onderwerpen (modus 'onderwerpextractie'; zet 'human-in-the-loop' aan).")
    ),
    question_text = lang()$t("Meerdere categorieën per tekst toegestaan?"),
    default_value = TRUE,
    show_when = reactive(mode() %in% c("Onderwerpextractie", "Categorisatie")),
    processing = processing,
    lang = lang
  )
}


#### 3 Example/development usage ####

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
