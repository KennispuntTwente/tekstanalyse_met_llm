# Toggle for having LLM write paragraphs about categorized texts
# Input of this toggle is used in categorization & topic modelling modes,
#   to determine if the LLM should write summarizing texts about each category
#
# This module uses the reusable yes_no_toggle_card component with modal support

#### 1 UI ####

write_paragraphs_toggle_ui <- function(id) {
  yes_no_toggle_card_ui(id)
}


##### 2 Server ####

write_paragraphs_toggle_server <- function(
  id,
  processing,
  mode,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(id, function(input, output, session) {
    # Use the reusable component with modal configuration
    result <- yes_no_toggle_card_server(
      id = id,
      title = lang()$t("Rapport schrijven"),
      tooltip_text = lang()$t(
        "Indien je dit aanzet, zal het model per categorie een samenvattende paragraaf schrijven met quotes uit de bijbehorende teksten."
      ),
      question_text = lang()$t("Rapport schrijven over categorieën?"),
      default_value = TRUE,
      show_when = reactive(mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren")),
      modal_config = list(
        icon = "palette",
        tooltip = lang()$t("Stijlprompt voor samenvattingen"),
        title = lang()$t("Stijlprompt voor samenvattingen"),
        body = p(paste0(
          lang()$t("Hier kan je aangeven hoe het LLM de samenvattingen moet schrijven."),
          lang()$t(" Deze instructies worden meegegeven wanneer het LLM samenvattingen schrijft over categorieën of onderwerpen.")
        )),
        input_label = lang()$t("Geef aan hoe de samenvattingen geschreven moeten worden. Welke stijl of focus wil je?"),
        input_placeholder = lang()$t("Bijvoorbeeld: 'Schrijf in een formele, academische stijl' of 'Focus op emotionele aspecten van de teksten'")
      ),
      processing = processing,
      lang = lang
    )

    # Return in expected format
    return(list(
      write_paragraphs = reactive({
        if (isTRUE(mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren"))) {
          result$toggle()
        } else {
          FALSE
        }
      }),
      style_prompt = result$modal_value
    ))
  })
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    write_paragraphs_toggle_ui("write_paragraphs_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Categorisatie")

    write_paragraphs_toggle_server("write_paragraphs_toggle", processing, mode)
  }

  shinyApp(ui, server)
}
