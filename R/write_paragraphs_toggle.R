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
  # Call the reusable component - note: passing raw strings, component will translate
  result <- yes_no_toggle_card_server(
    id = id,
    title = "Rapport schrijven",
    tooltip_text = "Indien je dit aanzet, zal het model per categorie een samenvattende paragraaf schrijven met quotes uit de bijbehorende teksten.",
    question_text = "Rapport schrijven over categorieën?",
    default_value = TRUE,
    show_when = reactive(mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren")),
    modal_config = list(
      icon = "palette",
      tooltip = "Stijlprompt voor samenvattingen",
      title = "Stijlprompt voor samenvattingen",
      body_text1 = "Hier kan je aangeven hoe het LLM de samenvattingen moet schrijven.",
      body_text2 = " Deze instructies worden meegegeven wanneer het LLM samenvattingen schrijft over categorieën of onderwerpen.",
      input_label = "Geef aan hoe de samenvattingen geschreven moeten worden. Welke stijl of focus wil je?",
      input_placeholder = "Bijvoorbeeld: 'Schrijf in een formele, academische stijl' of 'Focus op emotionele aspecten van de teksten'"
    ),
    translate_texts = TRUE,  # Tell component to translate the texts
    processing = processing,
    lang = lang
  )

  # Return in expected format (wrapped in moduleServer to get proper scoping)
  moduleServer(id, function(input, output, session) {
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
