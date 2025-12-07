# Toggle for having LLM write paragraphs about categorized texts
# Input of this toggle is used in categorization & topic modelling modes,
#   to determine if the LLM should write summarizing texts about each category

#### 1 UI ####

write_paragraphs_toggle_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_toggle"))
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
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      toggle <- reactiveVal(FALSE)
      style_prompt <- reactiveVal("")

      # Only show in Categorisatie/Onderwerpextractie
      output$ui_toggle <- renderUI({
        req(mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren"))
        tagList(
          shinyjs::useShinyjs(),
          bslib::card(
            class = "card",
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center w-100",
                span(
                  lang()$t("Rapport schrijven"),
                  bslib::tooltip(
                    bsicons::bs_icon("info-circle"),
                    lang()$t(
                      "Indien je dit aanzet, zal het model per categorie een samenvattende paragraaf schrijven met quotes uit de bijbehorende teksten."
                    )
                  )
                ),
                # Style prompt button in top right
                uiOutput(ns("style_button"))
              )
            ),
            card_body(
              # Toggle for inter-rater reliability
              p(
                lang()$t("Rapport schrijven over categorieën?"),
                class = "mb-2 text-center"
              ),
              div(
                class = "d-flex justify-content-center",
                shinyWidgets::radioGroupButtons(
                  ns("toggle"),
                  NULL,
                  choices = c(
                    lang()$t("Nee"),
                    lang()$t("Ja")
                  ),
                  selected = lang()$t("Ja"),
                  size = "sm"
                )
              )
            )
          )
        )
      })

      # Render style button only when toggle is "Ja"
      output$style_button <- renderUI({
        # Dynamic colour: blue if there's a custom style prompt, gray otherwise
        has_style <- nzchar(style_prompt())
        style <- if (has_style) "color:#0d6efd;" else "color:#6c757d;"
        style <- paste0(
          style,
          "font-size:1rem; border:none; background:transparent;"
        )

        actionLink(
          ns("show_style_modal"),
          icon("palette", lib = "font-awesome"),
          style = style
        ) |>
          bslib::tooltip(lang()$t("Stijlprompt voor samenvattingen"))
      })

      # Show style prompt modal
      observeEvent(input$show_style_modal, {
        showModal(
          modalDialog(
            title = tagList(
              icon("palette"),
              " ",
              lang()$t("Stijlprompt voor samenvattingen")
            ),
            div(
              p(paste0(
                lang()$t(
                  "Hier kan je aangeven hoe het LLM de samenvattingen moet schrijven."
                ),
                lang()$t(
                  " Deze instructies worden meegegeven wanneer het LLM samenvattingen schrijft over categorieën of onderwerpen."
                )
              )),
              textAreaInput(
                ns("style_prompt_input"),
                lang()$t(
                  "Geef aan hoe de samenvattingen geschreven moeten worden. Welke stijl of focus wil je?"
                ),
                value = style_prompt(),
                rows = 4,
                width = "100%",
                placeholder = lang()$t(
                  "Bijvoorbeeld: 'Schrijf in een formele, academische stijl' of 'Focus op emotionele aspecten van de teksten'"
                )
              )
            ),
            footer = tagList(
              tags$div(
                style = "display:flex; width:100%; align-items:center;",
                tags$div(
                  style = "flex:1; text-align:left;",
                  modalButton(lang()$t("Sluiten"))
                ),
                tags$div(
                  style = "flex:1; text-align:center;",
                  actionButton(
                    ns("discard_style"),
                    lang()$t("Reset"),
                    class = "btn-danger"
                  )
                ),
                tags$div(
                  style = "flex:1; text-align:right;",
                  actionButton(
                    ns("save_style"),
                    lang()$t("Sla op"),
                    class = "btn-primary"
                  )
                )
              )
            ),
            size = "m",
            easyClose = TRUE
          )
        )

        # If processing, disable text field & also disable the save/reset buttons
        if (isTRUE(processing())) {
          shinyjs::disable("style_prompt_input")
          shinyjs::disable("save_style")
          shinyjs::disable("discard_style")
        } else {
          shinyjs::enable("style_prompt_input")
          shinyjs::enable("save_style")
          shinyjs::enable("discard_style")
        }
      })

      # Save style prompt
      observeEvent(input$save_style, {
        style_prompt(input$style_prompt_input)
        removeModal()
      })

      # Discard style prompt: clear and close modal
      observeEvent(input$discard_style, {
        style_prompt("") # clear stored prompt
        updateTextAreaInput(
          session,
          "style_prompt_input",
          value = ""
        )
        removeModal()
      })

      # Observe the toggle input and update the reactive value
      observeEvent(input$toggle, {
        toggle(input$toggle == lang()$t("Ja"))
      })

      # Disable when processing
      observeEvent(
        processing(),
        {
          shinyjs::toggleState(
            "toggle",
            condition = !processing()
          )
          shinyjs::toggleState(
            "show_style_modal",
            condition = !processing()
          )
          shinyjs::toggleState(
            "style_prompt_input",
            condition = !processing()
          )
        },
        ignoreInit = TRUE
      )

      return(list(
        write_paragraphs = reactive({
          if (
            isTRUE(
              mode() %in% c("Onderwerpextractie", "Categorisatie", "Markeren")
            )
          ) {
            toggle()
          } else {
            FALSE
          }
        }),
        style_prompt = style_prompt
      ))
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
    write_paragraphs_toggle_ui("write_paragraphs_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Categorisatie")

    write_paragraphs_toggle_server("write_paragraphs_toggle", processing, mode)
  }

  shinyApp(ui, server)
}
