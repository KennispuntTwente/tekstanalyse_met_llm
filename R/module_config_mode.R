# Module for selecting mode; categorization/scoring/topic extraction

# 1 UI ---------------------------------------------------------------
mode_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))
  )
}


# 2 Server ---------------------------------------------------------
mode_server <- function(
  id,
  processing,
  lang = default_lang()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      mode_values <- c(
        "Categorisatie",
        "Scoren",
        "Onderwerpextractie",
        "Markeren"
      )

      mode <- reactiveVal("Categorisatie")
      shiny::exportTestValues(mode = mode())

      output$card <- renderUI({
        mode_choices <- stats::setNames(
          mode_values,
          c(
            lang()$t("Categorisatie"),
            lang()$t("Scoren"),
            lang()$t("Onderwerpextractie"),
            lang()$t("Markeren")
          )
        )

        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Modus"),
            lang()$t(
              "Kies de gewenste analysemethode: categoriseren, scoren, onderwerpen extraheren of markeren."
            )
          ),
          card_body(
            div(
              class = "d-flex justify-content-center",
              shinyWidgets::radioGroupButtons(
                ns("mode"),
                NULL,
                choices = mode_choices,
                selected = mode(),
                size = "sm"
              )
            ),
            uiOutput(ns("mode_description_ui"))
          )
        )
      })

      # Reactive value which holds text message about the splitting progress
      #   (set from async process via the local queue object)
      output$mode_description_ui <- renderUI({
        req(mode())

        msg <- switch(
          mode(),
          "Categorisatie" = lang()$t(
            "Teksten worden door het model ingedeeld op basis van categorieën die jij opgeeft. Per categorie kan het model een samenvatting met quotes schrijven."
          ),
          "Scoren" = lang()$t(
            "Teksten worden door het model beoordeeld op een score (van 0 t/m 100) voor in hoeverre ze overeenkomen met een door jou opgegeven kenmerk."
          ),
          "Onderwerpextractie" = lang()$t(
            "Het model zal verschillende perspectieven extraheren uit de teksten, en de teksten hiernaar categoriseren. Per categorie kan het model een samenvatting met quotes schrijven."
          ),
          "Markeren" = paste0(
            lang()$t(
              "Het model zal per opgegeven code de relevante delen bij in teksten markeren. Bijvoorbeeld, bij code 'kleur' zou het model 'geel' markeren in de tekst 'de zon is geel'."
            ),
            lang()$t(
              " Deze modus is met name bedoeld voor langere teksten, zoals interviews. Het is bij deze modus niet nodig om teksten gesplitst te hebben naar kleinere stukken; dat gebeurt automatisch tijdens de analyse."
            ),
            lang()$t(
              " Het resultaat bevat een databestand, rapport en optioneel samenvattende alinea's."
            )
          )
        )

        description_box(msg, use_html = TRUE)
      })

      # When selecting input, update reactiveVal
      observeEvent(input$mode, {
        req(input$mode %in% mode_values)

        if (!identical(mode(), input$mode)) {
          mode(input$mode)
          log_action("mode_changed", details = input$mode)
        }
      })

      # When processing, disable the mode selection
      disable_when_processing(processing, "mode")

      return(mode)
    }
  )
}


# 3 Example/development usage --------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(bsicons)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    mode_ui("mode_module")
  )

  server <- function(input, output, session) {
    mode_server("mode_module", reactiveVal(FALSE))
  }

  shinyApp(ui, server)
}
