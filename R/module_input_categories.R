# Module for selecting and managing categories, which will be used
# by `analysis_deductive_categorization.R`

# 1 UI --------------------------------------------------------------------

categories_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("categories"))
  )
}


# 2 Server ----------------------------------------------------------------

categories_server <- function(
  id,
  mode,
  processing,
  assign_multiple_categories = reactiveVal(FALSE),
  lang = default_lang()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Use the reusable editable field list module
    fields <- editable_field_list_server(
      id = "fields",
      field_label = "Categorie",
      initial_count = 3,
      show_exclusive = assign_multiple_categories,
      processing = processing,
      lang = lang
    )

    # Re-export test values from child module
    observe({
      shiny::exportTestValues(
        n_fields = fields$unique_non_empty_count(),
        txt_in_fields = fields$texts(),
        isEditing = fields$editing()
      )
    })

    ## UI: Card wrapper ####
    output$categories <- renderUI({
      if (mode() == "Categorisatie") {
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Categorieën"),
            paste0(
              lang()$t(
                "Bewerk hier de categorieën waarin het taalmodel de teksten kan indelen."
              ),
              lang()$t(
                " Gebruik de '+'- en '-'-knoppen om categorieën toe te voegen of te verwijderen."
              ),
              lang()$t(
                " Gebruik tenslotte de save/edit-knop om de categorieën op te slaan (of weer te kunnen bewerken)."
              ),
              lang()$t(
                " In een verder blok hieronder kun je kiezen of het model meerdere categorieën mag toewijzen aan een tekst, of slechts één categorie."
              ),
              lang()$t(
                " Indien je het model meerdere categorieën laat toewijzen, kan je alsnog specifieke categorieën als 'exclusief' aanmerken."
              ),
              lang()$t(
                " Als een exlusieve categorie wordt toegewezen aan een tekst, mogen daarnaast geen andere categorieën worden toegewezen aan de tekst."
              )
            )
          ),
          card_body(
            p(lang()$t(
              "Geef beknopte, duidelijke omschrijvingen. Overweeg een categorie 'Overig'/'Onbekend'/'Geen antwoord'."
            )),
            editable_field_list_ui(ns("fields"))
          )
        )
      }
    })

    return(list(
      texts = fields$texts,
      editing = fields$editing,
      unique_non_empty_count = fields$unique_non_empty_count,
      exclusive_texts = fields$exclusive_texts
    ))
  })
}


# 3 Example/development usage ---------------------------------------------

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page_fluid(
    useShinyjs(),
    css_js_head(),
    categories_ui("categories_module"),
    assign_multiple_categories_toggle_ui("multiple"),
    uiOutput("categories_entered")
  )

  server <- function(input, output, session) {
    mode <- reactiveVal("Categorisatie")
    processing <- reactiveVal(FALSE)
    assign_multiple_categories <- assign_multiple_categories_toggle_server(
      "multiple",
      processing = reactiveVal(FALSE),
      mode = mode
    )

    categories <- categories_server(
      "categories_module",
      mode = mode,
      processing = processing,
      assign_multiple_categories = assign_multiple_categories
    )

    output$categories_entered <- renderPrint({
      if (categories$editing()) {
        "Currently editing categories."
      } else {
        cat("Entered categories:\n")
        cat(paste(categories$texts(), collapse = "\n"))
        cat(
          "\nUnique non-empty categories count: ",
          categories$unique_non_empty_count()
        )
        cat("\nExclusive categories selected:\n")
        cat(paste(
          categories$exclusive_texts(),
          collapse = "\n"
        ))
      }
    })
  }

  shinyApp(ui, server)
}
