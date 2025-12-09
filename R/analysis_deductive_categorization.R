# This script contains the prompt builder for categorization tasks,
#   as well as the UI and server logic for managing categories in the Shiny app

#### 1 Prompt builders ####

# Functions to build a prompt for categorizing a text into categories

#' Build prompt for categorizing a text into a single category
#'
#' @param text Text to categorize
#' @param research_background Background information about the research
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_category <- function(
  text,
  research_background,
  categories
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(categories),
    length(text) == 1,
    length(research_background) == 1,
    length(categories) > 0,
    !anyDuplicated(categories) > 0
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
    collapse = "\n  "
  )

  instruction <- paste0(
    "You need to categorize a text for a research project.",
    "\n\n"
  )
  if (research_background != "") {
    instruction <- paste0(
      instruction,
      "Research background:\n  ",
      research_background,
      "\n\n"
    )
  }
  instruction <- paste0(
    instruction,
    "Text:\n  '",
    text,
    "'\n\n",
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the number of the category that best describes the text.",
    "Choose a single category.",
    "\n",
    "(Use no other words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        # Check if number matches
        normalized <- trimws(tolower(x))
        if (normalized %in% as.character(seq_along(categories))) {
          return(categories[[as.integer(normalized)]])
        }

        # Sometimes, the model may return multiple numbers
        has_multiple_numbers <- function(normalized) {
          # tell strsplit to use the PCRE engine (perl = TRUE)
          tokens <- unlist(strsplit(normalized, "[,;/|\\s]+", perl = TRUE))

          # keep non-empty pieces, trim, and filter to integer-like strings
          numbers <- trimws(tokens[nzchar(tokens)])
          numbers <- numbers[grepl("^\\d+$", numbers)]

          length(numbers) > 1
        }
        if (has_multiple_numbers(normalized)) {
          return(tidyprompt::llm_feedback(paste0(
            "You must select only one valid category number.",
            "\nChoose the one category that best fits the text."
          )))
        }

        return(tidyprompt::llm_feedback(instruction))
      }
    )

  return(prompt)
}

#' Build prompt for categorizing a text into a single or multiple categories
#'
#' @param text Text to categorize
#' @param research_background Background information about the research
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_multi_category <- function(
  text = "this product is red",
  research_background = "",
  categories = c(
    "positive review",
    "negative review",
    "mentions color",
    "does not mention color",
    "unclear/not applicable"
  ),
  exclusive_categories = c(
    "unclear/not applicable"
  )
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(categories),
    length(text) == 1,
    length(research_background) == 1,
    length(categories) > 0,
    !anyDuplicated(categories) > 0,
    all(exclusive_categories %in% categories)
  )

  annotated_categories <- ifelse(
    categories %in% exclusive_categories,
    paste0(categories, " [exclusive]"),
    categories
  )

  numbered_categories <- paste0(
    seq_along(annotated_categories),
    ". ",
    annotated_categories,
    collapse = "\n  "
  )

  instruction <- "You need to categorize a text for a research project.\n\n"
  if (research_background != "") {
    instruction <- paste0(
      instruction,
      "Research background:\n  ",
      research_background,
      "\n\n"
    )
  }
  instruction <- paste0(
    instruction,
    "Text:\n  '",
    text,
    "'\n\n",
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the numbers of all categories that apply to this text, separated by commas.",
    "\n(E.g., \"1, 3, 5\" to select categories 1, 3, and 5.)",
    "\n(Use only numbers separated by commas, no extra words or characters.)"
  )

  if (length(exclusive_categories) > 0) {
    instruction <- paste0(
      instruction,
      "\n(If you choose an exclusive category",
      " (indicated with '[exclusive]'), ",
      "you may not choose any other categories.)"
    )
  }

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(tolower(x))
        numbers <- unlist(strsplit(normalized, "[,\\s]+"))
        valid_numbers <- numbers[
          numbers %in% as.character(seq_along(categories))
        ]
        if (length(valid_numbers) == 0) {
          return(tidyprompt::llm_feedback(
            "You must select at least one valid category number.",
            "Format your response as a comma-separated list of numbers (e.g., \"1, 3, 5\")."
          ))
        }
        categories_selected <- categories[as.integer(valid_numbers)]

        # Validate exclusive categories
        if (any(categories_selected %in% exclusive_categories)) {
          if (length(categories_selected) > 1) {
            return(tidyprompt::llm_feedback(paste0(
              "You have selected one or more of the exclusive categories (selected: '",
              paste(
                categories_selected[
                  categories_selected %in% exclusive_categories
                ],
                collapse = ", "
              ),
              "').",
              "\nWhen you select an exclusive category, you must select only one exclusive category and no other categories."
            )))
          }
        }

        return(
          jsonlite::toJSON(categories_selected, auto_unbox = FALSE)
        )
      }
    )

  return(prompt)
}


#### 2 Categories UI & server ####

categories_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("categories"))
  )
}

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
    shiny::exportTestValues(
      n_fields = fields$unique_non_empty_count(),
      txt_in_fields = fields$texts(),
      isEditing = fields$editing()
    )

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


#### 3 Example/development usage ####

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
