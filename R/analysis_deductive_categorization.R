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
    !any(duplicated(categories))
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
    "\n",
    "(Use no other words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(tolower(x))
        if (normalized %in% as.character(seq_along(categories))) {
          return(categories[[as.integer(normalized)]])
        }
        match <- which(tolower(categories) == normalized)
        if (length(match) == 1) {
          return(categories[[match]])
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
    "does not mention color"
  )
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(categories),
    length(text) == 1,
    length(research_background) == 1,
    length(categories) > 0,
    !any(duplicated(categories))
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
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
    "\n",
    "(Use only numbers separated by commas, no extra words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(tolower(x))
        numbers <- unlist(strsplit(normalized, ",\\s*"))
        valid_numbers <- numbers[
          numbers %in% as.character(seq_along(categories))
        ]
        if (length(valid_numbers) == 0) {
          return(tidyprompt::llm_feedback(
            "You must select at least one valid category number."
          ))
        }
        categories_selected <- categories[as.integer(valid_numbers)]
        return(
          jsonlite::toJSON(categories_selected, auto_unbox = FALSE)
        )
      }
    )

  return(prompt)
}


#### 2 Categories UI & server ####

# User interface for entering categories, + server logic to manage them

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
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      n_fields <- reactiveVal(3)
      txt_in_fields <- reactiveVal(rep("", 3))
      isEditing <- reactiveVal(TRUE)

      output$category_fields <- renderUI({
        tagList(
          lapply(seq_len(n_fields()), function(i) {
            value <- txt_in_fields()[i] %||% ""
            textAreaInput(
              inputId = ns(paste0("category", i)),
              label = paste(lang()$t("Categorie"), i),
              value = value,
              rows = 1,
              width = "100%"
            )
          })
        )
      })

      output$editButtonUI <- renderUI({
        button_label <- if (isEditing()) icon("save") else icon("pencil")
        actionButton(
          ns("toggleEdit"),
          label = tagList(button_label, ""),
          class = "btn btn-primary",
          style = "min-width: 75px;"
        )
      })

      output$categories <- renderUI({
        if (mode() == "Categorisatie") {
          bslib::card(
            class = "card",
            card_header(lang()$t("Categorieën")),
            card_body(
              p(lang()$t(
                "Geef beknopte, duidelijke omschrijvingen. Overweeg een categorie 'Overig'/'Onbekend'/'Geen antwoord'."
              )),
              div(
                class = "category-button-container",
                actionButton(
                  ns("addCategory"),
                  label = icon("plus"),
                  class = "btn btn-success category-button",
                  style = "min-width: 75px;"
                ),
                actionButton(
                  ns("removeCategory"),
                  label = icon("minus"),
                  class = "btn btn-danger category-button",
                  style = "min-width: 75px;"
                ),
                uiOutput(ns("editButtonUI"))
              ),
              uiOutput(ns("category_fields"))
            )
          )
        }
      })

      observeEvent(input$addCategory, {
        if (isEditing()) {
          current_texts <- sapply(
            seq_len(n_fields()),
            function(i) input[[paste0("category", i)]],
            simplify = TRUE,
            USE.NAMES = FALSE
          )
          new_texts <- c(current_texts, "")
          txt_in_fields(new_texts)
          n_fields(length(new_texts))
        }
      })

      observeEvent(input$removeCategory, {
        if (isEditing()) {
          current_texts <- sapply(
            seq_len(n_fields()),
            function(i) input[[paste0("category", i)]],
            simplify = TRUE,
            USE.NAMES = FALSE
          )
          if (length(current_texts) > 1) {
            new_texts <- utils::head(current_texts, -1)
            txt_in_fields(new_texts)
            n_fields(length(new_texts))
          }
        }
      })

      observeEvent(input$toggleEdit, {
        if (isEditing()) {
          current_texts <- sapply(
            seq_len(n_fields()),
            function(i) {
              input[[paste0("category", i)]]
            },
            simplify = TRUE,
            USE.NAMES = FALSE
          )

          txt_in_fields(current_texts)
          isEditing(FALSE)

          shinyjs::disable("addCategory")
          shinyjs::disable("removeCategory")

          shinyjs::delay(0, {
            lapply(seq_len(n_fields()), function(i) {
              shinyjs::disable(paste0("category", i))
            })
          })
        } else {
          isEditing(TRUE)

          shinyjs::enable("addCategory")
          shinyjs::enable("removeCategory")

          shinyjs::delay(0, {
            lapply(seq_len(n_fields()), function(i) {
              shinyjs::enable(paste0("category", i))
            })
          })
        }
      })

      observe({
        if (processing()) {
          shinyjs::disable("addCategory")
          shinyjs::disable("removeCategory")
          shinyjs::disable("toggleEdit")
          lapply(seq_len(n_fields()), function(i) {
            shinyjs::disable(paste0("category", i))
          })
        }
      })

      observeEvent(lang(), {
        shinyjs::delay(100, {
          if (mode() == "Categorisatie") {
            if (!isEditing() || processing()) {
              lapply(seq_len(n_fields()), function(i) {
                shinyjs::disable(paste0("category", i))
              })
            } else {
              lapply(seq_len(n_fields()), function(i) {
                shinyjs::enable(paste0("category", i))
              })
            }

            # Re-render the edit button (pencil/save)
            output$editButtonUI <- renderUI({
              button_label <- if (isEditing()) icon("save") else icon("pencil")
              actionButton(
                ns("toggleEdit"),
                label = tagList(button_label, ""),
                class = "btn btn-primary",
                style = "min-width: 75px;"
              )
            })
          }
        })
      })

      # Also create reactive for unique, non-empty texts
      nonEmptyTexts <- reactive({
        values <- txt_in_fields()
        unique_values <- unique(trimws(values))
        unique_values[nzchar(unique_values)]
      })

      nonEmptyUniqueCount <- reactive({
        sum(nzchar(nonEmptyTexts()))
      })

      return(list(
        texts = nonEmptyTexts,
        editing = isEditing,
        unique_non_empty_count = nonEmptyUniqueCount
      ))
    }
  )
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
    uiOutput("categories_entered")
  )

  server <- function(input, output, session) {
    mode <- reactiveVal("Categorisatie")
    processing <- reactiveVal(FALSE)

    categories <- categories_server("categories_module", mode, processing)

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
      }
    })
  }

  shinyApp(ui, server)
}
