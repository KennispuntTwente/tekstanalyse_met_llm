# Editable Field List Module
# Reusable Shiny module for dynamic text input fields with add/remove/edit functionality
# Used by: categories (categorization mode), codes (marking mode)

# 1 UI --------------------------------------------------------------------

editable_field_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("fields_container"))
  )
}


# 2 Server ----------------------------------------------------------------

#' Editable Field List Server
#'
#' @param id Module ID
#' @param field_label Label for each field (e.g., "Categorie", "Code")
#' @param initial_count Initial number of fields
#' @param show_exclusive Show exclusive checkbox per field (for multi-category mode)
#' @param processing Reactive value for processing state
#' @param lang Language translator reactive
#'
#' @return List with texts, editing, unique_non_empty_count, exclusive_texts reactives
editable_field_list_server <- function(
  id,
  field_label = "Field",
  initial_count = 3,
  show_exclusive = reactiveVal(FALSE),
  processing = reactiveVal(FALSE),
  lang = default_lang()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State ----------------------------------------------------------
    n_fields <- reactiveVal(initial_count)
    txt_in_fields <- reactiveVal(rep("", initial_count))
    exclusive_vals <- reactiveVal(rep(FALSE, initial_count))
    isEditing <- reactiveVal(TRUE)

    shiny::exportTestValues(
      n_fields = n_fields(),
      txt_in_fields = txt_in_fields(),
      exclusive_sel = exclusive_vals(),
      isEditing = isEditing()
    )

    # UI rendering ---------------------------------------------------
    # Individual fields
    output$field_inputs <- renderUI({
      show_excl <- isTRUE(show_exclusive()) || identical(show_exclusive, TRUE)

      tagList(lapply(seq_len(n_fields()), function(i) {
        value <- txt_in_fields()[i] %||% ""
        excl_value <- exclusive_vals()[i] %||% FALSE

        fluidRow(
          column(
            width = if (show_excl) 10 else 12,
            textAreaInput(
              ns(paste0("field", i)),
              label = paste(lang()$t(field_label), i),
              value = value,
              rows = 1,
              width = "100%"
            )
          ),
          if (show_excl) {
            column(
              width = 2,
              checkboxInput(
                ns(paste0("exclusive", i)),
                label = lang()$t("Exclusief"),
                value = excl_value
              )
            )
          }
        )
      }))
    })

    # Edit button
    output$editButtonUI <- renderUI({
      button_label <- if (isEditing()) icon("save") else icon("pencil")
      actionButton(
        ns("toggleEdit"),
        label = tagList(button_label, ""),
        class = "btn btn-primary",
        style = "min-width: 75px;"
      )
    })

    # Container with buttons and fields
    output$fields_container <- renderUI({
      tagList(
        div(
          class = "category-button-container",
          actionButton(
            ns("addField"),
            label = icon("plus"),
            class = "btn btn-success category-button",
            style = "min-width: 75px;"
          ),
          actionButton(
            ns("removeField"),
            label = icon("minus"),
            class = "btn btn-danger category-button",
            style = "min-width: 75px;"
          ),
          uiOutput(ns("editButtonUI"))
        ),
        uiOutput(ns("field_inputs"))
      )
    })

    # Sync state from inputs -----------------------------------------
    observe({
      req(isEditing())
      show_excl <- isTRUE(show_exclusive()) || identical(show_exclusive, TRUE)

      # Sync text values
      txt_in_fields(sapply(
        seq_len(n_fields()),
        function(i) {
          isolate(input[[paste0("field", i)]]) %||% txt_in_fields()[i]
        },
        simplify = TRUE,
        USE.NAMES = FALSE
      ))

      # Sync exclusive checkboxes
      if (show_excl) {
        exclusive_vals(sapply(
          seq_len(n_fields()),
          function(i) input[[paste0("exclusive", i)]] %||% exclusive_vals()[i],
          simplify = TRUE,
          USE.NAMES = FALSE
        ))
      }
    })

    # Add/remove fields ----------------------------------------------
    observeEvent(input$addField, {
      req(isEditing())
      txt_in_fields(c(txt_in_fields(), ""))
      exclusive_vals(c(exclusive_vals(), FALSE))
      n_fields(n_fields() + 1)
    })

    observeEvent(input$removeField, {
      req(isEditing(), n_fields() > 1)
      txt_in_fields(utils::head(txt_in_fields(), -1))
      exclusive_vals(utils::head(exclusive_vals(), -1))
      n_fields(n_fields() - 1)
    })

    # Toggle edit/save -----------------------------------------------
    observeEvent(input$toggleEdit, {
      if (isEditing()) {
        # SAVE
        txt_in_fields(sapply(
          seq_len(n_fields()),
          function(i) input[[paste0("field", i)]] %||% txt_in_fields()[i]
        ))
        show_excl <- isTRUE(show_exclusive()) || identical(show_exclusive, TRUE)
        if (show_excl) {
          exclusive_vals(sapply(
            seq_len(n_fields()),
            function(i) input[[paste0("exclusive", i)]] %||% exclusive_vals()[i]
          ))
        }
        isEditing(FALSE)
        shinyjs::disable("addField")
        shinyjs::disable("removeField")
      } else {
        # EDIT
        isEditing(TRUE)
        shinyjs::enable("addField")
        shinyjs::enable("removeField")
      }
    })

    # Disable when processing ----------------------------------------
    update_input_state <- function() {
      show_excl <- isTRUE(show_exclusive()) || identical(show_exclusive, TRUE)

      lapply(seq_len(n_fields()), function(i) {
        txt_id <- paste0("field", i)
        ex_id <- paste0("exclusive", i)
        if (!isEditing() || isTRUE(processing())) {
          shinyjs::disable(txt_id)
          if (show_excl) shinyjs::disable(ex_id)
        } else {
          shinyjs::enable(txt_id)
          if (show_excl) shinyjs::enable(ex_id)
        }
      })

      if (!isEditing() || isTRUE(processing())) {
        shinyjs::disable("addField")
        shinyjs::disable("removeField")
        shinyjs::disable("toggleEdit")
      } else {
        shinyjs::enable("addField")
        shinyjs::enable("removeField")
        shinyjs::enable("toggleEdit")
      }
    }

    observe({
      show_exclusive()
      isEditing()
      processing()
      n_fields()
      shinyjs::delay(50, update_input_state())
    })

    # Return values --------------------------------------------------
    # Non-empty unique texts
    nonEmptyTexts <- reactive({
      vals <- txt_in_fields()
      trimmed <- trimws(vals)
      trimmed <- trimmed[nzchar(trimmed)]
      unique(trimmed)
    })

    nonEmptyUniqueCount <- reactive({
      length(nonEmptyTexts())
    })

    # Exclusive flags (respects show_exclusive)
    exclusive_flags <- reactive({
      show_excl <- isTRUE(show_exclusive()) || identical(show_exclusive, TRUE)
      if (!show_excl) {
        rep(TRUE, n_fields())
      } else {
        exclusive_vals()
      }
    })

    # Texts that are marked exclusive
    exclusive_texts <- reactive({
      txt_in_fields()[exclusive_flags()]
    })

    # Method to programmatically set field values (e.g., after code generation)
    set_values <- function(values) {
      if (length(values) == 0) {
        return()
      }
      txt_in_fields(values)
      n_fields(length(values))
      isEditing(TRUE)

      # Update UI inputs to match
      shinyjs::delay(100, {
        lapply(seq_along(values), function(i) {
          shiny::updateTextAreaInput(
            session,
            paste0("field", i),
            value = values[i] %||% ""
          )
        })
      })
    }

    return(list(
      texts = nonEmptyTexts,
      editing = isEditing,
      unique_non_empty_count = nonEmptyUniqueCount,
      exclusive_texts = exclusive_texts,
      set_values = set_values
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
    bslib::card(
      card_header("Test Editable Fields"),
      card_body(
        editable_field_list_ui("test_fields")
      )
    ),
    verbatimTextOutput("debug")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    show_exclusive <- reactiveVal(TRUE)

    fields <- editable_field_list_server(
      "test_fields",
      field_label = "Categorie",
      initial_count = 2,
      show_exclusive = show_exclusive,
      processing = processing
    )

    output$debug <- renderPrint({
      list(
        texts = fields$texts(),
        editing = fields$editing(),
        count = fields$unique_non_empty_count(),
        exclusive = fields$exclusive_texts()
      )
    })
  }

  shinyApp(ui, server)
}
