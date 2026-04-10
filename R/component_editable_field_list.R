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

    prev_exclusive_vals <- reactiveVal(rep(FALSE, initial_count))

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
      fields_disabled <- !isTRUE(isEditing()) || isTRUE(processing())

      tagList(lapply(seq_len(n_fields()), function(i) {
        value <- txt_in_fields()[i] %||% ""
        excl_value <- exclusive_vals()[i] %||% FALSE
        text_input <- textAreaInput(
          ns(paste0("field", i)),
          label = paste(lang()$t(field_label), i),
          value = value,
          rows = 1,
          width = "100%"
        )

        if (fields_disabled) {
          text_input <- shinyjs::disabled(text_input)
        }

        fluidRow(
          column(
            width = if (show_excl) 10 else 12,
            text_input
          ),
          if (show_excl) {
            exclusive_input <- checkboxInput(
              ns(paste0("exclusive", i)),
              label = lang()$t("Exclusief"),
              value = excl_value
            )

            if (fields_disabled) {
              exclusive_input <- shinyjs::disabled(exclusive_input)
            }

            column(
              width = 2,
              exclusive_input
            )
          }
        )
      }))
    })

    # Edit button
    output$editButtonUI <- renderUI({
      button_label <- if (isEditing()) icon("save") else icon("pencil")
      edit_button <- actionButton(
        ns("toggleEdit"),
        label = tagList(button_label, ""),
        class = "btn btn-primary",
        style = "min-width: 75px;"
      )

      if (isTRUE(processing())) {
        edit_button <- shinyjs::disabled(edit_button)
      }

      edit_button
    })

    # Container with buttons and fields
    output$fields_container <- renderUI({
      buttons_disabled <- !isTRUE(isEditing()) || isTRUE(processing())
      add_button <- actionButton(
        ns("addField"),
        label = icon("plus"),
        class = "btn btn-success category-button",
        style = "min-width: 75px;"
      )
      remove_button <- actionButton(
        ns("removeField"),
        label = icon("minus"),
        class = "btn btn-danger category-button",
        style = "min-width: 75px;"
      )

      if (buttons_disabled) {
        add_button <- shinyjs::disabled(add_button)
        remove_button <- shinyjs::disabled(remove_button)
      }

      tagList(
        div(
          class = "category-button-container",
          add_button,
          remove_button,
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
        new_exclusive <- sapply(
          seq_len(n_fields()),
          function(i) input[[paste0("exclusive", i)]] %||% exclusive_vals()[i],
          simplify = TRUE,
          USE.NAMES = FALSE
        )

        prev <- prev_exclusive_vals() %||% rep(FALSE, length(new_exclusive))
        if (length(prev) != length(new_exclusive)) {
          prev <- rep(FALSE, length(new_exclusive))
        }

        changed_idx <- which(prev != new_exclusive)
        if (length(changed_idx) > 0) {
          log_action(
            "exclusive_changed",
            details = sprintf(
              "field=%s indices=%s values=%s",
              field_label,
              paste(changed_idx, collapse = ","),
              paste(new_exclusive[changed_idx], collapse = ",")
            )
          )
        }

        exclusive_vals(new_exclusive)
        prev_exclusive_vals(new_exclusive)
      }
    })

    # Add/remove fields ----------------------------------------------
    observeEvent(input$addField, {
      req(isEditing())
      txt_in_fields(c(txt_in_fields(), ""))
      exclusive_vals(c(exclusive_vals(), FALSE))
      n_fields(n_fields() + 1)

      prev_exclusive_vals(exclusive_vals())

      log_action(
        "field_added",
        details = sprintf("field=%s n_fields=%d", field_label, n_fields())
      )
    })

    observeEvent(input$removeField, {
      req(isEditing(), n_fields() > 1)
      txt_in_fields(utils::head(txt_in_fields(), -1))
      exclusive_vals(utils::head(exclusive_vals(), -1))
      n_fields(n_fields() - 1)

      prev_exclusive_vals(exclusive_vals())

      log_action(
        "field_removed",
        details = sprintf("field=%s n_fields=%d", field_label, n_fields())
      )
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
        # Log config saved
        log_action(
          "config_saved",
          details = sprintf(
            "field=%s, n_items=%d",
            field_label,
            length(nonEmptyTexts())
          )
        )
      } else {
        # EDIT
        isEditing(TRUE)
        shinyjs::enable("addField")
        shinyjs::enable("removeField")

        log_action(
          "config_edit_enabled",
          details = sprintf("field=%s n_fields=%d", field_label, n_fields())
        )
      }
    })

    # Disable when processing ----------------------------------------
    update_input_state <- function() {
      show_excl <- if (is.function(show_exclusive)) {
        isTRUE(shiny::isolate(show_exclusive()))
      } else {
        isTRUE(show_exclusive)
      }

      editing_now <- shiny::isolate(isEditing())
      processing_now <- shiny::isolate(processing())
      fields_now <- shiny::isolate(n_fields())

      lapply(seq_len(fields_now), function(i) {
        txt_id <- paste0("field", i)
        ex_id <- paste0("exclusive", i)
        if (!editing_now || isTRUE(processing_now)) {
          shinyjs::disable(txt_id)
          if (show_excl) shinyjs::disable(ex_id)
        } else {
          shinyjs::enable(txt_id)
          if (show_excl) shinyjs::enable(ex_id)
        }
      })

      # Add/remove buttons: disabled when not editing OR when processing
      if (!editing_now || isTRUE(processing_now)) {
        shinyjs::disable("addField")
        shinyjs::disable("removeField")
      } else {
        shinyjs::enable("addField")
        shinyjs::enable("removeField")
      }

      # Toggle edit button: only disabled when processing (always available to switch modes)
      if (isTRUE(processing_now)) {
        shinyjs::disable("toggleEdit")
      } else {
        shinyjs::enable("toggleEdit")
      }
    }

    observe({
      lang()
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

    # Texts that are marked exclusive (trimmed + deduplicated like texts())
    exclusive_texts <- reactive({
      raw <- txt_in_fields()[exclusive_flags()]
      trimmed <- trimws(raw)
      trimmed <- trimmed[nzchar(trimmed)]
      unique(trimmed)
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
