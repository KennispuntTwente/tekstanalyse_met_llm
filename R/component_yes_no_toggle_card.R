# Reusable Yes/No toggle card component
# This component provides a standardized card with a Yes/No toggle button
# Used by: human_in_the_loop, interrater_reliability, assign_multiple_categories,
#          write_paragraphs, etc.

# 1 UI --------------------------------------------------------------------

yes_no_toggle_card_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_toggle"))
}


# 2 Server ----------------------------------------------------------------

#' Yes/No Toggle Card Server
#'
#' @param id Module ID
#' @param title Card title (pre-translated, or raw if translate_texts = TRUE)
#' @param tooltip_text Tooltip text (pre-translated, or raw if translate_texts = TRUE)
#' @param question_text Question shown above buttons (pre-translated, or raw if translate_texts = TRUE)
#' @param default_value Default toggle value: TRUE = "Ja", FALSE = "Nee"
#' @param show_when Reactive condition for when to show the card
#' @param header_extra Optional UI to add in the card header (right side)
#' @param modal_config Optional list for a modal button in header:
#'   - icon: FontAwesome icon name (e.g., "palette")
#'   - tooltip: Tooltip text for the button
#'   - title: Modal title
#'   - body_text: Text for modal body (will be wrapped in p() and translated)
#'   - input_label: Label for the textarea
#'   - input_placeholder: Placeholder for the textarea
#' @param translate_texts If TRUE, translate title/tooltip_text/question_text/modal_config texts
#' @param extra_disable_ids Additional input IDs to disable when processing
#' @param processing Reactive value for processing state
#' @param lang Language translator reactive
#'
#' @return Reactive value containing the toggle state (TRUE/FALSE),
#'         or a list with toggle and modal_value if modal_config is provided
yes_no_toggle_card_server <- function(
  id,
  title,
  tooltip_text,
  question_text,
  default_value = FALSE,
  show_when = reactive(TRUE),
  header_extra = NULL,
  modal_config = NULL,
  translate_texts = FALSE,
  extra_disable_ids = character(0),
  processing = reactiveVal(FALSE),
  lang = default_lang()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    toggle <- reactiveVal(default_value)
    modal_value <- reactiveVal("")

    # Helper to translate text if translate_texts is TRUE
    t <- function(text) {
      if (translate_texts) lang()$t(text) else text
    }

    # Render modal button if modal_config is provided
    output$modal_button <- renderUI({
      req(modal_config)
      modal_trigger_icon(
        ns = ns,
        input_id = "show_modal",
        icon_name = modal_config$icon,
        tooltip_text = t(modal_config$tooltip),
        is_active = nzchar(modal_value()),
        font_size = "1rem"
      )
    })

    # Show modal when button is clicked
    observeEvent(input$show_modal, {
      req(modal_config)
      showModal(
        modalDialog(
          title = tagList(icon(modal_config$icon), " ", t(modal_config$title)),
          tags$div(
            style = "display:none;",
            `data-kwallm-modal-id` = paste0(id, "_modal")
          ),
          div(
            if (
              !is.null(modal_config$body_text1) ||
                !is.null(modal_config$body_text2)
            ) {
              p(paste0(
                if (!is.null(modal_config$body_text1)) {
                  t(modal_config$body_text1)
                } else {
                  ""
                },
                if (!is.null(modal_config$body_text2)) {
                  t(modal_config$body_text2)
                } else {
                  ""
                }
              ))
            },
            if (!is.null(modal_config$body)) modal_config$body,
            textAreaInput(
              ns("modal_input"),
              t(modal_config$input_label),
              value = modal_value(),
              rows = 4,
              width = "100%",
              placeholder = t(modal_config$input_placeholder)
            )
          ),
          footer = tagList(
            tags$div(
              style = "display:flex; width:100%; align-items:center;",
              tags$div(
                style = "flex:1; text-align:left;",
                actionButton(
                  ns("modal_close"),
                  lang()$t("Sluiten"),
                  class = "btn-secondary"
                )
              ),
              tags$div(
                style = "flex:1; text-align:center;",
                actionButton(
                  ns("modal_reset"),
                  lang()$t("Reset"),
                  class = "btn-danger"
                )
              ),
              tags$div(
                style = "flex:1; text-align:right;",
                actionButton(
                  ns("modal_save"),
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

      if (isTRUE(processing())) {
        shinyjs::disable("modal_input")
        shinyjs::disable("modal_save")
        shinyjs::disable("modal_reset")
      }
    })

    # Modal save/reset
    observeEvent(input$modal_save, {
      modal_value(input$modal_input)
      log_action("modal_saved", details = sprintf("id=%s", id))
      removeModal()
    })

    observeEvent(input$modal_reset, {
      modal_value("")
      updateTextAreaInput(session, "modal_input", value = "")
      log_action("modal_reset", details = sprintf("id=%s", id))
      removeModal()
    })

    observeEvent(input$modal_close, {
      removeModal()
    })

    # Render main UI
    output$ui_toggle <- renderUI({
      req(show_when())

      # Build header extra: use provided header_extra OR modal button if modal_config
      final_header_extra <- if (!is.null(modal_config)) {
        uiOutput(ns("modal_button"))
      } else {
        header_extra
      }

      tagList(
        shinyjs::useShinyjs(),
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            t(title),
            t(tooltip_text),
            extra = final_header_extra
          ),
          card_body(
            p(t(question_text), class = "mb-2 text-center"),
            div(
              class = "d-flex justify-content-center",
              shinyWidgets::radioGroupButtons(
                ns("toggle"),
                NULL,
                choices = c(lang()$t("Nee"), lang()$t("Ja")),
                selected = if (default_value) {
                  lang()$t("Ja")
                } else {
                  lang()$t("Nee")
                },
                size = "sm"
              )
            )
          )
        )
      )
    })

    # Observe toggle
    observeEvent(input$toggle, {
      new_value <- input$toggle == lang()$t("Ja")
      toggle(new_value)
      log_action(
        "toggle_changed",
        details = sprintf("id=%s, value=%s", id, new_value)
      )
    })

    # Disable when processing
    observeEvent(
      processing(),
      {
        shinyjs::toggleState("toggle", condition = !processing())
        if (!is.null(modal_config)) {
          shinyjs::toggleState("show_modal", condition = !processing())
        }
        for (id in extra_disable_ids) {
          shinyjs::toggleState(id, condition = !processing())
        }
      },
      ignoreInit = TRUE
    )

    # Return
    if (!is.null(modal_config)) {
      return(list(toggle = toggle, modal_value = modal_value))
    } else {
      return(toggle)
    }
  })
}


# 3 Example/development usage ---------------------------------------------

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(bsicons)

  ui <- bslib::page(
    useShinyjs(),
    yes_no_toggle_card_ui("test_toggle")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    result <- yes_no_toggle_card_server(
      id = "test_toggle",
      title = "Test Toggle",
      tooltip_text = "This is a tooltip.",
      question_text = "Enable this feature?",
      default_value = FALSE,
      show_when = reactive(TRUE),
      processing = processing
    )

    observe({
      print(paste("Toggle:", result()))
    })
  }

  shinyApp(ui, server)
}
