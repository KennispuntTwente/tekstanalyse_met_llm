# Progress bar module; this module provides a progress bar UI and server logic
# Progress bar can be updated with a value and text, and can be shown or hidden

#### 1 UI ####

progress_bar_ui <- function(
  id,
  default_value = 0,
  default_text = "...",
  visible = TRUE
) {
  ns <- NS(id)

  display_style <- if (visible) "" else "display: none;"

  tagList(
    tags$script(HTML(sprintf(
      "
      Shiny.addCustomMessageHandler('update_progress_%s', function(message) {
        var bar = document.getElementById('%s');
        if (bar) { bar.style.width = message.value + '%%'; }
      });
      Shiny.addCustomMessageHandler('update_progress_text_%s', function(message) {
        var text = document.getElementById('%s');
        if (text) { text.innerHTML = message.value; }
      });
      Shiny.addCustomMessageHandler('toggle_visibility_%s', function(message) {
        var container = document.getElementById('%s');
        if (container) {
          container.style.display = message.show ? '' : 'none';
        }
      });
      ",
      id,
      ns("bar"),
      id,
      ns("text"),
      id,
      ns("wrapper")
    ))),
    div(
      class = "card-container",
      div(
        id = ns("wrapper"),
        style = display_style,
        div(
          class = "progress mb-2",
          div(
            id = ns("bar"),
            class = "progress-bar",
            role = "progressbar",
            style = sprintf("width: %s%%;", default_value),
            `aria-valuenow` = default_value,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100"
          )
        ),
        div(
          id = ns("text"),
          default_text,
          class = "text-center mb-3"
        )
      )
    )
  )
}


#### 2 Server ####

progress_bar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns_id <- session$ns("")

    set_progress <- function(value) {
      session$sendCustomMessage(
        paste0("update_progress_", id),
        list(value = value)
      )
    }

    set_text <- function(text) {
      session$sendCustomMessage(
        paste0("update_progress_text_", id),
        list(value = text)
      )
    }

    show <- function() {
      session$sendCustomMessage(
        paste0("toggle_visibility_", id),
        list(show = TRUE)
      )
    }

    hide <- function() {
      session$sendCustomMessage(
        paste0("toggle_visibility_", id),
        list(show = FALSE)
      )
    }

    reactiveValues(
      set_progress = set_progress,
      set_text = set_text,
      show = show,
      hide = hide
    )
  })
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(bslib)

  ui <- fluidPage(
    progress_bar_ui("task1"),
    progress_bar_ui("task2"),
    actionButton("start", "Start"),
    actionButton("hide_task2", "Hide Task 2"),
    actionButton("show_task2", "Show Task 2")
  )

  server <- function(input, output, session) {
    task1 <- progress_bar_server("task1")
    task2 <- progress_bar_server("task2")

    observeEvent(input$start, {
      task1$set_progress(50)
      task1$set_text("Taak 1 op 50%")

      task2$set_progress(100)
      task2$set_text("Taak 2 voltooid")
    })

    observeEvent(input$hide_task2, {
      task2$hide()
    })

    observeEvent(input$show_task2, {
      task2$show()
    })
  }

  shinyApp(ui, server)
}
