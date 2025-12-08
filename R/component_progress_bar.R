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

progress_bar_server <- function(
  id,
  initially_hidden = FALSE
) {
  moduleServer(id, function(input, output, session) {
    ns_id <- session$ns("")

    # State --------------------------------------------------------------------

    progress <- reactiveVal(0)
    text <- reactiveVal("...")
    hidden <- reactiveVal(initially_hidden)

    # Synchronous control functions --------------------------------------------

    set <- function(value, txt) {
      if (is.numeric(value) && value >= 0 && value <= 100) {
        progress(value)
      } else {
        stop("Progress value must be a number between 0 and 100.")
      }

      if (!is.null(txt) && is.character(txt) && length(txt) == 1) {
        text(txt)
      } else {
        stop("Text must be a character string.")
      }
    }

    set_with_total <- function(i, total, txt) {
      stopifnot(
        is.numeric(i) && i >= 0 && i <= total,
        is.numeric(total) && total > 0,
        is.character(txt) && length(txt) == 1
      )

      formatted_txt <- paste0(
        i,
        "/",
        total,
        "\n",
        "<br><i>",
        stringr::str_trunc(txt, 50),
        "</i>"
      )

      progress(round((i / total) * 100, 0))
      text(formatted_txt)
    }

    show <- function() {
      hidden(FALSE)
    }

    hide <- function() {
      hidden(TRUE)
    }

    # Async controller ---------------------------------------------------------

    queue <- ipc::shinyQueue()
    queue$consumer$start(millis = 250)

    async <- AsyncProgressBarController$new(queue)

    # Listen for updates -------------------------------------------------------

    observe({
      req(progress())
      session$sendCustomMessage(
        paste0("update_progress_", id),
        list(value = progress())
      )

      # if (progress() >= 100) {
      #   queue$consumer$stop()
      # } else if (queue$consumer$stopped && progress() >= 0) {
      #   queue$consumer$start(millis = 250)
      # }
    })

    observe({
      req(text())
      session$sendCustomMessage(
        paste0("update_progress_text_", id),
        list(value = text())
      )
    })

    observe({
      session$sendCustomMessage(
        paste0("toggle_visibility_", id),
        list(show = !isTRUE(hidden()))
      )
    })

    # Return controls ----------------------------------------------------------

    reactiveValues(
      set = set,
      set_with_total = set_with_total,
      show = show,
      hide = hide,
      async = async
    )
  })
}


#### 3 Helpers ####

AsyncProgressBarController <- R6::R6Class(
  "AsyncProgressBarController",
  public = list(
    queue = NULL,

    initialize = function(queue) {
      self$queue <- queue
    },

    start = function(millis = 250) {
      try(self$queue$consumer$start(millis = millis))
    },

    stop = function() {
      try(self$queue$consumer$stop())
    },

    show = function() {
      try(self$queue$producer$fireAssignReactive("hidden", FALSE))
    },

    hide = function() {
      try(self$queue$producer$fireAssignReactive("hidden", TRUE))
    },

    set = function(value, txt) {
      if (is.numeric(value) && value >= 0 && value <= 100) {
        try(self$queue$producer$fireAssignReactive("progress", value))
      } else {
        stop("Progress value must be a number between 0 and 100.")
      }

      if (!is.null(txt) && is.character(txt) && length(txt) == 1) {
        try(self$queue$producer$fireAssignReactive("text", txt))
      } else {
        stop("Text must be a character string.")
      }
    },

    set_with_total = function(i, total, txt) {
      stopifnot(
        is.numeric(i) && i >= 0 && i <= total,
        is.numeric(total) && total > 0,
        is.character(txt) && length(txt) == 1
      )

      formatted_txt <- paste0(
        i,
        "/",
        total,
        "\n",
        "<br><i>",
        stringr::str_trunc(txt, 50),
        "</i>"
      )

      try(self$queue$producer$fireAssignReactive(
        "progress",
        round((i / total) * 100, 0)
      ))
      try(self$queue$producer$fireAssignReactive(
        "text",
        formatted_txt
      ))
    }
  )
)


#### 4 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(bslib)

  ui <- fluidPage(
    progress_bar_ui("task1"),
    progress_bar_ui("task2"),
    actionButton("start", "Start"),
    actionButton("start_aysnc", "Start Async"),
    actionButton("hide_task2", "Hide Task 2"),
    actionButton("show_task2", "Show Task 2"),
    actionButton("set_with_total", "Set with Total")
  )

  server <- function(input, output, session) {
    task1 <- progress_bar_server("task1")
    task2 <- progress_bar_server("task2")

    observeEvent(input$start, {
      task1$set(50, "Taak 1 op 50%")
      task2$set(100, "Taak 2 voltooid")
    })

    observeEvent(input$start_aysnc, {
      future::future(
        {
          for (i in 1:10) {
            Sys.sleep(1) # Simulate work
            task1$set(i * 10, glue::glue("Async taak: {i * 10}% voltooid"))

            # If even, show task2, if not, hide
            if (i %% 2 == 0) {
              task2$show()
            } else {
              task2$hide()
            }
          }
        },
        seed = TRUE,
        globals = list(
          task1 = task1$async,
          task2 = task2$async
        )
      )

      NULL
    })

    observeEvent(input$hide_task2, {
      task2$hide()
    })

    observeEvent(input$show_task2, {
      task2$show()
    })

    observeEvent(input$set_with_total, {
      task1$set_with_total(5, 10, "Set met totaal")
    })
  }

  shinyApp(ui, server)
}
