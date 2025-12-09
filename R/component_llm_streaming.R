# LLM Streaming Module
# This module provides a UI component and server logic for displaying real-time
# LLM streaming output in the Shiny app. It follows the same pattern as
# component_progress_bar.R, using ipc::shinyQueue() for async communication.

# 1 UI --------------------------------------------------------------------

llm_streaming_ui <- function(
  id,
  visible = FALSE
) {
  ns <- NS(id)

  display_style <- if (visible) "" else "display: none;"

  tagList(
    tags$script(HTML(sprintf(
      "
      Shiny.addCustomMessageHandler('update_stream_%s', function(message) {
        var el = document.getElementById('%s');
        if (el) {
          el.textContent = message.value;
          el.scrollTop = el.scrollHeight;
        }
      });
      Shiny.addCustomMessageHandler('append_stream_%s', function(message) {
        var el = document.getElementById('%s');
        if (el) {
          el.textContent += message.value;
          el.scrollTop = el.scrollHeight;
        }
      });
      Shiny.addCustomMessageHandler('toggle_stream_visibility_%s', function(message) {
        var container = document.getElementById('%s');
        if (container) {
          container.style.display = message.show ? '' : 'none';
        }
      });
      ",
      id,
      ns("stream_output"),
      id,
      ns("stream_output"),
      id,
      ns("wrapper")
    ))),
    div(
      id = ns("wrapper"),
      style = display_style,
      tags$pre(
        id = ns("stream_output"),
        style = "
          max-height: 200px;
          overflow-y: auto;
          background-color: #1e1e1e;
          color: #d4d4d4;
          padding: 12px;
          border-radius: 6px;
          border: 1px solid rgba(255,255,255,0.1);
          font-family: 'Consolas', 'Monaco', monospace;
          font-size: 0.85em;
          white-space: pre-wrap;
          word-wrap: break-word;
          margin: 8px 0 16px 0;
        ",
        ""
      )
    )
  )
}


# 2 Server ----------------------------------------------------------------

llm_streaming_server <- function(
  id,
  initially_hidden = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns_id <- session$ns("")

    # State ------------------------------------------------------------------

    stream_text <- reactiveVal("")
    hidden <- reactiveVal(initially_hidden)

    # Synchronous control functions ------------------------------------------

    set <- function(text) {
      if (is.character(text) && length(text) == 1) {
        stream_text(text)
      }
    }

    append_text <- function(text) {
      if (is.character(text) && length(text) == 1) {
        current <- stream_text()
        stream_text(paste0(current, text))
      }
    }

    clear <- function() {
      stream_text("")
    }

    show <- function() {
      hidden(FALSE)
    }

    hide <- function() {
      hidden(TRUE)
    }

    # Async controller -------------------------------------------------------

    queue <- ipc::shinyQueue()
    queue$consumer$start(millis = 100)

    async <- AsyncStreamController$new(queue)

    # Listen for updates -----------------------------------------------------

    observe({
      session$sendCustomMessage(
        paste0("update_stream_", id),
        list(value = stream_text())
      )
    })

    observe({
      session$sendCustomMessage(
        paste0("toggle_stream_visibility_", id),
        list(show = !isTRUE(hidden()))
      )
    })

    # Return controls --------------------------------------------------------

    reactiveValues(
      set = set,
      append = append_text,
      clear = clear,
      show = show,
      hide = hide,
      text = stream_text,
      async = async
    )
  })
}


# 3 Async Controller ------------------------------------------------------

#' AsyncStreamController
#'
#' R6 class for controlling the stream output from an async context.
#' Uses ipc::shinyQueue() to send updates to the main Shiny process.
#' Follows the same pattern as AsyncProgressBarController in component_progress_bar.R.
#'
#' @export
AsyncStreamController <- R6::R6Class(
  "AsyncStreamController",
  public = list(
    queue = NULL,

    initialize = function(queue) {
      self$queue <- queue
    },

    start = function(millis = 100) {
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

    set = function(text) {
      if (is.character(text) && length(text) == 1) {
        try(self$queue$producer$fireAssignReactive("stream_text", text))
      }
    },

    clear = function() {
      try(self$queue$producer$fireAssignReactive("stream_text", ""))
    }
  )
)


# 4 Streaming Callback Factory --------------------------------------------

#' Create a streaming callback for tidyprompt providers
#'
#' Creates a callback function compatible with tidyprompt's stream_callback
#' parameter. The callback sends streaming tokens to a Shiny reactive via
#' an ipc::shinyQueue.
#'
#' @param queue An ipc::shinyQueue object for async communication
#' @param mode One of "token" (append each token) or "partial" (replace with partial response)
#'
#' @return A function suitable for use as stream_callback in a tidyprompt provider
#' @export
create_stream_callback <- function(queue, mode = c("partial", "token")) {
  mode <- match.arg(mode)

  function(token, meta) {
    if (mode == "partial") {
      # Replace entire text with accumulated partial response
      try(queue$producer$fireAssignReactive(
        "stream_text",
        meta$partial_response %||% ""
      ))
    } else {
      # Append just the new token (less reliable due to queue timing)
      current_text <- ""
      try({
        current_text <- queue$producer$fireEval({
          stream_text()
        })
      })
      try(queue$producer$fireAssignReactive(
        "stream_text",
        paste0(current_text, token)
      ))
    }
    invisible(TRUE)
  }
}


# 5 Example/development usage ---------------------------------------------

if (FALSE) {
  library(shiny)
  library(bslib)
  library(tidyprompt)
  library(future)
  library(promises)
  library(ipc)

  future::plan(future::multisession)

  ui <- bslib::page(
    shinyjs::useShinyjs(),
    div(
      class = "container mt-4",
      style = "max-width: 800px;",
      h3("LLM Streaming Demo"),
      llm_streaming_ui("stream"),
      actionButton("start", "Start Streaming", class = "btn-primary"),
      actionButton("clear", "Clear", class = "btn-secondary"),
      actionButton(
        "toggle",
        "Toggle Visibility",
        class = "btn-outline-secondary"
      )
    )
  )

  server <- function(input, output, session) {
    stream <- llm_streaming_server("stream", initially_hidden = FALSE)

    observeEvent(input$start, {
      stream$clear()
      stream$show()

      # Simulate streaming with a simple example
      future_promise(
        {
          for (i in 1:10) {
            Sys.sleep(0.3)
            stream_async$set(paste0(
              "Processing step ",
              i,
              " of 10...\n",
              "This is simulated streaming output.\n",
              paste(rep("Token ", i * 5), collapse = "")
            ))
          }
          "Done!"
        },
        globals = list(stream_async = stream$async)
      ) %...>%
        (function(result) {
          stream$append("\n\n--- Complete ---")
        })

      NULL
    })

    observeEvent(input$clear, {
      stream$clear()
    })

    observeEvent(input$toggle, {
      if (isTRUE(stream$text() == "")) {
        stream$show()
      } else {
        stream$hide()
      }
    })
  }

  shinyApp(ui, server)
}
