# Manual browser demo for paragraph batch/reduction streaming.
#
# Run from the repository root with:
#   Rscript tests/manual/paragraph_batch_streaming_app.R
#
# This uses the production streaming UI, but deterministic simulated LLM text.
# It requires no API key and makes each clear/restart boundary easy to inspect.

paragraph_batch_streaming_demo_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to run this demo.")
  }
  if (!requireNamespace("later", quietly = TRUE)) {
    stop("The later package is required to run this demo.")
  }

  component_candidates <- c(file.path("R", "component_llm_streaming.R"))
  if (requireNamespace("here", quietly = TRUE)) {
    component_candidates <- c(
      here::here("R", "component_llm_streaming.R"),
      component_candidates
    )
  }
  component_path <- component_candidates[file.exists(component_candidates)][1]
  if (is.na(component_path)) {
    stop("Run this demo from the repository root.")
  }

  demo_env <- new.env(parent = globalenv())
  demo_env$NS <- shiny::NS
  demo_env$tagList <- shiny::tagList
  demo_env$tags <- shiny::tags
  demo_env$HTML <- shiny::HTML
  demo_env$div <- shiny::div
  sys.source(component_path, envir = demo_env)

  events <- list(
    list(
      label = "Source batch 1 of 3",
      text = paste(
        "Respondents appreciate the quick and personal support.",
        "Several describe the contact as clear, friendly, and practical."
      )
    ),
    list(
      label = "Source batch 2 of 3",
      text = paste(
        "Other respondents report long waiting times and inconsistent answers.",
        "They especially want clearer communication about expected delays."
      )
    ),
    list(
      label = "Source batch 3 of 3",
      text = paste(
        "A third perspective emphasizes that the outcome was usually helpful,",
        "even when reaching the correct employee required multiple attempts."
      )
    ),
    list(
      label = "Reduction round 1 — combining partial summaries",
      text = paste(
        "The partial summaries show a contrast between helpful, personal",
        "support and frustration about delays, hand-offs, and unclear timing."
      )
    ),
    list(
      label = "Final synthesis",
      text = paste(
        "Experiences with support are mixed. Respondents value friendly,",
        "practical assistance and generally helpful outcomes, while waiting",
        "times, repeated hand-offs, and unclear delay communication remain",
        "the main sources of dissatisfaction."
      )
    )
  )

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("\
        body { max-width: 900px; margin: 0 auto; padding: 28px; }\
        .demo-status { font-weight: 600; margin: 16px 0 6px; }\
        .demo-note { color: #555; margin-bottom: 18px; }\
      "))
    ),
    shiny::h2("Batched paragraph streaming demo"),
    shiny::p(
      class = "demo-note",
      paste(
        "This simulates three source batches, a reduction summary, and the",
        "final synthesis. Each completed summary remains visible until the",
        "next call starts; the stream then clears and restarts."
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          "start",
          "Run streaming demo",
          class = "btn-primary"
        )
      ),
      shiny::column(
        4,
        shiny::selectInput(
          "speed",
          "Streaming speed",
          choices = c("Slow" = 0.09, "Normal" = 0.05, "Fast" = 0.02),
          selected = 0.05
        )
      )
    ),
    shiny::div(class = "demo-status", shiny::textOutput("status", inline = TRUE)),
    demo_env$llm_streaming_ui("demo_stream", visible = TRUE)
  )

  server <- function(input, output, session) {
    status <- shiny::reactiveVal("Ready")
    generation <- shiny::reactiveVal(0L)
    output$status <- shiny::renderText(status())

    update_stream <- function(value) {
      session$sendCustomMessage(
        "update_stream_demo_stream",
        list(value = value)
      )
    }

    run_event <- function(event_index, run_id) {
      if (!identical(shiny::isolate(generation()), run_id)) {
        return(invisible(NULL))
      }
      if (event_index > length(events)) {
        status("Complete — the final synthesis remains visible")
        return(invisible(NULL))
      }

      event <- events[[event_index]]
      status(event$label)
      update_stream("")

      ends <- seq.int(5L, nchar(event$text), by = 5L)
      if (!length(ends) || tail(ends, 1L) != nchar(event$text)) {
        ends <- c(ends, nchar(event$text))
      }

      emit <- function(chunk_index) {
        if (!identical(shiny::isolate(generation()), run_id)) {
          return(invisible(NULL))
        }
        update_stream(substr(event$text, 1L, ends[[chunk_index]]))
        if (chunk_index < length(ends)) {
          later::later(
            function() emit(chunk_index + 1L),
            delay = as.numeric(shiny::isolate(input$speed))
          )
        } else {
          later::later(
            function() run_event(event_index + 1L, run_id),
            delay = 0.9
          )
        }
        invisible(NULL)
      }

      later::later(function() emit(1L), delay = 0.15)
      invisible(NULL)
    }

    shiny::observeEvent(input$start, {
      run_id <- shiny::isolate(generation()) + 1L
      generation(run_id)
      run_event(1L, run_id)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

if (sys.nframe() == 0L) {
  shiny::runApp(
    paragraph_batch_streaming_demo_app(),
    host = "127.0.0.1",
    launch.browser = TRUE
  )
}
