# Module for splitting long texts into shorter texts via semantic chunking

# 1 UI & server ------------------------------------------------------
text_split_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

text_split_server <- function(
  id,
  document_texts, # reactive vector with current document texts
  document_rows = NULL,
  processing = reactiveVal(FALSE),
  lang = default_lang(),
  enabled = getOption("text_split__enabled", TRUE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    input_rows <- reactive({
      if (!is.null(document_rows)) {
        return(document_rows())
      }

      values <- document_texts()
      if (is.null(values)) {
        return(NULL)
      }

      data.frame(
        source_document_id = seq_along(values),
        document_id = seq_along(values),
        source_document_text = as.character(values),
        document_text = as.character(values),
        stringsAsFactors = FALSE
      )
    })

    rows <- reactive({
      # Output rows stay unchanged when splitting is off, and become chunk rows
      # when splitting is on.
      if (!isTRUE(splitting())) {
        return(input_rows())
      }

      if (isTRUE(split_in_progress())) {
        return(NULL)
      }

      if (is.null(split_rows())) {
        return(input_rows())
      }

      split_rows()
    })

    # Convenience view of the current document texts after optional splitting.
    texts <- reactive({
      current_rows <- rows()
      if (is.null(current_rows)) {
        return(NULL)
      }

      current_rows$document_text
    })

    # If text splitting is activated
    splitting <- reactive({
      if (isTRUE(input$toggle == lang()$t("Ja")) && isTRUE(enabled)) {
        TRUE
      } else {
        FALSE
      }
    })

    # If right now we are running the splitting process
    split_in_progress <- reactiveVal(FALSE)
    input_rows_version <- reactiveVal(0L)

    # Upon observing new document texts, reset the split texts and message.
    observeEvent(input_rows(), {
      input_rows_version(input_rows_version() + 1L)
      split_document_texts(NULL)
      split_rows(NULL)
      source_document_texts(NULL)
      semchunk_message("...")
    })

    # Chunked current-document texts created by this module.
    split_document_texts <- reactiveVal(NULL)
    split_rows <- reactiveVal(NULL)

    # One source-document text per chunk row, used for lineage and grouping.
    source_document_texts <- reactiveVal(NULL)

    # Reactive value which holds text message about the splitting progress
    #   (set from async process via 'ipc' package, queue object)
    semchunk_message <- reactiveVal("...")
    output$semchunk_message_ui <- renderUI({
      req(semchunk_message())
      req(isTRUE(splitting()))
      div(
        class = "text-center",
        style = "font-style: italic; color: #6c757d;", # Bootstrap muted text style
        semchunk_message()
      )
    })

    # Queue object to talk to the main process when loading model from async
    queue <- ipc::shinyQueue()

    # Reactive value to hold the maximum token size for splitting
    max_tokens_val <- reactiveVal(128)

    # Reactive value to hold the overlap value
    overlap_val <- reactiveVal(0)

    # Export test values
    shiny::exportTestValues(
      splitting = splitting,
      split_in_progress = split_in_progress,
      split_document_texts = split_document_texts,
      split_rows = split_rows,
      source_document_texts = source_document_texts,
      semchunk_message = semchunk_message,
      max_tokens_val = max_tokens_val,
      overlap_val = overlap_val
    )

    # -- UI: main card -------------------------------------------

    output$card <- renderUI({
      req(lang())
      req(isTRUE(enabled))

      tagList(
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t('Splits teksten'),
            paste0(
              lang()$t("Wil je teksten splitsen naar kortere teksten?"),
              lang()$t(
                " Als je teksten erg lang zijn (bijv., interviews) kan een taalmodel hier mogelijk minder goed mee omgaan."
              ),
              lang()$t(
                " Het kan dan nuttig zijn om je teksten op te splitsen in kortere teksten. (Let op: dit is niet nodig als je de 'markeren'-modus gebruikt.)"
              ),
              lang()$t(
                " Splitsen kan gedaan worden met behulp van semantische chunking, waarbij teksten worden gesplitst op basis van hun inhoud."
              ),
              lang()$t(
                " De teksten worden hier met Python package 'semchunk' gesplitst in stukken van een opgegeven maximale lengte (in tokens, naar OpenAI's gpt-4; een token is ongeveer 4 karakters)."
              )
            )
          ),
          card_body(
            # Toggle for text splitting
            p(
              lang()$t("Teksten splitsen naar kortere teksten?"),
              class = "text-center"
            ),
            div(
              class = "d-flex justify-content-center",
              shinyWidgets::radioGroupButtons(
                ns("toggle"),
                NULL,
                choices = c(
                  lang()$t("Nee"),
                  lang()$t("Ja")
                ),
                selected = lang()$t("Nee"),
                size = "sm"
              )
            ),
            uiOutput(ns("split_section"))
          )
        )
      )
    })

    # -- UI: splitting UI

    output$split_section <- renderUI({
      if (!isTRUE(splitting())) {
        return(div(style = "display: none;"))
      }

      tagList(
        div(
          class = "d-flex flex-column align-items-center",
          uiOutput(ns("split_ui"))
        )
      )
    })

    output$split_ui <- renderUI({
      req(splitting())

      div(
        # Center the content
        class = "d-flex flex-column align-items-center",
        # Max token size input
        numericInput(
          ns("max_tokens"),
          label = lang()$t("Maximale lengte per tekst (tokens)"),
          value = isolate(max_tokens_val()),
          min = 1,
          step = 1
        ),
        # Overlap
        # `overlap` argument to overlap chunks by a ratio (if < 1) or
        #   an absolute number of tokens (if >= 1)'
        numericInput(
          ns("overlap"),
          label = span(
            lang()$t("Overlap tussen teksten (tokens)"),
            tooltip(
              bsicons::bs_icon("info-circle"),
              paste0(
                lang()$t(
                  "Waarde die de toegestane overlap tussen de teksten bepaalt."
                ),
                lang()$t(
                  " Een waarde tussen 0 en 1 wordt geïnterpreteerd als een ratio van de tekstlengte; een waarde groter dan 1 wordt geïnterpreteerd als een absoluut aantal tokens."
                )
              ),
              placement = "bottom"
            )
          ),
          value = 0,
          min = 0,
          step = 1
        ),
        # Button to split texts
        div(
          class = "d-flex flex-column align-items-center",
          div(
            class = "text-center mb-3",
            actionButton(
              ns("split_texts"),
              icon = shiny::icon("scissors"),
              label = lang()$t("Splits teksten"),
              class = "btn btn-primary"
            )
          ),
          uiOutput(ns("semchunk_message_ui"))
        )
      )
    })

    # Listen for user inputs ---------------------------------------

    observeEvent(input$split_texts, {
      req(input_rows())
      req(isTRUE(splitting()))
      req(input$max_tokens)
      req(isFALSE(processing()))
      req(isTRUE(enabled))

      # Set processing state
      split_in_progress(TRUE)
      request_input_rows_version <- isolate(input_rows_version())
      # Reset previous split texts
      split_document_texts(NULL)
      # Disable the button while splitting
      shinyjs::disable("split_texts")
      # Set message
      semchunk_message(lang()$t("..."))

      # Log split action start
      log_info(
        sprintf(
          "Text split started: max_tokens=%d, overlap=%d, n_texts=%d",
          input$max_tokens,
          input$overlap %||% 0,
          nrow(input_rows())
        ),
        component = "split"
      )

      # Start queue consumer
      queue$consumer$start(millis = 50)

      # Async text splitting
      log_ctx <- log_context_capture(is_async = TRUE)

      mirai::mirai(
        {
          log_context_apply(log_ctx)

          split_texts_with_semchunk(
            texts = input_rows$document_text,
            source_document_ids = input_rows$source_document_id,
            source_document_texts = input_rows$source_document_text,
            chunk_size = chunk_size,
            overlap = overlap,
            queue = queue
          )
        },
        .args = c(
          log_async_globals(log_ctx),
          list(
            input_rows = input_rows(),
            chunk_size = max_tokens_val(),
            overlap = overlap_val(),
            queue = queue,
            split_texts_with_semchunk = split_texts_with_semchunk,
            semchunk_load_chunker = semchunk_load_chunker,
            async_message_printer = async_message_printer
          )
        )
      ) %...>%
        {
          result <- .

          if (
            !identical(
              request_input_rows_version,
              isolate(input_rows_version())
            )
          ) {
            log_info(
              "Ignoring stale split result after source texts changed",
              component = "split"
            )
            split_in_progress(FALSE)
            shinyjs::enable("split_texts")
            queue$consumer$stop()
            return(NULL)
          }

          split_in_progress(FALSE)
          split_rows(result$rows)
          split_document_texts(result$rows$document_text)
          source_document_texts(result$rows$source_document_text)

          if (identical(input_rows()$document_text, split_document_texts())) {
            semchunk_message(lang()$t(
              "Splitsing resulteerde niet in meer teksten"
            ))
            log_info("Text split: no change", component = "split")
          } else {
            n <- nrow(input_rows())
            m <- length(split_document_texts())

            semchunk_message(paste0(
              lang()$t("Originele "),
              n,
              lang()$t(" teksten zijn gesplitst naar "),
              m,
              lang()$t(" teksten")
            ))
            log_info(
              sprintf("Text split complete: %d -> %d texts", n, m),
              component = "split"
            )
          }

          shinyjs::enable("split_texts")
          queue$consumer$stop()
        } %...!%
        {
          error <- .

          if (
            !identical(
              request_input_rows_version,
              isolate(input_rows_version())
            )
          ) {
            log_info(
              "Ignoring stale split error after source texts changed",
              component = "split"
            )
            split_in_progress(FALSE)
            shinyjs::enable("split_texts")
            queue$consumer$stop()
            return(NULL)
          }

          log_error(
            paste("Text split error:", error$message %||% as.character(error)),
            component = "split"
          )

          split_in_progress(FALSE)
          split_document_texts(NULL)
          split_rows(NULL)
          semchunk_message("...")

          # Handle errors
          showNotification(
            paste0(
              lang()$t(
                "Er is een fout opgetreden bij het splitsen van de teksten:"
              ),
              " ",
              error$message
            ),
            type = "error",
            duration = 5
          )

          shinyjs::enable("split_texts")
          queue$consumer$stop()
        }

      NULL # Return NULL to avoid returning the promise object
    })

    # Ensure max_tokens value stays valid
    observeEvent(
      input$max_tokens,
      {
        req(input$max_tokens)
        req(isTRUE(enabled))

        new_val <- max(1, input$max_tokens)
        max_tokens_val(new_val)

        if (input$max_tokens != new_val) {
          updateNumericInput(session, "max_tokens", value = new_val)
        }

        log_action(
          "max_tokens_changed",
          details = sprintf(
            "splitting=%s value=%d",
            isTRUE(splitting()),
            new_val
          )
        )
      },
      ignoreInit = TRUE
    )

    # Ensure overlap value stays valid
    observeEvent(
      input$overlap,
      {
        req(input$overlap)
        req(isTRUE(enabled))

        new_val <- max(0, input$overlap)
        overlap_val(new_val)

        if (input$overlap != new_val) {
          updateNumericInput(session, "overlap", value = new_val)
        }

        log_action(
          "overlap_changed",
          details = sprintf(
            "splitting=%s value=%d",
            isTRUE(splitting()),
            new_val
          )
        )
      },
      ignoreInit = TRUE
    )

    # Disable inputs when processing -------------------------------

    disable_when_processing(
      processing,
      c("toggle", "max_tokens", "overlap", "split_texts")
    )

    split_settings <- reactive({
      list(
        enabled = isTRUE(splitting()),
        chunk_size = max_tokens_val(),
        overlap = overlap_val()
      )
    })

    # Return -------------------------------------------------------
    return(list(
      texts = texts,
      rows = rows,
      source_document_texts = source_document_texts,
      split_in_progress = split_in_progress,
      split_settings = split_settings
    ))
  })
}


# 2 Helpers --------------------------------------------------------
split_texts_with_semchunk <- function(
  texts,
  source_document_ids = NULL,
  source_document_texts = NULL,
  chunk_size = 128,
  overlap = 0,
  queue = NULL
) {
  if (is.null(source_document_ids)) {
    source_document_ids <- seq_along(texts)
  }
  if (is.null(source_document_texts)) {
    source_document_texts <- texts
  }

  chunker <- semchunk_load_chunker(
    chunk_size = chunk_size,
    queue = queue
  )

  if (!is.null(queue)) {
    try(
      queue$producer$fireAssignReactive(
        "semchunk_message",
        "Splitting texts..."
      ),
      silent = TRUE
    )
  }

  chunks_list <- chunker(
    texts,
    progress = FALSE,
    offsets = FALSE,
    overlap = overlap
  )

  # source_document_* still points to the uploaded row.
  # document_* identifies each chunk row created by the split.
  source_document_text <- rep(
    source_document_texts,
    times = lengths(chunks_list)
  )
  source_document_id <- rep(source_document_ids, times = lengths(chunks_list))
  chunk_texts <- as.character(unlist(chunks_list))

  rows <- data.frame(
    source_document_id = as.integer(source_document_id),
    document_id = seq_along(chunk_texts),
    source_document_text = as.character(source_document_text),
    document_text = chunk_texts,
    stringsAsFactors = FALSE
  )

  list(
    texts = chunk_texts,
    source_document_text = source_document_text,
    rows = rows
  )
}

# 3 Example/development usage ----------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(shiny.i18n)
  library(mirai)
  library(promises)

  source("R/utils_semchunk.R")

  ui <- bslib::page(
    useShinyjs(),
    text_split_ui("text_split")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    # Example current document texts
    document_texts <- reactiveVal(c(
      "Dit is een voorbeeldtekst die we gaan splitsen.",
      "Hier is nog een andere tekst die ook gesplitst moet worden."
    ))

    lang <- reactive({
      shiny.i18n::Translator$new(
        translation_json_path = "language/language.json"
      )
    })

    text_split_server(
      "text_split",
      document_texts = document_texts,
      processing = processing,
      lang = lang
    )
  }

  shinyApp(ui, server)
}
