# Module for splitting long texts into shorter texts via semantic chunking

# 1 UI & server ------------------------------------------------------
text_split_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

text_split_server <- function(
  id,
  raw_texts, # reactive vector with raw texts (provided by text_upload module)
  processing = reactiveVal(FALSE),
  lang = default_lang(),
  enabled = getOption("text_split__enabled", TRUE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- State ----------------------------------------------------

    # Reactive value which will return the texts; split or original
    texts <- reactive({
      # If not splitting, return raw texts
      if (!isTRUE(splitting())) {
        return(raw_texts())
      }

      # If splitting is in progress, return NULL
      if (isTRUE(split_in_progress())) {
        return(NULL)
      }

      # If split not in progress and no split texts, return raw texts
      if (is.null(split_texts())) {
        return(raw_texts())
      }

      return(split_texts())
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

    # Upon observing new raw texts, reset the split texts and message
    observeEvent(raw_texts(), {
      split_texts(NULL)
      semchunk_message("...")
    })

    # Reactive value which holds the split texts
    split_texts <- reactiveVal(NULL)

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
      split_texts = split_texts,
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
      req(raw_texts())
      req(isTRUE(splitting()))
      req(input$max_tokens)
      req(isFALSE(processing()))
      req(isTRUE(enabled))

      # Set processing state
      split_in_progress(TRUE)
      # Reset previous split texts
      split_texts(NULL)
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
          length(raw_texts())
        ),
        component = "split"
      )

      # Start queue consumer
      queue$consumer$start(millis = 50)

      # Async text splitting
      log_opts <- list(
        level = getOption("logger__level", "INFO"),
        dir = getOption("logger__dir", "logs"),
        retention = getOption("logger__retention")
      )

      promises::future_promise(
        {
          options(
            logger__level = log_opts$level,
            logger__dir = log_opts$dir,
            logger__retention = log_opts$retention
          )
          try(log_init(), silent = TRUE)

          split_texts_with_semchunk(
            texts = raw_texts,
            chunk_size = chunk_size,
            overlap = overlap,
            queue = queue
          )
        },
        globals = list(
          raw_texts = raw_texts(),
          chunk_size = max_tokens_val(),
          overlap = overlap_val(),
          queue = queue,
          split_texts_with_semchunk = split_texts_with_semchunk,
          semchunk_load_chunker = semchunk_load_chunker,
          async_message_printer = async_message_printer,
          log_opts = log_opts,
          log_init = log_init,
          get_session_id = get_session_id
        ),
        seed = NULL
      ) %...>%
        {
          result <- .
          split_in_progress(FALSE)
          split_texts(result)

          if (identical(raw_texts(), split_texts())) {
            semchunk_message(lang()$t(
              "Splitsing resulteerde niet in meer teksten"
            ))
            log_info("Text split: no change", component = "split")
          } else {
            n <- length(raw_texts())
            m <- length(split_texts())

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
          log_error(
            paste("Text split error:", error$message %||% as.character(error)),
            component = "split"
          )

          split_in_progress(FALSE)
          split_texts(NULL)
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

    # Return -------------------------------------------------------
    return(texts)
  })
}


# 2 Helpers --------------------------------------------------------
split_texts_with_semchunk <- function(
  texts,
  chunk_size = 128,
  overlap = 0,
  queue = NULL
) {
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

  result <- chunker(
    texts,
    progress = FALSE,
    offsets = FALSE,
    overlap = overlap
  ) |>
    unlist()

  return(result)
}

# 3 Example/development usage ----------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(bslib)
  library(shiny.i18n)
  library(future)
  library(promises)

  source("R/utils_semchunk.R")

  ui <- bslib::page(
    useShinyjs(),
    text_split_ui("text_split")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    # Example raw texts
    raw_texts <- reactiveVal(c(
      "Dit is een voorbeeldtekst die we gaan splitsen.",
      "Hier is nog een andere tekst die ook gesplitst moet worden."
    ))

    lang <- reactive({
      shiny.i18n::Translator$new(
        translation_json_path = "language/language.json"
      )
    })

    text_split_server("text_split", raw_texts, processing, lang)
  }

  shinyApp(ui, server)
}
