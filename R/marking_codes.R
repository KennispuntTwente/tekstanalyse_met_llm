#### 1 UI & server ####

marking_codes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("codes"))
  )
}

marking_codes_server <- function(
  id,
  mode,
  processing,
  texts,
  research_background,
  context_window,
  llm_provider_rv,
  models,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Use the reusable editable field list module
    fields <- editable_field_list_server(
      id = "fields",
      field_label = "Code",
      initial_count = 1,
      show_exclusive = reactiveVal(FALSE),
      processing = processing,
      lang = lang
    )

    # Re-export test values
    shiny::exportTestValues(
      n_fields = fields$unique_non_empty_count(),
      txt_in_fields = fields$texts(),
      isEditing = fields$editing(),
      generated_codes = generated_codes()
    )

    ## UI: Card wrapper with code generation ####
    output$codes <- renderUI({
      if (mode() == "Markeren") {
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Codes"),
            paste0(
              lang()$t("Bewerk hier de codes waarnaar het taalmodel relevante delen van de teksten zal markeren."),
              lang()$t(" Gebruik de '+'- en '-'-knoppen om codes toe te voegen of te verwijderen."),
              lang()$t(" Gebruik tenslotte de save/edit-knop om de codes op te slaan (of weer te kunnen bewerken).")
            )
          ),
          card_body(
            p(
              paste0(
                lang()$t(
                  "Voer de codes in waarvoor het taalmodel relevante delen van de teksten zal markeren."
                ),
                lang()$t(
                  " Geef beknopte, duidelijke omschrijvingen (met optioneel voorbeelden van teksten die bij de code zouden moeten horen)."
                )
              )
            ),
            editable_field_list_ui(ns("fields")),
            # Centered button for generating codes
            div(
              class = "text-center mt-3",
              actionButton(
                ns("generateCodes"),
                label = lang()$t("Genereer codes") |>
                  bslib::tooltip(
                    paste0(
                      lang()$t(
                        "Genereer mogelijke codes door het taalmodel alle teksten te laten lezen."
                      ),
                      lang()$t(
                        " (Let op, dit kan even duren, afhankelijk van het aantal teksten en de snelheid van het taalmodel dat je gebruikt.)"
                      )
                    )
                  ),
                class = "btn btn-primary",
                style = "min-width: 250px;"
              )
            ),
            # Message for code generation progress
            uiOutput(ns("generate_codes_message_ui"))
          )
        )
      }
    })

    ## Disable generateCodes when processing ####
    disable_when_processing(processing, "generateCodes")

    ## Auto-generate codes by reading texts ---------------------------------

    # Interrupter can stop async processing if user quits
    interrupter <- ipc::AsyncInterruptor$new()
    # Queue to communicate between async/main process
    queue <- ipc::shinyQueue()

    # Helper to check if number of texts is under maximum
    number_of_texts_under_maximum <- function(
      maximum = getOption("processing__max_texts", 3000)
    ) {
      if (length(texts$preprocessed) > maximum) {
        shiny::showNotification(
          paste0(
            lang()$t("Je mag maximaal "),
            maximum,
            lang()$t(" teksten analyseren.")
          ),
          type = "error"
        )
        return(FALSE)
      }
      return(TRUE)
    }

    # Reactive value to store generated codes
    generated_codes <- reactiveVal(NULL)

    # Reactive value to store if generation is in progress
    code_generation_in_progress <- reactiveVal(FALSE)

    # Reactive value to store progress about code generation
    generate_codes_message <- reactiveVal("...")
    output$generate_codes_message_ui <- renderUI({
      req(generate_codes_message())
      div(
        class = "text-center",
        style = "font-style: italic; color: #6c757d;", # Bootstrap muted text style
        generate_codes_message()
      )
    })

    # Listen for button click & start generating codes
    observeEvent(
      input$generateCodes,
      {
        if (!isFALSE(processing())) {
          return()
        }
        if (!isTRUE(mode() %in% c("Markeren"))) {
          return()
        }
        if (!isTRUE(number_of_texts_under_maximum())) {
          return()
        }
        req(isFALSE(context_window$any_fit_problem))

        # Verify we have texts available to read
        if (length(texts$preprocessed) == 0) {
          shiny::showNotification(
            lang()$t("Geen teksten beschikbaar om codes voor te genereren"),
            type = "error"
          )
          return()
        }

        # Verify a LLM provider is set
        if (is.null(models$main)) {
          shiny::showNotification(
            lang()$t("Er is nog geen LLM provider ingesteld"),
            type = "error"
          )
          return()
        }

        # Start code generation
        code_generation_in_progress(TRUE)

        # Set generation message
        generate_codes_message(lang()$t("Codes genereren..."))
        shiny::showNotification(lang()$t("Codes genereren..."))

        # Empty all previously generated codes
        generated_codes(NULL)
        # Reset fields to empty (will be populated after generation)
        fields$set_values(c(""))

        # Disable generateCodes button during generation
        shinyjs::disable("generateCodes")

        # Set model
        llm_provider <- models$main

        # Async generate codes
        queue$consumer$start()
        future_promise(
          {
            generate_codes_by_reading_texts(
              texts = texts,
              research_background = research_background,
              llm_provider = llm_provider,
              queue = queue,
              interrupter = interrupter,
              language = language
            )
          },
          globals = list(
            texts = texts$preprocessed,
            research_background = research_background(),
            llm_provider = llm_provider,
            queue = queue,
            interrupter = interrupter,
            language = lang()$get_translation_language(),
            generate_codes_by_reading_texts = generate_codes_by_reading_texts,
            send_prompt_with_retries = send_prompt_with_retries,
            get_context_window_size_in_tokens = get_context_window_size_in_tokens,
            create_text_chunks = create_text_chunks,
            create_candidate_topics = create_candidate_topics,
            prompt_candidate_topics = prompt_candidate_topics,
            reduce_topics = reduce_topics,
            semchunk_load_chunker = semchunk_load_chunker,
            tiktoken_load_tokenizer = tiktoken_load_tokenizer,
            count_tokens = count_tokens,
            async_message_printer = async_message_printer
          ),
          packages = c(
            "tidyprompt",
            "purrr",
            "dplyr",
            "stringr"
          ),
          seed = NULL
        ) %...>%
          {
            generated_codes(.)
            code_generation_in_progress(FALSE)
            shinyjs::delay(500, queue$consumer$stop())
          } %...!%
          {
            code_generation_in_progress(FALSE)
            shinyjs::delay(500, queue$consumer$stop())
            app_error(
              .,
              when = "generating codes by reading texts (marking)",
              fatal = FALSE,
              lang = lang()
            )
          }

        NULL # Avoid blocking the main thread
      }
    )

    # Observe generated codes and update fields using set_values
    observe({
      req(generated_codes())
      codes <- generated_codes()
      if (is.null(codes) || length(codes) == 0) {
        shiny::showNotification(
          lang()$t("Geen codes gegenereerd"),
          type = "warning"
        )
        return()
      }

      # Update fields with generated codes using module method
      fields$set_values(codes)
    })

    # Disable/enable generateCodes upon generation in progress
    observe({
      if (isTRUE(code_generation_in_progress())) {
        shinyjs::disable("generateCodes")
      } else if (isTRUE(fields$editing()) && !isTRUE(processing())) {
        shinyjs::enable("generateCodes")
      }
    })

    # Disable generation button when length of texts is 0
    observe({
      req(isTRUE(mode() == "Markeren"))
      if (length(texts$preprocessed) == 0) {
        shinyjs::delay(
          250,
          shinyjs::disable("generateCodes")
        )
      } else {
        shinyjs::delay(
          250,
          shinyjs::enable("generateCodes")
        )
      }
    })

    # Upon mode change, fire interrupter
    observeEvent(mode(), {
      if (
        isTRUE(code_generation_in_progress()) && isTRUE(mode() != "Markeren")
      ) {
        try(interrupter$interrupt("Mode changed during code generation"))
      }
    })

    # On exit, fire interrupter
    shiny::onStop(function() {
      try(
        {
          interrupter$interrupt(
            "Shiny session was stopped (`shiny::onStop()`)"
          )
          interrupter$destroy()
        },
        silent = TRUE
      )
    })

    ## Return -----------------------------------------------------------------

    return(list(
      texts = fields$texts,
      editing = fields$editing,
      unique_non_empty_count = fields$unique_non_empty_count
    ))
  })
}

#### 2 Helpers ####

# Function to generate potential codes by having the LLM read all texts
generate_codes_by_reading_texts <- function(
  texts = c("hi", "hello world", "i like this product"),
  text_size_tokens = 256,
  overlap_size_tokens = 64,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(),
  queue = NULL,
  interrupter = NULL,
  language = c("nl", "en")
) {
  # Validate inputs
  language <- match.arg(language)
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    all(nzchar(texts)),
    is.numeric(text_size_tokens),
    text_size_tokens > 0,
    is.numeric(overlap_size_tokens),
    overlap_size_tokens >= 0
  )

  # Helper to print messages to the console + queue if needed
  print_message <- function(
    message,
    type = c("info", "success")
  ) {
    type <- match.arg(type)
    if (type == "success") {
      cli::cli_alert_success(message)
      message <- paste0(
        cli::col_green("✔"),
        " ",
        message
      )
    } else {
      message <- paste0(
        cli::col_blue("ℹ"),
        " ",
        message
      )
      cli::cli_alert_info(message)
    }

    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive(
        "generate_codes_message",
        message
      ))
    }
  }

  # Load chunker
  print_message("Loading semantic chunker...")
  chunker_name <- paste0("semchunker_", text_size_tokens)
  if (
    !exists(
      chunker_name
    )
  ) {
    semchunker <- semchunk_load_chunker(chunk_size = text_size_tokens)
    assign(chunker_name, semchunker, envir = .GlobalEnv)
  } else {
    semchunker <- get(chunker_name, envir = .GlobalEnv)
  }

  if (!is.null(interrupter)) {
    interrupter$execInterrupts()
  }

  # Split texts
  split_texts <- semchunker(
    texts,
    overlap = overlap_size_tokens
  ) |>
    unlist()
  if (length(split_texts) == length(texts)) {
    print_message("No splitting needed, using original texts...")
  } else {
    print_message(paste0(
      "Split ",
      length(texts),
      " texts into ",
      length(split_texts),
      " smaller texts..."
    ))
  }

  if (!is.null(interrupter)) {
    interrupter$execInterrupts()
  }

  # Determine context window size
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) {
    n_tokens_context_window <- 2048
  }

  # Create text chunks
  chunks <- create_text_chunks(
    split_texts,
    chunk_size = 50,
    draws = 1,
    n_tokens_context_window = n_tokens_context_window
  )
  print_message(paste0(
    "Created ",
    length(chunks),
    " text chunk(s) from the texts..."
  ))

  # Create candidate topics
  candidate_topics <- c()
  for (i in seq_along(chunks)) {
    if (!is.null(interrupter)) {
      interrupter$execInterrupts()
    }

    chunk <- chunks[[i]]
    print_message(paste0(
      "Reading chunk ",
      i,
      " of ",
      length(chunks),
      "..."
    ))

    candidate_topics_x <- create_candidate_topics(
      list(chunk),
      research_background = research_background,
      llm_provider = llm_provider,
      language = language
    )

    candidate_topics <- c(
      candidate_topics,
      candidate_topics_x
    )
  }
  # Make unique
  candidate_topics <- unique(candidate_topics)
  # Print progress
  print_message(paste0(
    "Created ",
    length(candidate_topics),
    " candidate codes, reducing to final list..."
  ))

  # Reduce topics
  final_topics <- reduce_topics(
    candidate_topics = candidate_topics,
    research_background = research_background,
    llm_provider = llm_provider,
    always_add_not_applicable = FALSE,
    interrupter = interrupter,
    language = language
  )

  print_message(paste0(
    "Generated ",
    length(final_topics),
    " codes"
  ))

  return(final_topics)
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    marking_codes_ui("marking_codes")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)
    mode <- reactiveVal("Markeren")

    codes <- marking_codes_server("marking_codes", mode, processing)

    observe({
      req(codes$texts())
      print(codes$texts())
    })
  }

  shinyApp(ui, server)
}
