# 1 UI & server --------------------------------------------------------
marking_codes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("codes"))
  )
}

.kwallm_marking_code_generation_chunk_settings <- function(context_window) {
  list(
    text_size_tokens = as.numeric(context_window$max_tokens %||% 256),
    overlap_size_tokens = as.numeric(context_window$overlap %||% 0)
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
  lang = default_lang()
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

    # Test exports are registered after all reactives are created

    ## UI: Card wrapper with code generation ####
    output$codes <- renderUI({
      if (isTRUE(mode() == "Markeren")) {
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            lang()$t("Codes"),
            paste0(
              lang()$t(
                "Bewerk hier de codes waarnaar het taalmodel relevante delen van de teksten zal markeren."
              ),
              lang()$t(
                " Gebruik de '+'- en '-'-knoppen om codes toe te voegen of te verwijderen."
              ),
              lang()$t(
                " Gebruik tenslotte de save/edit-knop om de codes op te slaan (of weer te kunnen bewerken)."
              )
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
    interrupter <- AsyncInterruptor$new()
    # Queue to communicate between async/main process
    queue <- shinyQueue()

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

    shiny::exportTestValues(
      n_fields = fields$unique_non_empty_count(),
      txt_in_fields = fields$texts(),
      isEditing = fields$editing(),
      generated_codes = generated_codes()
    )

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

        # Log code generation start
        log_info(
          sprintf(
            "Code generation started: n_texts=%d",
            length(texts$preprocessed)
          ),
          component = "codes"
        )

        # Empty all previously generated codes
        generated_codes(NULL)
        # Reset fields to empty (will be populated after generation)
        fields$set_values(c(""))

        # Disable generateCodes button during generation
        shinyjs::disable("generateCodes")

        # Set model
        llm_provider <- models$main
        chunk_settings <- .kwallm_marking_code_generation_chunk_settings(
          context_window
        )

        # Async generate codes
        queue$consumer$start()
        log_context <- log_context_capture(is_async = TRUE)
        worker_payload <- if (
          exists("kwallm_mori_share_worker_payload", mode = "function")
        ) {
          kwallm_mori_share_worker_payload(list(texts = texts$preprocessed))
        } else {
          list(
            args = list(texts = texts$preprocessed),
            guard = list(),
            scope_key = NULL
          )
        }
        shared_memory_guard <- worker_payload$guard

        kwallm_mirai_submit(
          {
            kwallm_worker_bootstrap(
              task = "code_generation",
              app_root = app_root,
              worker_options = worker_options,
              log_context = log_context
            )
            if (exists("kwallm_mori_resolve_worker_arg", mode = "function")) {
              texts <- kwallm_mori_resolve_worker_arg(texts, mori_scope_key)
            }

            generate_codes_by_reading_texts(
              texts = texts,
              text_size_tokens = text_size_tokens,
              overlap_size_tokens = overlap_size_tokens,
              research_background = research_background,
              llm_provider = llm_provider,
              queue = queue,
              interrupter = interrupter,
              language = language
            )
          },
          .args = c(
            list(
              app_root = kwallm_worker_app_root(),
              worker_options = kwallm_worker_capture_options(),
              log_context = log_context,
              mori_scope_key = worker_payload$scope_key,
              texts = worker_payload$args$texts,
              text_size_tokens = chunk_settings$text_size_tokens,
              overlap_size_tokens = chunk_settings$overlap_size_tokens,
              research_background = research_background(),
              llm_provider = llm_provider,
              queue = queue,
              interrupter = interrupter,
              language = lang()$get_translation_language()
            ),
            kwallm_worker_bootstrap_globals()
          )
        ) %...>%
          {
            force(shared_memory_guard)
            on.exit(kwallm_mori_release_guard(shared_memory_guard), add = TRUE)
            generated_codes(.)
            code_generation_in_progress(FALSE)
            log_info(
              sprintf("Code generation complete: n_codes=%d", length(.)),
              component = "codes"
            )
            shinyjs::delay(500, queue$consumer$stop())
          } %...!%
          {
            force(shared_memory_guard)
            on.exit(kwallm_mori_release_guard(shared_memory_guard), add = TRUE)
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
      unique_non_empty_count = fields$unique_non_empty_count,
      has_duplicates = fields$has_duplicates
    ))
  })
}

# 3 Example/development usage --------------------------------------
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
