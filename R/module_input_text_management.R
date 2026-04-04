# Module for managing text anonymization and preprocessing
#
# This version integrates global options that control:
#   * which anonymization methods are available to users;
#   * which method is selected by default.
#
# The following options are recognised (with sensible fall‑backs):#
#   anonymization__default        – character, one of "none", "regex", or "gliner"
#   anonymization__none           – logical,   whether the "none" method is offered      (default TRUE)
#   anonymization__regex          – logical,   whether the simple regex method is offered (default TRUE)
#   anonymization__gliner_model   – logical,   whether the GLiNER method is offered       (default FALSE)
#
# If the configured *default* method is not available, the module will
# gracefully fall back to the first available method in the order
# regex → gliner → none.

# 1 UI & server ------------------------------------------------------
text_management_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

text_management_server <- function(
  id,
  document_texts, # reactive vector with current document texts
  document_rows = NULL,
  gliner_model, # pre‑loaded GLiNER model object (or NULL)
  processing = reactiveVal(FALSE),
  lang = default_lang()
) {
  opt_none <- isTRUE(getOption("anonymization__none", TRUE))
  opt_regex <- isTRUE(getOption("anonymization__regex", TRUE))
  opt_gliner <- isTRUE(getOption("anonymization__gliner_model", FALSE))

  # Ensure at least one anonymization method is enabled
  if (
    !opt_none &&
      !opt_regex &&
      !opt_gliner
  ) {
    stop("At least one anonymization method must be enabled via options.")
  }

  # Resolve the default anonymization method, falling back if the configured

  # default is not enabled (documented order: regex -> gliner -> none).
  opt_default <- getOption("anonymization__default", "regex")
  if (!(opt_default %in% c("none", "regex", "gliner"))) {
    warning(
      "Invalid default anonymization method '",
      opt_default,
      "'; falling back."
    )
    opt_default <- NA_character_
  }

  available_modes <- c(
    none = if (opt_none) "none" else NA_character_,
    simple = if (opt_regex) "simple" else NA_character_,
    gliner = if (opt_gliner) "gliner" else NA_character_
  ) |>
    stats::na.omit() |>
    unname()

  configured_default_mode <- switch(
    opt_default,
    none = "none",
    regex = "simple",
    gliner = "gliner",
    NULL
  )

  if (
    !is.null(configured_default_mode) &&
      configured_default_mode %in% available_modes
  ) {
    initial_mode <- configured_default_mode
  } else {
    initial_mode <- intersect(c("simple", "gliner", "none"), available_modes)[1]

    if (!is.na(opt_default)) {
      warning(
        "Default anonymization method '",
        opt_default,
        "' is not enabled; falling back to '",
        initial_mode,
        "'."
      )
    }
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (length(available_modes) == 0) {
      stop("At least one anonymization method must be enabled via options.")
    }

    # -- 1  Child module: GLiNER ------------------------------------
    gliner <- gliner_server(
      id = "gliner", # namespacing inside current module
      pii_texts = reactive(input_rows()$document_text),
      lang = lang,
      gliner_model = gliner_model
    )

    # -- 2  State ----------------------------------------------------
    anonymization_mode <- reactiveVal(initial_mode)

    # document_text = current rows before anonymization/preprocessing.
    # preprocessed = unique texts the LLM will actually see.
    # analysis_units = lookup table from analysis_unit_id to one preprocessed text.
    # df = row-level bridge from source row -> current document row -> analysis unit.
    texts <- reactiveValues(
      document_text = NULL,
      preprocessed = NULL,
      analysis_units = NULL,
      df = NULL,
      anonymization_mode = NULL,
      anonymization_requested_mode = NULL,
      anonymization_applied_mode = NULL,
      anonymization_completed = NULL
    )

    input_rows <- reactive({
      # Upstream modules may already have split one source row into many
      # document rows. Keep that lineage intact when it is provided.
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

    shiny::exportTestValues(
      anonymization_mode = anonymization_mode(),
      anonymization_requested_mode = texts$anonymization_requested_mode,
      anonymization_applied_mode = texts$anonymization_applied_mode,
      anonymization_completed = texts$anonymization_completed,
      texts__document_text = texts$document_text,
      texts__preprocessed = texts$preprocessed,
      texts__analysis_units = texts$analysis_units,
      texts__df = texts$df
    )

    # -- 3  UI: main card -------------------------------------------
    output$card <- renderUI({
      req(lang())
      n_pre <- if (is.null(texts$preprocessed)) {
        0
      } else {
        length(texts$preprocessed)
      }

      tagList(
        tags$style(HTML(
          "
          /* --- icon buttons in text‑management (GLiNER / Regex / None) --- */
          .tm-icon {
            padding: 2px;              /* same visual feel as .llm-icon */
            border-radius: 2px;
            transition: all 0.2s ease;
            cursor: pointer;
          }

          .tm-icon:hover {
            background-color: #f0f0f0;
            box-shadow: 0 0 5px rgba(0,0,0,0.15);
            transform: scale(1.05);
          }

          .tm-icon-active {
            background-color: #f0f0f0;
            box-shadow: 0 0 5px rgba(0,0,0,0.15);
            transform: scale(1.05);
            cursor: default;           /* disable pointer while active */
          }
        "
        )),

        bslib::card(
          class = "card",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center w-100",
              span(
                lang()$t('Teksten'),
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  paste0(
                    lang()$t(
                      "Hier kun je de teksten bekijken die zullen worden verwerkt."
                    ),
                    lang()$t(
                      " Dubbele of gelijk geanonimiseerde teksten worden voor LLM-calls hergebruikt als één analyse-eenheid, terwijl de originele rijen behouden blijven."
                    ),
                    lang()$t(
                      " Daarnaast kan je kiezen om de teksten te anonimiseren met behulp van regex of een GLiNER-model. Regex verwijdert e-mailadressen, telefoonnummers en (Nederlandse) postcodes. Het GLiNER-model kan verschillende vormen van PII detecteren."
                    ),
                    lang()$t(
                      " Anonimisering vindt lokaal plaats voordat de teksten naar het grote taalmodel worden gestuurd."
                    )
                  )
                )
              ),
              uiOutput(ns('mode_selection'))
            )
          ),
          card_body(
            div(
              class = 'text-center',
              ## add the helper class here ↓↓↓
              actionButton(
                ns('open_text_table_modal'),
                paste0(
                  lang()$t('Bekijk tabel'),
                  ' (',
                  n_pre,
                  ' ',
                  lang()$t('teksten'),
                  ')'
                ),
                disabled = (n_pre == 0),
                class = 'btn btn-primary tm-fullwidth-btn'
              ),
              br(),
              br(),
              uiOutput(ns('preprocess_counts'))
            )
          )
        )
      )
    })

    # -- 4  Mode selector icons -------------------------------------
    output$mode_selection <- renderUI({
      cur <- anonymization_mode()

      # Build list of available buttons
      buttons <- list()
      if ("none" %in% available_modes) {
        buttons <- c(
          buttons,
          list(list(
            id = "none",
            icon = "x-square",
            title = lang()$t("Geen anonimisering"),
            tooltip = lang()$t("Geen anonimisering")
          ))
        )
      }
      if ("simple" %in% available_modes) {
        buttons <- c(
          buttons,
          list(list(
            id = "simple",
            icon = "regex",
            title = "Regex",
            tooltip = lang()$t("Eenvoudige anonimisering met regex")
          ))
        )
      }
      if ("gliner" %in% available_modes) {
        buttons <- c(
          buttons,
          list(list(
            id = "gliner",
            icon = "magic",
            title = "GLiNER",
            tooltip = lang()$t("Geavanceerde anonimisering met GLiNER-model")
          ))
        )
      }

      icon_toggle_group(
        ns = ns,
        buttons = buttons,
        active_id = cur,
        css_prefix = "tm-icon"
      )
    })

    # Click observers ------------------------------------------------
    if ("none" %in% available_modes) {
      observeEvent(input$select_none, {
        req(!isTRUE(processing()))
        anonymization_mode("none")
        log_action("anonymization_mode_changed", details = "none")
      })
    }
    if ("simple" %in% available_modes) {
      observeEvent(input$select_simple, {
        req(!isTRUE(processing()))
        anonymization_mode("simple")
        log_action("anonymization_mode_changed", details = "regex")
      })
    }
    if ("gliner" %in% available_modes) {
      observeEvent(input$select_gliner, {
        req(!isTRUE(processing()))
        anonymization_mode("gliner")
        log_action("anonymization_mode_changed", details = "gliner")
      })
    }

    # Highlight active icon (add/remove class) ----------------------
    observe({
      lapply(c("none", "simple", "gliner"), function(m) {
        # skip unavailable modes
        if (!(m %in% available_modes)) {
          return()
        }
        shinyjs::removeClass(ns(paste0("select_", m)), "tm-icon-active")
      })
      shinyjs::addClass(
        ns(paste0("select_", anonymization_mode())),
        "tm-icon-active"
      )
    })

    # Disable selectors while processing ----------------------------
    observe({
      lapply(c("none", "simple", "gliner"), function(m) {
        id <- paste0("select_", m)
        if (!(m %in% available_modes)) {
          return()
        }
        if (isTRUE(processing())) {
          shinyjs::disable(id)
        } else {
          shinyjs::enable(id)
        }
      })
    })

    # -- 5  Compute/refresh texts -----------------------------------
    # Track previous state to avoid duplicate logs
    prev_text_state <- reactiveVal(list(
      source = 0,
      document = 0,
      unique = 0,
      mode = ""
    ))

    observe({
      req(input_rows())
      mode <- anonymization_mode()
      requested_mode <- if (identical(mode, "simple")) "regex" else mode
      anonymization_completed <- TRUE
      document_text_vals <- input_rows()$document_text

      out <- switch(
        mode,
        none = document_text_vals,
        simple = pre_process_texts(document_text_vals, lang = lang()),
        gliner = {
          if (isTRUE(gliner$done)) {
            unname(gliner$anonymized_texts)
          } else {
            anonymization_completed <- FALSE
            document_text_vals
          }
        }
      )
      applied_mode <- switch(
        mode,
        simple = "regex",
        gliner = if (isTRUE(gliner$done)) "gliner" else "none",
        "none"
      )

      # Many document rows can collapse to the same analysis unit after
      # anonymization/preprocessing. The LLM only sees the unique texts.
      analysis_unit_id <- match(out, unique(out))
      analysis_units <- data.frame(
        analysis_unit_id = seq_along(unique(out)),
        preprocessed = unique(out),
        stringsAsFactors = FALSE
      )

      texts$document_text <- document_text_vals
      texts$preprocessed <- analysis_units$preprocessed
      texts$analysis_units <- analysis_units
      # Keep row-level lineage so results can later fan back out from one
      # analysis unit to all document rows that reuse it.
      texts$df <- data.frame(
        input_rows(),
        preprocessed = out,
        analysis_unit_id = as.integer(analysis_unit_id),
        stringsAsFactors = FALSE
      )
      texts$anonymization_mode <- mode
      texts$anonymization_requested_mode <- requested_mode
      texts$anonymization_applied_mode <- applied_mode
      texts$anonymization_completed <- anonymization_completed

      # Only log when there's an actual change in counts
      new_state <- list(
        source = length(unique(texts$df$source_document_id %||% integer())),
        document = length(texts$document_text),
        unique = length(texts$preprocessed),
        mode = mode
      )
      old_state <- prev_text_state()

      if (
        new_state$source != old_state$source ||
          new_state$document != old_state$document ||
          new_state$unique != old_state$unique ||
          new_state$mode != old_state$mode
      ) {
        log_info(
          sprintf(
            "Text count changed: source=%d, document=%d, unique=%d, mode=%s",
            new_state$source,
            new_state$document,
            new_state$unique,
            mode
          ),
          component = "text"
        )
        prev_text_state(new_state)
      }
    })

    # -- 6  Summary counts ------------------------------------------
    output$preprocess_counts <- renderUI({
      req(texts$preprocessed)
      count_labels <- if (identical(lang()$get_translation_language(), "en")) {
        list(
          source = "Uploaded text rows",
          document = "Current texts/chunks",
          units = "Unique analysis units sent to the LLM",
          reused = "Rows reusing an existing analysis"
        )
      } else {
        list(
          source = "Geuploade tekstrijen",
          document = "Huidige teksten/chunks",
          units = "Unieke analyse-eenheden voor het LLM",
          reused = "Rijen die een bestaande analyse hergebruiken"
        )
      }

      count_box <- {
        source_total <- length(unique(texts$df$source_document_id))
        document_total <- nrow(texts$df)
        unit_total <- length(texts$preprocessed)
        reused_total <- max(document_total - unit_total, 0)

        count_items <- list(
          div(
            class = "d-flex align-items-center justify-content-between gap-3",
            span(class = "text-muted small", count_labels$source),
            span(class = "badge bg-secondary", source_total)
          )
        )

        if (document_total != source_total) {
          count_items[[length(count_items) + 1L]] <- div(
            class = "d-flex align-items-center justify-content-between gap-3",
            span(class = "text-muted small", count_labels$document),
            span(class = "badge bg-secondary", document_total)
          )
        }

        count_items[[length(count_items) + 1L]] <- div(
          class = "d-flex align-items-center justify-content-between gap-3",
          span(class = "text-muted small", count_labels$units),
          span(class = "badge bg-secondary", unit_total)
        )

        if (reused_total > 0) {
          count_items[[length(count_items) + 1L]] <- div(
            class = "d-flex align-items-center justify-content-between gap-3",
            span(class = "text-muted small", count_labels$reused),
            span(class = "badge bg-secondary", reused_total)
          )
        }

        div(
          class = "border rounded p-2 mb-3 bg-light fade-in gap-2",
          do.call(tagList, count_items)
        )
      }

      mode <- anonymization_mode()
      if (mode == "simple") {
        # counts for regex anonymisation -----------------------------------
        txts <- texts$preprocessed
        email <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang()$t("<< e-mailadres verwijderd >>"))
        ))
        phone <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang()$t("<< (telefoon)nummer verwijderd >>"))
        ))
        postal <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang()$t("<< postcode verwijderd >>"))
        ))

        simp_box <- div(
          class = "border rounded p-2 bg-light fade-in",
          div(
            class = "text-muted small mb-1",
            lang()$t("Persoonsgegevens geanonimiseerd:")
          ),
          div(
            class = "small d-flex flex-wrap justify-content-center align-items-center gap-2",
            # e-mail
            div(
              class = "d-flex align-items-center gap-1",
              bsicons::bs_icon("envelope"),
              span(class = "badge bg-secondary", email),
              span(class = "text-muted", lang()$t("e-mail(s)"))
            ),
            # phone
            div(
              class = "d-flex align-items-center gap-1",
              bsicons::bs_icon("telephone"),
              span(class = "badge bg-secondary", phone),
              span(class = "text-muted", lang()$t("nummer(s)"))
            ),
            # postcode
            div(
              class = "d-flex align-items-center gap-1",
              bsicons::bs_icon("mailbox"),
              span(class = "badge bg-secondary", postal),
              span(class = "text-muted", lang()$t("postcode(s)"))
            )
          )
        )

        tagList(div(
          class = "mx-auto",
          style = "max-width:700px;",
          count_box,
          simp_box
        ))
      } else if (mode == "gliner") {
        # GLiNER counts ------------------------------------------------------
        if (!opt_gliner) {
          return(div(
            class = "text-center text-muted small",
            lang()$t("GLiNER-anonimisering is niet beschikbaar")
          ))
        }

        # persistent open‑modal button
        open_btn <- div(
          class = "text-center mb-3",
          actionButton(
            ns("open_gliner_modal"),
            "Open GLiNER",
            class = "btn btn-primary"
          )
        )

        if (!isTRUE(gliner$done)) {
          tagList(
            open_btn,
            p(
              class = "text-muted small mt-1",
              lang()$t("GLiNER-anonimisering nog niet voltooid...")
            )
          )
        } else {
          counts_tbl <- gliner$pii_label_counts %||%
            tibble::tibble(count = integer())
          total_pii <- if (nrow(counts_tbl) == 0) {
            0
          } else {
            sum(counts_tbl$count, na.rm = TRUE)
          }

          counts_ui <- div(
            class = "d-flex align-items-center justify-content-center gap-2",
            bsicons::bs_icon("shield-lock"),
            span(class = "badge bg-secondary", total_pii)
          )

          tagList(
            open_btn,
            div(
              class = "mx-auto",
              style = "max-width:700px;",
              count_box,
              div(
                class = "border rounded p-2 bg-light fade-in",
                div(
                  class = "text-muted small mb-1",
                  lang()$t("Persoonsgegevens geanonimiseerd:")
                ),
                counts_ui
              )
            )
          )
        }
      } else {
        # mode == "none"
        count_box
      }
    })

    # Open GLiNER modal ---------------------------------------------
    observeEvent(input$open_gliner_modal, {
      req(!isTRUE(processing()))
      isolate({
        log_action(
          "gliner_open_clicked",
          details = sprintf(
            "n_texts=%d mode=%s",
            length(texts$preprocessed %||% character(0)),
            anonymization_mode() %||% "unknown"
          )
        )
        if (is.function(gliner$start)) gliner$start()
      })
    })

    # -- 7  Modal: text table ---------------------------------------
    observeEvent(input$open_text_table_modal, {
      showModal(modalDialog(
        title = lang()$t("Teksten"),
        tags$div(
          style = "display:none;",
          `data-kwallm-modal-id` = "text_table_modal",
          `data-kwallm-modal-details` = sprintf(
            "module=text_management, n_texts=%d",
            length(texts$preprocessed)
          )
        ),
        DT::dataTableOutput(ns("text_table")),
        easyClose = TRUE,
        footer = tagList(
          div(
            class = "text-center w-100",
            downloadButton(
              ns("download_preprocessed"),
              label = lang()$t("Download"),
              class = "btn btn-sm"
            ) |>
              bslib::tooltip(
                lang()$t(
                  "Download de voorbewerkte teksten als .csv-bestand. Dit is niet nodig, maar kan soms handig zijn (als je bijv. alleen de anonimiserings- of tekst-splits-functies van de app wilde gebruiken)."
                ),
                placement = "top"
              )
          ),
          actionButton(
            ns("close_text_table_modal"),
            lang()$t("Sluiten"),
            class = "btn-secondary"
          )
        ),
        size = "l"
      ))
    })

    # Log text table modal close
    observeEvent(input$close_text_table_modal, {
      removeModal()
    })

    output$text_table <- DT::renderDataTable(
      {
        data.frame(Tekst = texts$preprocessed)
      },
      options = list(pageLength = 5, scrollX = TRUE)
    )

    observeEvent(anonymization_mode(), {
      log_info(
        sprintf("Anonymization mode changed to: %s", anonymization_mode()),
        component = "anonymization_mode"
      )
    })

    output$download_preprocessed <- downloadHandler(
      filename = function() {
        paste0("preprocessed_texts", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(texts$df)
        log_action(
          "preprocessed_texts_download_started",
          details = sprintf("n_texts=%d", nrow(texts$df))
        )
        log_info(
          sprintf("Preprocessed texts downloaded: n_texts=%d", nrow(texts$df)),
          component = "download"
        )
        vroom::vroom_write(
          x = texts$df,
          file = file,
          delim = ";"
        )
      }
    )

    # 8 Return -------------------------------------------------------
    return(texts)
  })
}


# 2 Helper function for preprocessing texts ------------------------
pre_process_texts <- function(
  txts,
  lang = shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  )
) {
  # Ensure stringr is loaded
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install and load the 'stringr' package.")
  }

  txts <- stringr::str_squish(txts)

  # Find all e-mail addresses, replace with "<< e-mailadres verwijderd >>"
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex(
      "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
      ignore_case = TRUE
    ),
    lang$t("<< e-mailadres verwijderd >>")
  )

  # Find all phone numbers, replace with "<< (telefoon)nummer verwijderd >>"
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex(
      "(?<!\\S)(?=(?:\\D*\\d){7})\\+?[\\d\\-\\.\\(\\)\\s]{7,}?(?=\\s|$|[[:punct:]])",
      ignore_case = TRUE
    ),
    lang$t("<< (telefoon)nummer verwijderd >>")
  )

  # Find all Dutch postal codes, replace with "<< postcode verwijderd >>"
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex("\\b\\d{4}\\s*[a-zA-Z]{2}\\b", ignore_case = TRUE),
    lang$t("<< postcode verwijderd >>")
  )

  return(txts)
}


# 3 Example/development usage --------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)
  library(DT)

  # Allows to load Python & interrupt R session without fatal R crash:
  Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

  # Example global options ---------------------------------------------------
  options(
    anonymization__default = "regex", # "none" | "regex" | "gliner"
    anonymization__none = TRUE,
    anonymization__regex = TRUE,
    anonymization__gliner_model = TRUE # Enable GLiNER model usage
  )

  # Load model (use NULL for demo if you don't have it):
  gliner_model <- NULL # or gliner_load_model()

  ui <- bslib::page_fluid(
    shinyjs::useShinyjs(),
    text_management_ui("tm")
  )

  server <- function(input, output, session) {
    document_texts <- reactive(c(
      "My name is Luka Koning, I live on 5th avenue street in London.",
      "Call me on +3125251512 or mail me at bob@bobthebob.com",
      "It's a nice and sunny day today!"
    ))

    text_management_server(
      "tm",
      document_texts = document_texts,
      gliner_model = gliner_model
    )
  }

  shinyApp(ui, server)
}
