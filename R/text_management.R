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

#### 1 UI & server ###################################################

text_management_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

text_management_server <- function(
  id,
  raw_texts, # reactive vector with raw texts
  gliner_model, # pre‑loaded GLiNER model object (or NULL)
  processing = reactiveVal(FALSE),
  lang = reactiveVal(
    shiny.i18n::Translator$new(translation_json_path = "language/language.json")
  )
) {
  # Ensure at least one anonymization method is enabled
  if (
    !isTRUE(getOption("anonymization__none", TRUE)) &&
      !isTRUE(getOption("anonymization__regex", TRUE)) &&
      !isTRUE(getOption("anonymization__gliner_model", FALSE))
  ) {
    stop("At least one anonymization method must be enabled via options.")
  }

  # Ensure the default anonymization method is also enabled
  opt_default <- getOption("anonymization__default", "regex")
  if (!(opt_default %in% c("none", "regex", "gliner"))) {
    stop("Invalid default anonymization method specified in options.")
  }
  # Check that default method is also enabled in options
  if (opt_default == "none" && !getOption("anonymization__none", TRUE)) {
    stop("Default anonymization method 'none' is not enabled in options.")
  }
  if (opt_default == "regex" && !getOption("anonymization__regex", TRUE)) {
    stop("Default anonymization method 'regex' is not enabled in options.")
  }
  if (
    opt_default == "gliner" && !getOption("anonymization__gliner_model", FALSE)
  ) {
    stop("Default anonymization method 'gliner' is not enabled in options.")
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- 0  Global options ------------------------------------------
    opt_default <- getOption("anonymization__default", "regex")
    opt_none <- isTRUE(getOption("anonymization__none", TRUE))
    opt_regex <- isTRUE(getOption("anonymization__regex", TRUE))
    opt_gliner <- isTRUE(getOption("anonymization__gliner_model", FALSE))

    # Determine which methods are actually available ----------------
    available_modes <- c(
      none = if (opt_none) "none" else NA,
      simple = if (opt_regex) "simple" else NA,
      gliner = if (opt_gliner) "gliner" else NA
    ) |>
      stats::na.omit() |>
      unname()

    if (length(available_modes) == 0) {
      stop("At least one anonymization method must be enabled via options.")
    }

    # Determine the initial mode ------------------------------------
    initial_mode <- switch(
      opt_default,
      none = if ("none" %in% available_modes) "none" else NULL,
      regex = if ("simple" %in% available_modes) "simple" else NULL,
      gliner = if ("gliner" %in% available_modes) "gliner" else NULL,
      NULL
    )
    if (is.null(initial_mode)) {
      # fall‑back order: regex → gliner → none
      fallback_order <- c("simple", "gliner", "none")
      initial_mode <- intersect(fallback_order, available_modes)[1]
    }

    # -- 1  Child module: GLiNER ------------------------------------
    gliner <- gliner_server(
      id = "gliner", # namespacing inside current module
      pii_texts = reactive(raw_texts()),
      lang = lang,
      gliner_model = gliner_model
    )

    # -- 2  State ----------------------------------------------------
    anonymization_mode <- reactiveVal(initial_mode)

    texts <- reactiveValues(raw = NULL, preprocessed = NULL, df = NULL)

    # -- 3  UI: main card -------------------------------------------
    output$card <- renderUI({
      req(lang())
      n_pre <- if (is.null(texts$preprocessed)) 0 else
        length(texts$preprocessed)

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
              span(lang()$t('Teksten')),
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
      # Helper to produce one icon div
      make_icon <- function(id_suffix, icon_name, title_txt, tooltip_txt) {
        div(
          id = ns(paste0("select_", id_suffix)),
          class = paste("tm-icon", if (cur == id_suffix) "tm-icon-active"),
          title = title_txt,
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            ns(paste0("select_", id_suffix))
          ),
          bs_icon(icon_name, class = "tm-icon-img", style = "height:20px;")
        ) |>
          bslib::tooltip(tooltip_txt, placement = "bottom")
      }

      # Assemble only the icons that are available
      icon_list <- tagList()
      if ("none" %in% available_modes)
        icon_list <- tagAppendChildren(
          icon_list,
          make_icon(
            "none",
            "x-square",
            lang()$t("Geen anonimisering"),
            lang()$t("Geen anonimisering")
          )
        )
      if ("simple" %in% available_modes)
        icon_list <- tagAppendChildren(
          icon_list,
          make_icon(
            "simple",
            "regex",
            "Regex",
            lang()$t("Eenvoudige anonimisering met regex")
          )
        )
      if ("gliner" %in% available_modes)
        icon_list <- tagAppendChildren(
          icon_list,
          make_icon(
            "gliner",
            "magic",
            "GLiNER",
            lang()$t("Geavanceerde anonimisering met GLiNER-model")
          )
        )

      div(class = "d-flex justify-content-center gap-3", icon_list)
    })

    # Click observers ------------------------------------------------
    if ("none" %in% available_modes) {
      observeEvent(input$select_none, {
        req(!isTRUE(processing()))
        anonymization_mode("none")
      })
    }
    if ("simple" %in% available_modes) {
      observeEvent(input$select_simple, {
        req(!isTRUE(processing()))
        anonymization_mode("simple")
      })
    }
    if ("gliner" %in% available_modes) {
      observeEvent(input$select_gliner, {
        req(!isTRUE(processing()))
        anonymization_mode("gliner")
      })
    }

    # Highlight active icon (add/remove class) ----------------------
    observe({
      lapply(c("none", "simple", "gliner"), function(m) {
        # skip unavailable modes
        if (!(m %in% available_modes)) return()
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
        if (!(m %in% available_modes)) return()
        if (isTRUE(processing())) glossy <- shinyjs::disable(id) else
          shinyjs::enable(id)
      })
    })

    # -- 5  Compute/refresh texts -----------------------------------
    observe({
      req(raw_texts())
      mode <- anonymization_mode()

      out <- switch(
        mode,
        none = raw_texts(),
        simple = pre_process_texts(raw_texts(), lang = lang()),
        gliner = {
          if (isTRUE(gliner$done)) unname(gliner$anonymized_texts) else
            raw_texts()
        }
      )

      texts$raw <- raw_texts()
      texts$preprocessed <- unique(out)
      texts$df <- data.frame(
        raw = raw_texts(),
        preprocessed = out,
        stringsAsFactors = FALSE
      )
    })

    # -- 6  Summary counts ------------------------------------------
    output$preprocess_counts <- renderUI({
      req(texts$preprocessed)
      dup_box <- {
        total <- length(texts$raw)
        uniq <- length(texts$preprocessed)
        div(
          class = "border rounded p-2 mb-3 bg-light fade-in gap-2",
          div(
            class = "text-muted small mb-1 text-center",
            lang()$t("Dubbele teksten verwijderd:")
          ),
          div(
            class = "d-flex align-items-center justify-content-center gap-2",
            bs_icon("trash"),
            span(class = "badge bg-secondary", total - uniq)
          )
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
              bs_icon("envelope"),
              span(class = "badge bg-secondary", email),
              span(class = "text-muted", lang()$t("e-mail(s)"))
            ),
            # phone
            div(
              class = "d-flex align-items-center gap-1",
              bs_icon("telephone"),
              span(class = "badge bg-secondary", phone),
              span(class = "text-muted", lang()$t("nummer(s)"))
            ),
            # postcode
            div(
              class = "d-flex align-items-center gap-1",
              bs_icon("mailbox"),
              span(class = "badge bg-secondary", postal),
              span(class = "text-muted", lang()$t("postcode(s)"))
            )
          )
        )

        tagList(div(
          class = "mx-auto",
          style = "max-width:700px;",
          dup_box,
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
          total_pii <- if (nrow(counts_tbl) == 0) 0 else
            sum(counts_tbl$count, na.rm = TRUE)

          counts_ui <- div(
            class = "d-flex align-items-center justify-content-center gap-2",
            bs_icon("shield-lock"),
            span(class = "badge bg-secondary", total_pii)
          )

          tagList(
            open_btn,
            div(
              class = "mx-auto",
              style = "max-width:700px;",
              dup_box,
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
        dup_box
      }
    })

    # Open GLiNER modal ---------------------------------------------
    observeEvent(input$open_gliner_modal, {
      req(!isTRUE(processing()))
      isolate({
        if (is.function(gliner$start)) gliner$start()
      })
    })

    # -- 7  Modal: text table ---------------------------------------
    observeEvent(input$open_text_table_modal, {
      showModal(modalDialog(
        title = lang()$t("Teksten"),
        DT::dataTableOutput(ns("text_table")),
        easyClose = TRUE,
        footer = modalButton(lang()$t("Sluiten")),
        size = "l"
      ))
    })

    output$text_table <- DT::renderDataTable(
      {
        data.frame(Tekst = texts$preprocessed)
      },
      options = list(pageLength = 5, scrollX = TRUE)
    )

    # 8 Return -------------------------------------------------------
    return(texts)
  })
}


#### 2 Helper function for preprocessing texts ######################

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


#### 3 Example/development usage ###################################

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
    raw <- reactive(c(
      "My name is Luka Koning, I live on 5th avenue street in London.",
      "Call me on +3125251512 or mail me at bob@bobthebob.com",
      "It's a nice and sunny day today!"
    ))

    text_management_server(
      "tm",
      raw_texts = raw,
      gliner_model = gliner_model
    )
  }

  shinyApp(ui, server)
}
