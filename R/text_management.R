# Module for managing text anonymization and preprocessing

#### 1 UI & server ####

text_management_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("card"))
}

text_management_server <- function(
  id,
  raw_texts, # reactive vector with raw texts
  gliner_model, # pre‑loaded GLiNER model object
  processing = reactiveVal(FALSE),
  lang = reactiveVal(
    shiny.i18n::Translator$new(translation_json_path = "language/language.json")
  )
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- 0 Helper ------------------------------------------------------
    shinyjs::useShinyjs()

    # -- 1  Child module: GLiNER ------------------------------------
    gliner <- gliner_server(
      id = "gliner", # namespacing inside current module
      pii_texts = reactive(raw_texts()),
      lang = lang,
      gliner_model = gliner_model
    )

    # -- 2  State ----------------------------------------------------
    anonymization_mode <- reactiveVal("simple") # default

    texts <- reactiveValues(raw = NULL, preprocessed = NULL, df = NULL)

    # -- 3  UI: main card -------------------------------------------
    output$card <- renderUI({
      req(lang())
      n_pre <- if (is.null(texts$preprocessed)) 0 else
        length(texts$preprocessed)

      tagList(
        tags$style(HTML(
          "
          /* --- icon buttons in text-management (GLiNER / Regex / None) --- */
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
      div(
        class = "d-flex justify-content-center gap-3",

        # NONE
        div(
          id = ns("select_none"),
          class = paste("tm-icon", if (cur == "none") "tm-icon-active"),
          title = lang()$t("Geen anonimisering"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            ns("select_none")
          ),
          bs_icon("x-circle", class = "tm-icon-img", style = "height:20px;")
        ) |>
          bslib::tooltip(lang()$t("Geen anonimisering"), placement = "bottom"),

        # SIMPLE
        div(
          id = ns("select_simple"),
          class = paste("tm-icon", if (cur == "simple") "tm-icon-active"),
          title = "Regex",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            ns("select_simple")
          ),
          tags$img(src = "www/regex_avatar.png", height = "20", alt = "Regex")
        ) |>
          bslib::tooltip(
            lang()$t("Eenvoudige anonimisering met regex"),
            placement = "bottom"
          ),

        # GLINER
        div(
          id = ns("select_gliner"),
          class = paste("tm-icon", if (cur == "gliner") "tm-icon-active"),
          title = "GLiNER",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            ns("select_gliner")
          ),
          tags$img(src = "www/gliner_avatar.png", height = "20", alt = "GLiNER")
        ) |>
          bslib::tooltip(
            "Geavanceerde anonimisering met GLiNER-model",
            placement = "bottom"
          )
      )
    })

    # Click observers
    observeEvent(input$select_none, anonymization_mode("none"))
    observeEvent(input$select_simple, anonymization_mode("simple"))
    observeEvent(input$select_gliner, {
      anonymization_mode("gliner")
      # isolate({
      #   if (is.function(gliner$start)) gliner$start()
      # })
    })

    # Highlight active icon (add/remove class)
    observe({
      modes <- c("none", "simple", "gliner")
      lapply(
        modes,
        function(m)
          shinyjs::removeClass(ns(paste0("select_", m)), "tm-icon-active")
      )
      shinyjs::addClass(
        ns(paste0("select_", anonymization_mode())),
        "tm-icon-active"
      )
    })

    # Disable selectors while processing (optional)
    observe({
      if (isTRUE(processing())) {
        shinyjs::disable("select_none")
        shinyjs::disable("select_simple")
        shinyjs::disable("select_gliner")
      } else {
        shinyjs::enable("select_none")
        shinyjs::enable("select_simple")
        shinyjs::enable("select_gliner")
      }
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
              class = "d-flex align-items-center gap-1", # tighter spacing
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
        if (!getOption("anonymization__gliner_model", FALSE)) {
          return(div(
            class = "text-center text-muted small",
            lang()$t("GLiNER-anonimisering is niet beschikbaar")
          ))
        }

        # persistent button
        open_btn <- div(
          class = "text-center mb-3",
          actionButton(
            ns("open_gliner_modal"),
            lang()$t("Open GLiNER"),
            class = "btn btn-primary"
          )
        )

        if (!isTRUE(gliner$done)) {
          ## anonymisation has not been run or not saved yet
          tagList(
            open_btn,
            p(
              class = "text-muted small mt-1",
              lang()$t("GLiNER-anonimisering niet afgerond")
            )
          )
        } else {
          ## anonymisation finished – show counts *plus* the same button
          tagList(
            open_btn,
            div(
              class = "mx-auto",
              style = "max-width:700px;",
              dup_box,

              # PII summary counts
              {
                counts_tbl <- gliner$pii_label_counts %||%
                  tibble::tibble(count = integer())

                total_pii <- if (nrow(counts_tbl) == 0) 0 else
                  sum(counts_tbl$count, na.rm = TRUE)

                counts_ui <- div(
                  class = "d-flex align-items-center justify-content-center gap-2",
                  bs_icon("shield-lock"),
                  span(class = "badge bg-secondary", total_pii)
                )

                div(
                  class = "border rounded p-2 bg-light fade-in",
                  div(
                    class = "text-muted small mb-1",
                    lang()$t("Persoonsgegevens geanonimiseerd:")
                  ),
                  counts_ui
                )
              }
            )
          )
        }
      } else {
        dup_box # mode "none"
      }
    })

    observeEvent(input$open_gliner_modal, {
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

    # 8 Return ---------------------------------------------------------
    return(texts)
  })
}


#### 2 Helper function for preprocessing texts ####

# To anonymize texts with regex patterns
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

  # Find all e-mail addresses, replace with "<< removed e-mail address >>"
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex(
      "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
      ignore_case = TRUE
    ),
    lang$t("<< e-mailadres verwijderd >>")
  )

  # Find all Dutch IBAN numbers, replace with "<< removed IBAN number >>"
  # (e.g., NL91ABNA0417164300)
  # txts <- stringr::str_replace_all(
  #   txts,
  #   stringr::regex(
  #     "\\bNL\\d{2}[A-Z]{4}\\d{10}\\b",
  #     ignore_case = TRUE
  #   ),
  #   "<< removed IBAN number >>"
  # )

  # Find all phone numbers, replace with "<< removed phone number >>"
  # May also find other numbers like BSN, KvK, etc.
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex(
      "(?<!\\S)(?=(?:\\D*\\d){7})\\+?[\\d\\-\\.\\(\\)\\s]{7,}?(?=\\s|$|[[:punct:]])",
      ignore_case = TRUE
    ),
    lang$t("<< (telefoon)nummer verwijderd >>")
  )

  # Find all Dutch postal codes, replace with "<< removed postal code >>"
  # (e.g., 1234AB or 1234 ab) - Original regex is good
  txts <- stringr::str_replace_all(
    txts,
    stringr::regex(
      "\\b\\d{4}\\s*[a-zA-Z]{2}\\b",
      ignore_case = TRUE
    ),
    lang$t("<< postcode verwijderd >>")
  )

  return(txts)
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)
  library(DT)

  # Allows to load Python & interrupt R session without fatal R crash:
  Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

  # Load model:
  if (!exists("gliner_model")) gliner_model <- gliner_load_model()

  options(
    anonymization__gliner_model = TRUE # Enable GLiNER model usage
  )

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

    text_management_server("tm", raw_texts = raw, gliner_model = gliner_model)
  }

  shinyApp(ui, server)
}
