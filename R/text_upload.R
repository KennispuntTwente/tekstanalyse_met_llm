# Module to upload files with the texts
# Can handle .txt, .csv, .xlsx, and .sav files.
# Can select a sheet for Excel files, and a specific column for files with multiple columns
# Can filter rows based on column values through a modal dialog
# Note: pre-processing of texts is handled in the text_management module,
#   this module only uploads the raw data

#### 1 UI ####

text_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "card-container",
      uiOutput(ns("card"))
    )
  )
}


#### 2 Server ####

text_upload_server <- function(
  id,
  processing,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$card <- renderUI({
      card(
        # ---- Card header -----------------------------------------------------
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            span(lang()$t("Upload teksten")),
            # Dynamic filter icon (updated from server for colour change)
            uiOutput(ns("filter_icon"))
          )
        ),
        # ---- Card body -------------------------------------------------------
        card_body(
          div(
            class = "d-flex justify-content-center",
            style = "width: 100%;",
            div(
              class = "d-flex flex-wrap justify-content-center gap-3",
              style = "max-width: 800px;",

              # ---------- File input -----------------------------------------
              div(
                class = "selector-container",
                style = "max-width: 300px;", # fixed width
                fileInput(
                  inputId = ns("text_file"),
                  label = "Upload (.txt, .csv, .xlsx, of .sav)",
                  accept = c(".txt", ".csv", ".xlsx", ".sav")
                )
              ),

              # ---------- Sheet selector (Excel only) ------------------------
              div(
                class = "selector-container",
                style = "max-width: 300px; min-height: 100px;", # reserved height
                uiOutput(ns("sheet_selector"))
              ),

              # ---------- Column selector -----------------------------------
              div(
                id = ns("column_container"), # ◄ NEW: easy hide/show
                class = "selector-container",
                style = "max-width: 300px; min-height: 100px;", # reserved height
                uiOutput(ns("column_selector"))
              )
            )
          )
        )
      )
    })

    # ---- Helpers ------------------------------------------------------------
    discard_empty <- function(x) {
      x <- x[!is.na(x)]
      x <- x[stringr::str_trim(x) != ""]
      unique(x)
    }

    # ---- Reactive values ----------------------------------------------------
    raw_texts <- reactiveVal(NULL) # vector of texts returned by module
    uploaded_data <- reactiveVal(NULL) # raw data (data.frame) read from file
    sheet_names <- reactiveVal(NULL) # character vector of Excel sheet names
    filter_spec <- reactiveVal(NULL) # list(col = <chr>, vals = <chr>) | NULL
    file_type <- reactiveVal(NULL) # ◄ NEW: current file extension

    # Logical reactive: is a filter currently active?
    filter_active <- reactive({
      spec <- filter_spec()
      df <- uploaded_data()
      if (is.null(spec) || is.null(df)) return(FALSE)
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) return(FALSE)
      col_vals <- df[[col]]
      filtered <- col_vals %in% spec$vals
      any(filtered) && sum(filtered) < nrow(df)
    })

    # Data after optional filtering -----------------------------------------
    filtered_data <- reactive({
      df <- uploaded_data()
      spec <- filter_spec()
      if (is.null(df) || is.null(spec)) return(df)

      # Default to "text" column for txt files
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) return(df)

      df[df[[col]] %in% spec$vals, , drop = FALSE]
    })

    # ---- File upload --------------------------------------------------------
    observeEvent(input$text_file, {
      req(input$text_file)

      # Reset all state -------------------------------------------------------
      raw_texts(NULL)
      uploaded_data(NULL)
      sheet_names(NULL)
      filter_spec(NULL)

      file_ext <- tools::file_ext(input$text_file$name)
      file_type(file_ext) # ◄ track for UI logic
      file_path <- input$text_file$datapath

      if (file_ext == "txt") {
        tryCatch(
          {
            txt <- discard_empty(readLines(file_path, encoding = "UTF-8"))

            # Treat plain-text as a one-column data-frame ----------------------
            df <- data.frame(text = txt, stringsAsFactors = FALSE)
            uploaded_data(df)
            raw_texts(df$text)

            # No column selector shown → no need to updateSelectInput
          },
          error = function(e) {
            showNotification(
              paste(lang()$t("Error bij lezen van tekstbestand:"), e$message),
              type = "error"
            )
          }
        )
      } else if (file_ext %in% c("csv", "tsv")) {
        tryCatch(
          {
            df <- vroom::vroom(file_path)
            uploaded_data(df)
          },
          error = function(e) {
            showNotification(
              paste(
                lang()$t("Error bij lezen van CSV/TSV bestand:"),
                e$message
              ),
              type = "error"
            )
          }
        )
      } else if (file_ext == "xlsx") {
        tryCatch(
          {
            sheets <- readxl::excel_sheets(file_path)
            sheet_names(sheets)
            # Wait for user to choose sheet before loading data
          },
          error = function(e) {
            showNotification(
              paste(lang()$t("Error bij lezen van Excel-bestand:"), e$message),
              type = "error"
            )
          }
        )
      } else if (file_ext == "sav") {
        tryCatch(
          {
            df <- haven::read_sav(file_path)
            uploaded_data(df)
          },
          error = function(e) {
            showNotification(
              paste(lang()$t("Error bij lezen van SAV-bestand:"), e$message),
              type = "error"
            )
          }
        )
      } else {
        showNotification(
          lang()$t("Niet ondersteund bestandstype"),
          type = "error"
        )
      }
    })

    # ---- Show / hide column selector depending on file type ---------------
    observe({
      if (is.null(file_type())) return()
      if (file_type() == "txt") {
        shinyjs::hide(ns("column_container"))
      } else {
        shinyjs::show(ns("column_container"))
      }
    })

    # ---- Sheet selector (Excel only) ---------------------------------------
    output$sheet_selector <- renderUI({
      req(sheet_names())
      selectInput(
        ns("sheet"),
        lang()$t("Selecteer sheet"),
        choices = sheet_names(),
        selected = sheet_names()[1]
      )
    })

    observeEvent(input$sheet, {
      req(input$text_file, input$sheet)
      file_path <- input$text_file$datapath
      tryCatch(
        {
          df <- readxl::read_excel(file_path, sheet = input$sheet)
          uploaded_data(df)
        },
        error = function(e) {
          showNotification(
            paste(lang()$t("Error bij lezen sheet:"), e$message),
            type = "error"
          )
        }
      )
    })

    # ---- Column selector ----------------------------------------------------
    output$column_selector <- renderUI({
      req(filtered_data())
      if (file_type() == "txt") return(NULL)
      cols <- names(filtered_data())
      # if (length(cols) <= 1) return(NULL)
      selectInput(
        ns("column"),
        lang()$t("Selecteer kolom met teksten"),
        choices = cols,
        selected = NULL
      )
    })

    observeEvent(input$column, {
      req(filtered_data())
      col <- input$column
      if (!is.null(col) && nzchar(col)) {
        txt <- filtered_data()[[col]]
        raw_texts(discard_empty(txt))
      }
    })

    # ---- Filter icon (dynamic colour) --------------------------------------
    output$filter_icon <- renderUI({
      style <- if (filter_active()) "color:#0d6efd;" else "color:#6c757d;"
      actionLink(
        ns("filter_btn"),
        icon("filter", lib = "font-awesome"),
        style = paste0(style, "font-size:1.25rem;")
      ) |>
        bslib::tooltip(lang()$t("Filter data"))
    })

    # ---- Filter modal -------------------------------------------------------
    observeEvent(input$filter_btn, {
      req(uploaded_data())

      showModal(modalDialog(
        title = lang()$t("Filter data"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,

        bslib::page(
          p(lang()$t(
            "Je kunt hier de data filteren op basis van waarden in een kolom. Selecteer een kolom en kies waarden. Rijen zonder de gekozen waarden worden uitgesloten."
          )),
          hr(),

          # Inputs centered in modal
          div(
            class = "d-flex flex-column align-items-center text-center",
            style = "width: 100%; gap: 1rem;",
            uiOutput(ns("filter_col_selector")),
            uiOutput(ns("filter_values_ui"))
          ),
          hr(),

          ## ------------------------------------------------------------------
          ##  L E F T                |                R I G H T
          ## ------------------------------------------------------------------
          div(
            class = "d-flex justify-content-between align-items-stretch gap-2",

            # Left: Sluiten
            div(
              class = "d-flex align-items-stretch",
              div(class = "h-100", modalButton(lang()$t("Sluiten")))
            ),

            # Center: Filter wissen
            div(
              class = "d-flex justify-content-center flex-grow-1 align-items-stretch",
              actionButton(
                ns("clear_filter"),
                label = tagList(icon("rotate-left"), lang()$t("Filter wissen")),
                class = "btn btn-warning h-100 w-100"
              )
            ),

            # Right: Toepassen
            div(
              class = "d-flex align-items-stretch",
              actionButton(
                ns("apply_filter"),
                label = tagList(icon("filter"), lang()$t("Toepassen")),
                class = "btn btn-primary h-100"
              )
            )
          )
        )
      ))
    })

    # Dynamic values selector -------------------------------------------------
    output$filter_col_selector <- renderUI({
      req(uploaded_data())
      if (file_type() == "txt") return(NULL) # Hide if plain text file

      shinyWidgets::pickerInput(
        ns("filter_col"),
        label = lang()$t("Kies kolom voor filter"),
        choices = names(uploaded_data()),
        selected = filter_spec()$col %||% input$column %||% NULL,
        options = shinyWidgets::pickerOptions(container = "body")
      )
    })

    output$filter_values_ui <- renderUI({
      req(uploaded_data())
      if (!is.null(input$filter_col)) {
        df_col <- uploaded_data()[[input$filter_col]]
      } else {
        df_col <- uploaded_data()[[1]]
      }

      counts <- table(na.omit(df_col))
      vals <- names(counts)
      labels <- paste0(vals, " (", counts, ")")
      choices <- setNames(vals, labels)

      tagList(
        tags$style(
          HTML(
            "
            /* -------------------- overall menu -------------------- */
            .bootstrap-select .dropdown-menu.show{
              max-width: 75vw !important;   /* you said 75 % earlier */
            }

            /* -------------------- each row ------------------------ */
            .bootstrap-select .dropdown-item{
              /* turn each <a> into a flex-row so we can
                 allocate space separately for label + extras      */
              display: flex;
              align-items: center;
              gap: .4rem;                   /* little breathing room */
            }

            /* main label: truncate after 80 % of the row ----------- */
            .bootstrap-select .dropdown-item .text{ /* span that holds label */
              flex: 0 1 80%;               /* ≤ 80 % of the row */
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
            }

            /* optional sub-text (generated by data-subtext) -------- */
            .bootstrap-select .dropdown-item small{
              flex: 0 1 auto;              /* take only what it needs */
              white-space: nowrap;
            }

            /* the built-in check-mark is absolutely positioned by
               Bootstrap-select, so no extra work is needed here.    */
            "
          )
        ),

        shinyWidgets::pickerInput(
          ns("filter_vals"),
          lang()$t("Kies waarden om te behouden"),
          choices = choices,
          selected = filter_spec()$vals %||% vals,
          multiple = TRUE,
          width = "100%",
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE,
            deselectAllText = lang()$t("Deselecteer alles"),
            selectAllText = lang()$t("Selecteer alles"),
            noneSelectedText = lang()$t("Niks geselecteerd")
          )
        )
      )
    })

    # Apply / clear filter ----------------------------------------------------
    observeEvent(input$apply_filter, {
      if (!length(input$filter_vals))
        return(showNotification(
          lang()$t("Selecteer minstens één waarde om te behouden."),
          type = "error"
        ))

      filter_spec(list(
        col = if (file_type() == "txt") "text" else input$filter_col,
        vals = input$filter_vals
      ))
      removeModal()
    })

    observeEvent(input$clear_filter, {
      filter_spec(NULL)
      removeModal()
    })

    # Refresh raw_texts when filter or column changes ------------------------
    observeEvent(filtered_data(), {
      df <- filtered_data()
      req(df)

      if (file_type() == "txt") {
        # single-column data.frame called “text”
        raw_texts(discard_empty(df[["text"]]))
      } else if (!is.null(input$column) && nzchar(input$column)) {
        raw_texts(discard_empty(df[[input$column]]))
      }
    })

    # ---- Disable inputs while processing -----------------------------------
    observe({
      if (processing()) {
        shinyjs::disable(ns("text_file"))
        shinyjs::disable(ns("sheet"))
        shinyjs::disable(ns("column"))
        shinyjs::disable(ns("filter_btn"))
      }
    })

    # ---- Reset fileInput on new session ------------------------------------
    shinyjs::reset("text_file")

    # ---- Return raw texts (character vector) -------------------------------
    return(raw_texts)
  })
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    text_upload_ui("text_upload_module")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE) # Simulate processing state

    raw_texts <- text_upload_server("text_upload_module", processing)

    observe({
      req(raw_texts())
      print(raw_texts()) # For debugging: print uploaded texts
    })
  }

  shinyApp(ui, server)
}
