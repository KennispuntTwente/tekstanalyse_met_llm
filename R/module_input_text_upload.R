# Module to upload files with the texts
# Can handle .txt, .csv, .xlsx, and .sav files.
# Can select a sheet for Excel files, and a specific column for files with multiple columns
# Can filter rows based on column values through a modal dialog
# Note: pre-processing of texts is handled in the text_management module,
#   this module only uploads the raw data

# 1 UI ---------------------------------------------------------------
text_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "card-container",
      uiOutput(ns("card"))
    )
  )
}


# 2 Server ---------------------------------------------------------
text_upload_server <- function(
  id,
  processing,
  lang = default_lang()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$card <- renderUI({
      card(
        # ---- Card header -----------------------------------------------------
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center w-100",
            span(
              lang()$t("Upload teksten"),
              tooltip(
                bsicons::bs_icon("info-circle"),
                paste0(
                  lang()$t(
                    "Upload de teksten die je wilt analyseren. Je kunt een platte tekstbestand (.txt), een CSV-bestand (.csv), een Excel-bestand (.xlsx) of een SPSS-bestand (.sav) uploaden."
                  ),
                  lang()$t(
                    " Let op dat .txt-bestanden worden gesplitst in een tekst per nieuwe regel. Voor de andere bestanden kun je een specifieke kolom met teksten selecteren. Voor Excel-bestanden kun je ook een specifiek werkblad selecteren."
                  ),
                  lang()$t(
                    " Via het filter-icoon kun je de data filteren, voor wanneer je alleen een subset van de data wilt analyseren."
                  )
                )
              )
            ),
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

              # ---------- File input + checkbox -----------------------------------------
              div(
                class = "selector-container d-flex flex-column align-items-center",
                style = "max-width: 300px;",
                fileInput(
                  inputId = ns("text_file"),
                  label = lang()$t("Upload (.txt, .csv, .xlsx, of .sav)"),
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
              ),
              # ---------- By column selector (grouping variable) -------------
              div(
                id = ns("by_column_container"),
                class = "selector-container",
                style = "max-width: 300px; min-height: 100px;",
                uiOutput(ns("by_column_selector"))
              ),
              # ---- Text mode selector (for .txt files) -------------------
              uiOutput(ns("txt_mode_ui"))
            )
          )
        )
      )
    })

    observeEvent(
      input$txt_split_lines,
      {
        req(file_type() == "txt")
        req(input$txt_split_lines %in% c(lang()$t("Nee"), lang()$t("Ja")))
        log_action(
          "txt_split_lines_changed",
          details = sprintf("value=%s", input$txt_split_lines)
        )
      },
      ignoreInit = TRUE
    )
    # ---- Helpers ------------------------------------------------------------
    discard_empty <- function(x) {
      x <- x[!is.na(x)]
      keep <- stringr::str_trim(x) != ""
      unique(x[keep])
    }

    # ---- Reactive values ----------------------------------------------------
    raw_texts <- reactiveVal(NULL) # vector of texts returned by module
    uploaded_data <- reactiveVal(NULL) # raw data (data.frame) read from file
    sheet_names <- reactiveVal(NULL) # character vector of Excel sheet names
    by_column <- reactiveVal(NULL) # name of optional grouping column
    by_column_values <- reactiveVal(NULL) # values of by column aligned with texts
    filter_spec <- reactiveVal(NULL) # list(col = <chr>, vals = <chr>) | NULL
    file_type <- reactiveVal(NULL) # ◄ NEW: current file extension

    # Logical reactive: is a filter currently active?
    filter_active <- reactive({
      spec <- filter_spec()
      df <- uploaded_data()
      if (is.null(spec) || is.null(df)) {
        return(FALSE)
      }
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) {
        return(FALSE)
      }
      col_vals <- df[[col]]
      filtered <- col_vals %in% spec$vals
      any(filtered) && sum(filtered) < nrow(df)
    })

    # Data after optional filtering -----------------------------------------
    filtered_data <- reactive({
      df <- uploaded_data()
      spec <- filter_spec()
      if (is.null(df) || is.null(spec)) {
        return(df)
      }

      # Default to "text" column for txt files
      col <- spec$col %||% if (file_type() == "txt") "text" else NULL
      if (is.null(col) || !col %in% names(df)) {
        return(df)
      }

      df[df[[col]] %in% spec$vals, , drop = FALSE]
    })

    # ---- File upload --------------------------------------------------------
    observe({
      req(input$text_file)

      # Log file upload
      log_info(
        sprintf(
          "File uploaded: name=%s, type=%s",
          input$text_file$name,
          tools::file_ext(input$text_file$name)
        ),
        component = "upload"
      )

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
            # read every line first
            txt_lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)

            split_lines <- isTRUE(input$txt_split_lines == lang()$t("Ja"))

            txt <- if (split_lines) {
              discard_empty(stringr::str_trim(txt_lines))
            } else {
              paste(txt_lines, collapse = "\n") # combine to single text
            }

            df <- data.frame(text = txt, stringsAsFactors = FALSE)
            uploaded_data(df)
            raw_texts(df$text)
          },
          error = function(e) {
            log_error(
              sprintf(
                "Upload read error: file_type=txt, error=%s",
                conditionMessage(e)
              ),
              component = "upload"
            )
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
            log_error(
              sprintf(
                "Upload read error: file_type=%s, error=%s",
                file_ext,
                conditionMessage(e)
              ),
              component = "upload"
            )
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
            log_error(
              sprintf(
                "Upload read error: file_type=xlsx, error=%s",
                conditionMessage(e)
              ),
              component = "upload"
            )
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
            log_error(
              sprintf(
                "Upload read error: file_type=sav, error=%s",
                conditionMessage(e)
              ),
              component = "upload"
            )
            showNotification(
              paste(lang()$t("Error bij lezen van SAV-bestand:"), e$message),
              type = "error"
            )
          }
        )
      } else {
        log_warn(
          sprintf(
            "Unsupported upload file type: name=%s, file_type=%s",
            input$text_file$name,
            file_ext
          ),
          component = "upload"
        )
        showNotification(
          lang()$t("Niet ondersteund bestandstype"),
          type = "error"
        )
      }
    })

    # ---- Show / hide column selector depending on file type ---------------
    observe({
      if (is.null(file_type())) {
        return()
      }
      if (file_type() == "txt") {
        shinyjs::hide(ns("column_container"))
        shinyjs::hide(ns("by_column_container"))
      } else {
        shinyjs::show(ns("column_container"))
        shinyjs::show(ns("by_column_container"))
      }
    })

    # ---- Text mode selector (for .txt files) --------------------------------
    output$txt_mode_ui <- renderUI({
      req(file_type() == "txt")
      div(
        # Toggle for inter-rater reliability
        p(
          lang()$t("Splits tekst op nieuwe regels?"),
          class = "mb-2 text-center"
        ),
        div(
          class = "d-flex justify-content-center w-100", # add w-100
          shinyWidgets::radioGroupButtons(
            ns("txt_split_lines"),
            NULL,
            choices = c(lang()$t("Nee"), lang()$t("Ja")),
            selected = lang()$t("Ja"),
            size = "sm"
          )
        )
      )
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
          log_action("sheet_selected", details = input$sheet)
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
      if (file_type() == "txt") {
        return(NULL)
      }
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
        log_action("column_selected", details = col)
        txt <- filtered_data()[[col]]
        raw_texts(discard_empty(txt))
      }
    })

    # ---- By column selector (grouping variable) ----------------------------
    output$by_column_selector <- renderUI({
      req(filtered_data())
      if (file_type() == "txt") {
        return(NULL)
      }
      cols <- names(filtered_data())
      # Exclude the text column from available by columns
      text_col <- input$column
      available_cols <- setdiff(cols, text_col)
      if (length(available_cols) == 0) {
        return(NULL)
      }

      tagList(
        selectInput(
          ns("by_column"),
          tagList(
            lang()$t("Selecteer groepsvariabele (optioneel)"),
            tooltip(
              bsicons::bs_icon("info-circle"),
              lang()$t(
                "Selecteer optioneel een kolom om de resultaten op te splitsen per groep. In het rapport worden dan frequenties/statistieken per groep getoond naast de totalen."
              )
            )
          ),
          choices = stats::setNames(
            c("", available_cols),
            c("", available_cols)
          ),
          selected = by_column() %||% ""
        )
      )
    })

    observeEvent(input$by_column, {
      col <- input$by_column
      if (is.null(col) || !nzchar(col)) {
        by_column(NULL)
        by_column_values(NULL)
        log_action("by_column_cleared")
      } else {
        by_column(col)
        log_action("by_column_selected", details = col)
      }
    })

    # Update by_column_values when filtered_data, column, or by_column changes
    observe({
      req(filtered_data())
      text_col <- input$column
      by_col <- by_column()

      if (is.null(text_col) || !nzchar(text_col)) {
        by_column_values(NULL)
        return()
      }

      if (is.null(by_col) || !nzchar(by_col)) {
        by_column_values(NULL)
        return()
      }

      if (!by_col %in% names(filtered_data())) {
        by_column_values(NULL)
        return()
      }

      # Get the by column values aligned with the text column
      df <- filtered_data()
      text_vals <- df[[text_col]]
      by_vals <- df[[by_col]]

      # Only keep rows that have non-empty text (matching discard_empty logic)
      keep <- !is.na(text_vals) & stringr::str_trim(text_vals) != ""
      by_column_values(by_vals[keep])
    })

    # ---- Filter icon (dynamic colour) --------------------------------------
    output$filter_icon <- renderUI({
      modal_trigger_icon(
        ns = ns,
        input_id = "filter_btn",
        icon_name = "filter",
        tooltip_text = lang()$t("Filter data"),
        is_active = filter_active()
      )
    })

    # ---- Filter modal -------------------------------------------------------
    observeEvent(input$filter_btn, {
      req(uploaded_data())
      req(!isTRUE(processing()))

      showModal(modalDialog(
        title = lang()$t("Filter data"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,

        bslib::page(
          tags$div(
            style = "display:none;",
            `data-kwallm-modal-id` = "filter_modal",
            `data-kwallm-modal-details` = "module=text_upload"
          ),
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

          ## Footer buttons
          modal_footer_buttons(
            left = modalButton(lang()$t("Sluiten")),
            center = actionButton(
              ns("clear_filter"),
              label = tagList(icon("rotate-left"), lang()$t("Filter wissen")),
              class = "btn btn-warning"
            ),
            right = actionButton(
              ns("apply_filter"),
              label = tagList(icon("filter"), lang()$t("Toepassen")),
              class = "btn btn-primary"
            )
          )
        )
      ))
    })

    # Dynamic values selector -------------------------------------------------
    output$filter_col_selector <- renderUI({
      req(uploaded_data())
      if (file_type() == "txt") {
        return(NULL)
      } # Hide if plain text file

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
      if (!length(input$filter_vals)) {
        return(showNotification(
          lang()$t("Selecteer minstens één waarde om te behouden."),
          type = "error"
        ))
      }

      filter_spec(list(
        col = if (file_type() == "txt") "text" else input$filter_col,
        vals = input$filter_vals
      ))
      log_action(
        "filter_applied",
        details = sprintf(
          "col=%s, n_vals=%d",
          if (file_type() == "txt") "text" else input$filter_col,
          length(input$filter_vals)
        )
      )
      removeModal()
    })

    observeEvent(input$clear_filter, {
      filter_spec(NULL)
      log_action("filter_cleared")
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
    disable_when_processing(
      processing,
      c(
        "text_file",
        "sheet",
        "column",
        "by_column",
        "filter_btn",
        "txt_split_lines"
      )
    )

    # ---- Reset fileInput on new session ------------------------------------
    shinyjs::reset("text_file")

    # ---- Return raw texts and by_column info -------------------------------
    # Return a list with raw_texts and by_column information
    return(list(
      texts = raw_texts,
      by_column_name = by_column,
      by_column_values = by_column_values
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
