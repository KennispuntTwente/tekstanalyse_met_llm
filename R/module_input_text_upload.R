# Module to upload files with the texts
# Can handle .txt, .csv, .xlsx, and .sav files.
# Can select a sheet for Excel files, and a specific column for files with multiple columns
# Can filter rows based on column values through a modal dialog
# Note: pre-processing of texts is handled in the text_management module,
#   this module establishes the source-document and current-document rows.

.kwallm_raw_starts_with <- function(raw, prefix) {
  stopifnot(is.raw(raw), is.numeric(prefix))

  length(raw) >= length(prefix) &&
    identical(
      as.integer(raw[seq_along(prefix)]),
      as.integer(prefix)
    )
}


.kwallm_strip_utf_bom <- function(text) {
  sub("^\ufeff", "", text)
}


.kwallm_read_text_with_encoding <- function(path, encoding) {
  con <- file(path, open = "r", encoding = encoding)
  on.exit(close(con), add = TRUE)

  paste(readLines(con, warn = FALSE, skipNul = TRUE), collapse = "\n")
}


.kwallm_decode_txt_file <- function(path) {
  stopifnot(is.character(path), length(path) == 1, nzchar(path))

  raw <- readBin(path, "raw", file.info(path)$size)

  if (.kwallm_raw_starts_with(raw, c(0xEF, 0xBB, 0xBF))) {
    decoded <- rawToChar(raw[-seq_len(3)])
    Encoding(decoded) <- "UTF-8"
    return(.kwallm_strip_utf_bom(decoded))
  }

  if (.kwallm_raw_starts_with(raw, c(0xFF, 0xFE))) {
    decoded <- .kwallm_read_text_with_encoding(path, "UTF-16LE")
    return(.kwallm_strip_utf_bom(decoded))
  }

  if (.kwallm_raw_starts_with(raw, c(0xFE, 0xFF))) {
    decoded <- .kwallm_read_text_with_encoding(path, "UTF-16BE")
    return(.kwallm_strip_utf_bom(decoded))
  }

  txt_content <- tryCatch(
    {
      decoded <- rawToChar(raw)
      if (!validUTF8(decoded)) {
        stop("not valid utf-8")
      }
      Encoding(decoded) <- "UTF-8"
      decoded
    },
    error = function(e) {
      iconv(rawToChar(raw), from = "", to = "UTF-8", sub = "")
    }
  )

  .kwallm_strip_utf_bom(txt_content)
}

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
    # Stable id for the uploaded source row, even when text values repeat.
    source_id_col <- ".kwallm_source_document_id"

    # ---- Helpers ------------------------------------------------------------
    discard_empty <- function(x) {
      x <- as.character(x)
      x <- x[!is.na(x)]
      keep <- stringr::str_trim(x) != ""
      x[keep]
    }

    ensure_source_document_id <- function(df) {
      stopifnot(is.data.frame(df))

      if (!source_id_col %in% names(df)) {
        df[[source_id_col]] <- seq_len(nrow(df))
      }

      df
    }

    visible_uploaded_columns <- function(df) {
      if (is.null(df)) {
        return(character())
      }

      setdiff(names(df), source_id_col)
    }

    build_unsplit_rows <- function(df, text_col) {
      if (
        is.null(df) ||
          is.null(text_col) ||
          !nzchar(text_col) ||
          !text_col %in% names(df)
      ) {
        return(NULL)
      }

      text_vals <- as.character(df[[text_col]])
      keep <- !is.na(text_vals) & stringr::str_trim(text_vals) != ""

      if (!any(keep)) {
        return(data.frame(
          source_document_id = integer(),
          document_id = integer(),
          source_document_text = character(),
          document_text = character(),
          stringsAsFactors = FALSE
        ))
      }

      source_ids <- as.integer(df[[source_id_col]][keep])
      document_text_vals <- text_vals[keep]

      # source_document_* describes the uploaded row.
      # document_* describes the current row passed to later modules.
      # Before splitting, those two layers are still the same row.
      data.frame(
        source_document_id = source_ids,
        document_id = source_ids,
        source_document_text = document_text_vals,
        document_text = document_text_vals,
        stringsAsFactors = FALSE
      )
    }

    normalize_upload_info <- function(file_df) {
      stopifnot(!is.null(file_df), nrow(file_df) >= 1)

      file_df <- file_df[1, , drop = FALSE]
      file_name <- as.character(file_df$name[[1]])
      list(
        name = file_name,
        size = as.numeric(file_df$size[[1]] %||% 0),
        type = as.character(file_df$type[[1]] %||% ""),
        datapath = as.character(file_df$datapath[[1]]),
        ext = tolower(tools::file_ext(file_name))
      )
    }

    read_txt_file <- function(file_info, split_lines) {
      txt_content <- .kwallm_decode_txt_file(file_info$datapath)
      txt_lines <- strsplit(txt_content, "\r?\n")[[1]]

      txt <- if (isTRUE(split_lines)) {
        discard_empty(stringr::str_trim(txt_lines))
      } else {
        paste(txt_lines, collapse = "\n")
      }

      ensure_source_document_id(data.frame(
        text = txt,
        stringsAsFactors = FALSE
      ))
    }

    reset_file_input <- function(file_name = NULL) {
      shinyjs::delay(50, shinyjs::reset("text_file"))
      shinyjs::delay(
        80,
        shinyjs::runjs(sprintf(
          paste0(
            "(function(){",
            "var el=document.getElementById(%s);",
            "if(!el){return;}",
            "var group=el.closest('.input-group');",
            "if(!group){return;}",
            "var text=group.querySelector('input.form-control[readonly]');",
            "if(!text){return;}",
            "text.value=%s;",
            "})();"
          ),
          jsonlite::toJSON(ns("text_file"), auto_unbox = TRUE),
          jsonlite::toJSON(file_name %||% "", auto_unbox = TRUE)
        ))
      )
    }

    clear_by_column_data <- function() {
      by_column_values(NULL)
      by_column_lookup(NULL)
    }

    clear_upload_state <- function() {
      document_texts(NULL)
      text_rows(NULL)
      uploaded_data(NULL)
      sheet_names(NULL)
      filter_spec(NULL)
      clear_by_column_data()
      by_column(NULL)
      selected_sheet(NULL)
      selected_column(NULL)
      uploaded_file_info(NULL)
      file_type(NULL)
    }

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
              uiOutput(ns("column_group_controls")),
              # ---- Text mode selector (for .txt files) -------------------
              uiOutput(ns("txt_mode_ui"))
            )
          ),
          div(
            class = "d-flex justify-content-center mt-3",
            uiOutput(ns("file_status"))
          ),
          tags$script(HTML(sprintf(
            paste0(
              "(function(){",
              "var inputId=%s;",
              "setTimeout(function(){",
              "var el=document.getElementById(inputId);",
              "if(!el || el.dataset.kwallmResetOnClick==='1'){return;}",
              "el.addEventListener('click', function(){ this.value=''; });",
              "el.dataset.kwallmResetOnClick='1';",
              "}, 0);",
              "})();"
            ),
            jsonlite::toJSON(ns("text_file"), auto_unbox = TRUE)
          )))
        )
      )
    })

    # ---- Reactive values ----------------------------------------------------
    document_texts <- reactiveVal(NULL) # current document texts before preprocessing
    text_rows <- reactiveVal(NULL) # row-level source/document lineage for those texts
    uploaded_data <- reactiveVal(NULL) # raw data (data.frame) read from file
    sheet_names <- reactiveVal(NULL) # character vector of Excel sheet names
    by_column <- reactiveVal(NULL) # name of optional grouping column
    by_column_values <- reactiveVal(NULL) # values of by column aligned with texts
    by_column_lookup <- reactiveVal(NULL) # duplicate-preserving lookup for reports
    filter_spec <- reactiveVal(NULL) # list(col = <chr>, vals = <chr>) | NULL
    file_type <- reactiveVal(NULL) # ◄ NEW: current file extension
    uploaded_file_info <- reactiveVal(NULL)
    selected_sheet <- reactiveVal(NULL)
    selected_column <- reactiveVal(NULL)
    txt_split_lines_choice <- reactiveVal(TRUE)

    current_txt_split_lines <- reactive({
      if (
        !is.null(input$txt_split_lines) &&
          input$txt_split_lines %in% c(lang()$t("Nee"), lang()$t("Ja"))
      ) {
        identical(input$txt_split_lines, lang()$t("Ja"))
      } else {
        txt_split_lines_choice()
      }
    })

    current_sheet <- reactive({
      input$sheet %||% selected_sheet()
    })

    current_column <- reactive({
      input$column %||% selected_column()
    })

    current_by_column <- reactive({
      if (is.null(input$by_column)) {
        by_column()
      } else if (!nzchar(input$by_column)) {
        NULL
      } else {
        input$by_column
      }
    })

    shiny::exportTestValues(
      uploaded_file_name = uploaded_file_info()$name %||% NULL,
      file_type = file_type(),
      selected_sheet = selected_sheet(),
      selected_column = selected_column(),
      txt_split_lines = txt_split_lines_choice(),
      by_column = by_column()
    )

    output$file_status <- renderUI({
      current_file <- uploaded_file_info()
      if (is.null(current_file)) {
        return(NULL)
      }

      div(
        class = "small text-muted text-center",
        style = "max-width: 100%; word-break: break-word;",
        div(
          class = "d-inline-flex align-items-center gap-2 px-3 py-2 rounded bg-light",
          tags$span(icon("file"), " "),
          tags$strong(current_file$name)
        )
      )
    })

    output$column_group_controls <- renderUI({
      if (identical(file_type(), "txt")) {
        return(NULL)
      }

      tagList(
        div(
          id = ns("column_container"),
          class = "selector-container",
          style = "max-width: 300px; min-height: 100px;",
          uiOutput(ns("column_selector"))
        ),
        div(
          id = ns("by_column_container"),
          class = "selector-container",
          style = "max-width: 300px; min-height: 100px;",
          uiOutput(ns("by_column_selector"))
        )
      )
    })

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

    refresh_by_column_values <- function() {
      df <- filtered_data()
      text_col <- current_column()
      by_col <- current_by_column()

      if (
        is.null(df) ||
          is.null(text_col) ||
          !nzchar(text_col) ||
          is.null(by_col) ||
          !nzchar(by_col) ||
          !by_col %in% names(df)
      ) {
        clear_by_column_data()
        return(invisible(NULL))
      }

      text_vals <- df[[text_col]]
      by_vals <- df[[by_col]]
      keep <- !is.na(text_vals) & stringr::str_trim(text_vals) != ""
      text_keep <- text_vals[keep]
      by_keep <- by_vals[keep]
      source_ids <- as.integer(df[[source_id_col]][keep])

      by_column_lookup(data.frame(
        source_document_id = source_ids,
        text = text_keep,
        by_value = by_keep,
        stringsAsFactors = FALSE
      ))

      by_column_values(by_keep)
      invisible(NULL)
    }

    refresh_text_rows <- function() {
      df <- filtered_data()
      if (is.null(df)) {
        text_rows(NULL)
        document_texts(NULL)
        return(invisible(NULL))
      }

      text_col <- if (identical(file_type(), "txt")) {
        "text"
      } else {
        current_column()
      }

      rows <- build_unsplit_rows(df, text_col)
      text_rows(rows)
      if (is.null(rows)) {
        document_texts(NULL)
      } else {
        document_texts(rows$document_text)
      }
      invisible(NULL)
    }

    observeEvent(
      input$txt_split_lines,
      {
        req(input$txt_split_lines %in% c(lang()$t("Nee"), lang()$t("Ja")))
        split_lines <- identical(input$txt_split_lines, lang()$t("Ja"))

        if (!identical(txt_split_lines_choice(), split_lines)) {
          txt_split_lines_choice(split_lines)
          log_action(
            "txt_split_lines_changed",
            details = sprintf("value=%s", input$txt_split_lines)
          )
        }
      },
      ignoreInit = TRUE
    )

    # ---- File upload --------------------------------------------------------
    observe({
      req(input$text_file)

      file_info <- normalize_upload_info(input$text_file)
      previous_column <- isolate(selected_column())
      previous_by_column <- isolate(by_column())
      previous_sheet <- isolate(selected_sheet())

      # Log file upload
      log_info(
        sprintf(
          "File uploaded: name=%s, type=%s",
          file_info$name,
          file_info$ext
        ),
        component = "upload"
      )

      # Reset all state -------------------------------------------------------
      document_texts(NULL)
      uploaded_data(NULL)
      sheet_names(NULL)
      filter_spec(NULL)
      clear_by_column_data()
      by_column(NULL)
      selected_column(NULL)

      uploaded_file_info(file_info)
      file_type(file_info$ext)

      if (identical(file_info$ext, "txt")) {
        selected_sheet(NULL)
        tryCatch(
          {
            df <- read_txt_file(file_info, current_txt_split_lines())
            uploaded_data(df)
            text_rows(build_unsplit_rows(df, "text"))
            document_texts(text_rows()$document_text)
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
            clear_upload_state()
          }
        )
        reset_file_input(uploaded_file_info()$name %||% NULL)
      } else if (file_info$ext %in% c("csv", "tsv")) {
        selected_sheet(NULL)
        tryCatch(
          {
            df <- vroom::vroom(file_info$datapath)
            uploaded_data(ensure_source_document_id(df))

            if (!is.null(previous_column) && previous_column %in% names(df)) {
              selected_column(previous_column)
            }
            if (
              !is.null(previous_by_column) &&
                previous_by_column %in% names(df) &&
                !identical(previous_by_column, selected_column())
            ) {
              by_column(previous_by_column)
            }
          },
          error = function(e) {
            log_error(
              sprintf(
                "Upload read error: file_type=%s, error=%s",
                file_info$ext,
                conditionMessage(e)
              ),
              component = "upload"
            )
            showNotification(
              paste(
                lang()$t("Error bij lezen van CSV/TSV-bestand:"),
                e$message
              ),
              type = "error"
            )
            clear_upload_state()
          }
        )
        reset_file_input(uploaded_file_info()$name %||% NULL)
      } else if (file_info$ext == "xlsx") {
        tryCatch(
          {
            sheets <- readxl::excel_sheets(file_info$datapath)
            sheet_names(sheets)

            desired_sheet <- previous_sheet %||% sheets[1]
            if (!desired_sheet %in% sheets) {
              desired_sheet <- sheets[1]
            }
            selected_sheet(desired_sheet)
            uploaded_data(
              ensure_source_document_id(
                readxl::read_excel(file_info$datapath, sheet = desired_sheet)
              )
            )

            if (!is.null(previous_column)) {
              selected_column(previous_column)
            }
            if (!is.null(previous_by_column)) {
              by_column(previous_by_column)
            }
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
            clear_upload_state()
          }
        )
        reset_file_input(uploaded_file_info()$name %||% NULL)
      } else if (file_info$ext == "sav") {
        selected_sheet(NULL)
        tryCatch(
          {
            df <- haven::read_sav(file_info$datapath)
            uploaded_data(ensure_source_document_id(df))

            if (!is.null(previous_column) && previous_column %in% names(df)) {
              selected_column(previous_column)
            }
            if (
              !is.null(previous_by_column) &&
                previous_by_column %in% names(df) &&
                !identical(previous_by_column, selected_column())
            ) {
              by_column(previous_by_column)
            }
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
            clear_upload_state()
          }
        )
        reset_file_input(uploaded_file_info()$name %||% NULL)
      } else {
        log_warn(
          sprintf(
            "Unsupported upload file type: name=%s, file_type=%s",
            file_info$name,
            file_info$ext
          ),
          component = "upload"
        )
        showNotification(
          lang()$t("Niet ondersteund bestandstype"),
          type = "error"
        )
        clear_upload_state()
        reset_file_input()
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
            selected = if (isTRUE(txt_split_lines_choice())) {
              lang()$t("Ja")
            } else {
              lang()$t("Nee")
            },
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
        selected = current_sheet() %||% sheet_names()[1]
      )
    })

    observeEvent(
      input$sheet,
      {
        req(input$sheet)
        selected_sheet(input$sheet)
        log_action("sheet_selected", details = input$sheet)
      },
      ignoreInit = TRUE
    )

    observeEvent(selected_sheet(), {
      file_info <- uploaded_file_info()
      req(
        !is.null(file_info),
        identical(file_info$ext, "xlsx"),
        selected_sheet()
      )
      tryCatch(
        {
          df <- readxl::read_excel(file_info$datapath, sheet = selected_sheet())
          uploaded_data(ensure_source_document_id(df))
          filter_spec(NULL)
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
      cols <- visible_uploaded_columns(filtered_data())
      # if (length(cols) <= 1) return(NULL)
      selectInput(
        ns("column"),
        lang()$t("Selecteer kolom met teksten"),
        choices = cols,
        selected = current_column()
      )
    })

    observeEvent(
      input$column,
      {
        req(filtered_data())
        col <- input$column
        if (!is.null(col) && nzchar(col)) {
          selected_column(col)
          refresh_text_rows()
          refresh_by_column_values()
          log_action("column_selected", details = col)
        } else {
          selected_column(NULL)
          text_rows(NULL)
          document_texts(NULL)
        }
      },
      ignoreInit = TRUE
    )

    # ---- By column selector (grouping variable) ----------------------------
    output$by_column_selector <- renderUI({
      req(filtered_data())
      if (file_type() == "txt") {
        return(NULL)
      }
      cols <- names(filtered_data())
      cols <- visible_uploaded_columns(filtered_data())
      # Exclude the text column from available by columns
      text_col <- current_column()
      available_cols <- setdiff(cols, text_col)
      if (length(available_cols) == 0) {
        return(NULL)
      }

      tagList(
        selectInput(
          ns("by_column"),
          tagList(
            lang()$t("Selecteer groepsvariabele"),
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
        clear_by_column_data()
        log_action("by_column_cleared")
      } else {
        by_column(col)
        refresh_by_column_values()
        log_action("by_column_selected", details = col)
      }
    })

    # Update by-column metadata when filtered_data, column, or by_column changes
    observe({
      req(filtered_data())
      text_col <- current_column()
      by_col <- current_by_column()

      if (is.null(text_col) || !nzchar(text_col)) {
        clear_by_column_data()
        return()
      }

      if (is.null(by_col) || !nzchar(by_col)) {
        clear_by_column_data()
        return()
      }

      if (!by_col %in% names(filtered_data())) {
        clear_by_column_data()
        return()
      }

      # Keep both the dedup-aligned vector and the report lookup in sync.
      refresh_by_column_values()
    })

    observe({
      file_info <- uploaded_file_info()

      if (is.null(file_info) || is.null(uploaded_data())) {
        return()
      }

      if (identical(file_info$ext, "txt")) {
        if (!identical(selected_column(), "text")) {
          selected_column("text")
        }
        if (!is.null(by_column())) {
          by_column(NULL)
        }
        return()
      }

      cols <- visible_uploaded_columns(uploaded_data())
      current_column <- selected_column()
      if (is.null(current_column) || !current_column %in% cols) {
        selected_column(NULL)
      }

      current_by_column <- by_column()
      available_by_columns <- setdiff(cols, selected_column())
      if (
        !is.null(current_by_column) &&
          (!current_by_column %in% available_by_columns)
      ) {
        by_column(NULL)
      }
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
        choices = visible_uploaded_columns(uploaded_data()),
        selected = filter_spec()$col %||% current_column() %||% NULL,
        options = shinyWidgets::pickerOptions(container = "body")
      )
    })

    output$filter_values_ui <- renderUI({
      req(uploaded_data())
      if (!is.null(input$filter_col)) {
        df_col <- uploaded_data()[[input$filter_col]]
      } else {
        visible_cols <- visible_uploaded_columns(uploaded_data())
        req(length(visible_cols) > 0)
        df_col <- uploaded_data()[[visible_cols[[1]]]]
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

    # Refresh current document texts when filter or column changes ------------
    observe({
      df <- filtered_data()
      req(df)

      refresh_text_rows()
    })

    # Re-read persisted txt upload when split mode changes after upload -------
    observeEvent(
      txt_split_lines_choice(),
      {
        file_info <- uploaded_file_info()
        req(!is.null(file_info), identical(file_info$ext, "txt"))

        tryCatch(
          {
            df <- read_txt_file(file_info, current_txt_split_lines())
            uploaded_data(df)
            text_rows(build_unsplit_rows(df, "text"))
            document_texts(text_rows()$document_text)
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
      },
      ignoreInit = TRUE
    )

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

    upload_info <- reactive({
      spec <- filter_spec()
      filter_df <- if (is.null(spec)) {
        NULL
      } else {
        data.frame(
          column = rep(
            spec$col %||% NA_character_,
            length(spec$vals %||% character())
          ),
          value = as.character(spec$vals %||% character()),
          stringsAsFactors = FALSE
        )
      }

      list(
        file_type = file_type(),
        selected_sheet = current_sheet(),
        text_column = current_column(),
        grouping_column = current_by_column(),
        filter_spec = filter_df,
        txt_split_lines = txt_split_lines_choice()
      )
    })

    # ---- Return current document texts and by_column info ------------------
    # Return a list with current document texts and grouping metadata.
    return(list(
      texts = document_texts,
      text_rows = text_rows,
      by_column_name = by_column,
      by_column_values = by_column_values,
      by_column_lookup = by_column_lookup,
      upload_info = upload_info
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

    upload_result <- text_upload_server("text_upload_module", processing)

    observe({
      req(upload_result$texts())
      print(upload_result$texts())
    })
  }

  shinyApp(ui, server)
}
