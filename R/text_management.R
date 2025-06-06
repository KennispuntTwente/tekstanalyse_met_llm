# Module for pre-processing texts (anonymizing),
#   plus showing text table

#### 1 UI ####

text_management_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    bslib::card(
      class = "card",
      card_header(
        lang$t("Teksten")
      ),
      card_body(
        div(
          class = "text-center",
          actionButton(
            ns("open_text_table_modal"),
            label = paste0(lang$t("Bekijk tabel")),
            class = "btn btn-primary"
          ),
          br(),
          br(),
          uiOutput(ns("preprocess_counts"))
        )
      )
    )
  )
}


#### 2 Server ####

text_management_server <- function(
  id,
  raw_texts,
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      # Return reactive;
      #   raw_texts; preprocessed (unique), data.frame with raw and preprocessed texts
      texts <- reactiveValues(
        raw = NULL,
        preprocessed = NULL,
        df = NULL
      )

      # Pre-process texts, fill reactive values
      observeEvent(raw_texts(), {
        req(raw_texts())

        # Pre-process texts
        preprocessed_texts <- pre_process_texts(raw_texts())

        # Fill reactive values
        texts$raw <- raw_texts()
        texts$preprocessed <- unique(preprocessed_texts)
        texts$df <- data.frame(
          raw = raw_texts(),
          preprocessed = preprocessed_texts,
          stringsAsFactors = FALSE
        )
      })

      # Render anonymization count summary below the button
      output$preprocess_counts <- renderUI({
        req(texts$preprocessed)

        txts <- texts$preprocessed

        # Count how many times each placeholder appears
        email_count <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang$t("<< e-mailadres verwijderd >>"))
        ))
        # iban_count <- sum(stringr::str_count(
        #   txts,
        #   stringr::fixed("<< removed IBAN number >>")
        # ))
        phone_count <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang$t("<< (telefoon)nummer verwijderd >>"))
        ))
        postal_count <- sum(stringr::str_count(
          txts,
          stringr::fixed(lang$t("<< postcode verwijderd >>"))
        ))

        # Count removed duplicates
        total_texts <- length(texts$raw)
        unique_texts <- length(unique(texts$preprocessed))
        duplicate_count <- total_texts - unique_texts

        tagList(
          div(
            class = "mx-auto", # Center horizontally
            style = "max-width: 700px; width: 100%;", # Dynamically limit width
            # Put your entire content inside here
            tagList(
              # Box for Duplicates
              div(
                class = "border rounded p-2 mb-3 bg-light fade-in gap-2",
                div(
                  class = "text-muted small mb-1 text-center",
                  lang$t("Dubbele teksten verwijderd:")
                ),
                div(
                  class = "d-flex align-items-center justify-content-center gap-2",
                  bs_icon(
                    "trash",
                    class = "me-1",
                    `aria-hidden` = "true"
                  ),
                  span(class = "badge bg-secondary", duplicate_count)
                )
              ),

              # Box for Anonymization
              div(
                class = "border rounded p-2 bg-light fade-in",
                div(
                  class = "text-muted small mb-1 gap-2",
                  lang$t("Persoonsgegevens verwijderd:")
                ),
                div(
                  class = "small d-flex flex-wrap justify-content-center align-items-center gap-2",
                  div(
                    class = "d-flex align-items-center",
                    bs_icon("envelope", class = "me-1", aria_hidden = "true"),
                    span(class = "badge bg-secondary me-1", email_count),
                    span(class = "text-muted", lang$t("e‑mail(s)"))
                  ),
                  # div(
                  #   class = "d-flex align-items-center",
                  #   bs_icon("bank", class = "me-1", aria_hidden = "true"),
                  #   span(class = "badge bg-secondary me-1", iban_count),
                  #   span(class = "text-muted", "IBAN(s)")
                  # ),
                  div(
                    class = "d-flex align-items-center",
                    bs_icon("telephone", class = "me-1", aria_hidden = "true"),
                    span(class = "badge bg-secondary me-1", phone_count),
                    span(class = "text-muted", lang$t("nummer(s)"))
                  ),
                  div(
                    class = "d-flex align-items-center",
                    bs_icon("mailbox", class = "me-1", aria_hidden = "true"),
                    span(class = "badge bg-secondary me-1", postal_count),
                    span(class = "text-muted", lang$t("postcode(s)"))
                  )
                )
              )
            )
          )
        )
      })

      # Update modal button to show how many texts are available
      shinyjs::disable("open_text_table_modal")
      observeEvent(texts$preprocessed, {
        updateActionButton(
          session,
          "open_text_table_modal",
          label = paste0(
            lang$t("Bekijk tabel"),
            " (",
            length(texts$preprocessed),
            " ",
            lang$t("teksten"),
            ")"
          )
        )

        # Disable button when no texts are available
        if (length(texts$preprocessed) == 0) {
          shinyjs::disable("open_text_table_modal")
        } else {
          shinyjs::enable("open_text_table_modal")
        }
      })

      # Show modal with text table
      observeEvent(input$open_text_table_modal, {
        showModal(
          modalDialog(
            title = lang$t("Teksten"),
            DT::dataTableOutput(session$ns("text_table")),
            easyClose = TRUE,
            footer = modalButton(lang$t("Sluiten")),
            size = "l"
          )
        )
      })

      # Render text table
      output$text_table <- DT::renderDataTable(
        {
          data.frame(Tekst = texts$preprocessed)
        },
        options = list(pageLength = 5, scrollX = TRUE)
      )

      return(texts)
    }
  )
}


#### 3 Helper functions ####

# Function for pre-processing texts
pre_process_texts <- function(txts) {
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


#### 4 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page(
    useShinyjs(),
    css_js_head(),
    text_management_ui("text_management")
  )

  server <- function(input, output, session) {
    text_management_server(
      "text_management",
      reactive(c("a", "b", "0631377835", "b", "b"))
    )
  }

  shinyApp(ui, server)
}
