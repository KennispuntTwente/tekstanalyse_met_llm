# 1 Server -------------------------------------------------------------
# Shiny module for text anonymization using GLiNER model
#   Used to anonymize personally identifiable information (PII) in texts,
#     before sending it to an external (LLM) API for processing
#   Steps:
#     1: takes raw texts as input;
#     2: opens modal, where user can define labels for entities to remove
#       (e.g., person, phone number, e-mail address, etc.)
#     3: 'start' button to do the anonymizing, using the GLiNER model
#     4: user can review the PII that were removed,
#       and choose to undo specific removals
#     5: 'save' button where user finishes the anonymization process
#       preprocessed texts are returned for use in the other modules

#' GLiNER-based anonymization modal.
#'
#' @param pii_texts Reactive returning a character vector of texts to anonymize.
#' @param gliner_model Optional pre-loaded model object; leave NULL to load in async.
gliner_server <- function(
  id,
  pii_texts,
  lang = default_lang(),
  # If gliner model is NULL, the gliner model will be
  #   loaded in the async process. This takes some exra time but is required
  #   for true async, as the reticulate object cannot be passed to an
  #   async process
  # If async is not important (i.e., running with having planned async),
  #   you can pass the model directly here; that means model does not
  #   have to be (re)loaded upon start of anonymization, and things
  #   will go faster (initial loading can then be done once, before user clicks button)
  gliner_model = NULL
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # 1 Manage state -----------------------------------------------
      # To start/initialize the module from the main server:
      start <- function() {
        if (
          !getOption("anonymization__gliner_model", FALSE) ||
            length(pii_texts()) == 0
        ) {
          return$enabled <- FALSE
          return()
        }

        # Close any existing modal
        shiny::removeModal()

        showModal(modalDialog(
          title = lang()$t("Anonimiseer met GLiNER-model"),
          size = "xl",
          easyClose = FALSE,
          footer = NULL,
          tagList(
            tags$div(
              style = "display:none;",
              `data-kwallm-modal-id` = "gliner_modal",
              `data-kwallm-modal-details` = sprintf(
                "module_id=%s, n_texts=%d",
                id,
                length(pii_texts() %||% character(0))
              )
            ),
            uiOutput(ns("modal_content"))
          )
        ))

        pii_entities_ui_rerender(Sys.time())

        return$enabled <- TRUE
      }

      # Queue object to talk to the main process when loading model from async
      queue <- shinyQueue()

      # Make available to the main server:
      #   start function; done status; result (will be the anonymized texts)
      return <- reactiveValues(
        start = start,
        enabled = FALSE,
        done = FALSE,
        anonymized_texts = NULL,
        number_of_pii_entities_removed = NULL
      )

      # State can be:
      #   "defining" (user defines labels of PII entities, e.g., 'name')
      #   "running" (model is running, processing the texts)
      #   "error" (error occurred during processing)
      #   "evaluating" (user evaluates the PII entities that were removed)
      #   "finished" (user finished the anonymization process)
      module_state <- reactiveVal("defining")

      # 2 Modal UI ---------------------------------------------------
      output$modal_content <- renderUI({
        switch(
          module_state(),
          # User defines the PII entities the model should remove
          defining = {
            tagList(
              p(
                lang()$t(
                  "Voer PII-entiteiten in die je zou willen verwijderen."
                ),
                lang()$t(
                  "Bijvoorbeeld: naam, adres, werkgever, telefoonnummer, e-mail, et cetera."
                ),
                br(),
                lang()$t(
                  "Het lokale GLiNER-model zal deze entities proberen te flaggen in de teksten."
                )
              ),
              hr(),
              textAreaInput(
                ns("pii_labels"),
                width = "100%",
                label = lang()$t("Entiteiten (gescheiden door komma's):"),
                value = "
                  name of person,
                  date of birth,
                  employer,
                  address,
                  postal code,
                  place of residence,
                  phone number,
                  mobile phone number,
                  landline phone number,
                  Dutch (mobile) phone number,
                  registration number,
                  serial number,
                  email,
                  personal identification number,
                  identity card number,
                  passport number,
                  paspport expiration date,
                  bank account number,
                  license plate code,
                  personal religious orientation,
                  personal political orientation,
                  sexual orientation,
                  ip address,
                  username,
                  social media handle,
                  digital signature,
                  iban,
                  bic,
                  student id number
                " |>
                  stringr::str_squish(),
                rows = 4
              ),
              hr(),
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                actionButton(
                  ns("quit"),
                  label = tagList(
                    icon("sign-out-alt"),
                    lang()$t("Sluit")
                  ),
                  class = "btn btn-danger"
                ),
                actionButton(
                  ns("start_anonymization"),
                  label = tagList(
                    icon("play"),
                    lang()$t("Start")
                  ),
                  class = "btn btn-success"
                )
              )
            )
          },

          # Model is running, processing the texts;
          #   show loading spinner
          running = {
            tagList(
              p(
                lang()$t(
                  "Het model wordt geladen en de teksten worden geanalyseerd. Even geduld..."
                ),
                br(),
                lang()$t(
                  "De allereerste keer dat deze machine dit doet, kan het wat langer duren omdat het model moet worden gedownload."
                ),
                br(),
                lang()$t(
                  "Snelheid hangt af van de hardware die gebruikt wordt. Mogelijk is configuratie nodig om de hardware optimaal te benutten."
                )
              ),
              hr(),
              tags$div(
                class = "text-center",
                style = "margin-top: 20px;",
                icon("spinner", class = "fa-spin fa-3x")
              ),
              uiOutput(ns("gliner_load_message_ui")),
              hr()
            )
          },

          # Error occurred during processing
          error = {
            tagList(
              p(lang()$t(
                "Er is een fout opgetreden tijdens het anonimiseren. Probeer het opnieuw."
              )),
              hr(),
              # Quit & retry buttons
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                actionButton(
                  ns("quit"),
                  label = tagList(
                    icon("sign-out-alt"),
                    lang()$t("Sluit")
                  ),
                  class = "btn btn-danger"
                ),
                actionButton(
                  ns("retry"),
                  label = tagList(
                    icon("redo"),
                    lang()$t("Reset")
                  ),
                  class = "btn btn-primary"
                )
              )
            )
          },

          # User evaluates the PII entities that were removed by the model
          evaluating = {
            tagList(
              tags$style(HTML(
                "
                /* Make DT cells wrap so they don’t force a wide table */
                .pii-entities-table-container table.dataTable td {
                  white-space: normal !important;
                  word-wrap: break-word;
                }

                /* Stop DT from adding its own horizontal scroller */
                .pii-entities-table-container .dataTables_wrapper {
                  overflow-x: hidden;
                }
              "
              )),
              uiOutput(ns("pii_entities_ui")),
              hr(),
              # Quit button left & save button right
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                actionButton(
                  ns("quit"),
                  label = tagList(
                    icon("sign-out-alt"),
                    lang()$t("Sluit")
                  ),
                  class = "btn btn-danger"
                ),
                actionButton(
                  ns("retry"),
                  label = tagList(
                    icon("redo"),
                    lang()$t("Reset")
                  ),
                  class = "btn btn-primary"
                ),
                actionButton(
                  ns("save_anonymization"),
                  label = tagList(
                    icon("save"),
                    lang()$t("Sla op")
                  ),
                  class = "btn btn-success"
                )
              )
            )
          },

          # User finished the anonymization process
          finished = {
            tagList(
              h3(lang()$t("Anonymisatie voltooid!")),
              p(lang()$t("De teksten zijn geanonimiseerd en opgeslagen.")),
              actionButton(ns("close_modal"), lang()$t("Sluiten"))
            )
          }
        )
      })

      # 3 Process handlers -------------------------------------------
      ## 3.1 Run model to detect PII entities --------------------------
      # Reactive value to store the PII entities predictions
      pii_predictions <- reactiveVal(NULL)

      # User clicks the start anonymization button
      observeEvent(
        input$start_anonymization,
        {
          req(input$pii_labels)
          req(isTRUE(return$enabled))

          ## 1 Parse & validate labels
          labels <- strsplit(input$pii_labels, ",")[[1]] |> trimws()
          if (length(labels) < 2 || all(labels == "")) {
            # Need at least 2 labels, otherwise model won't function
            # Not sure why 1 label is not enough, but it isn't;
            #   bug (feature?) in the GLiNER package/model?
            shiny::showNotification(
              lang()$t(
                "Voer ten minste twee (2) entities in om te verwijderen."
              ),
              type = "error"
            )
            return()
          }
          if (!is.vector(labels)) {
            labels <- c(labels)
          }
          log_debug(
            sprintf("GLiNER labels: %s", paste(labels, collapse = ", ")),
            component = "gliner"
          )

          log_action(
            "gliner_start_clicked",
            details = sprintf(
              "n_texts=%d n_labels=%d",
              length(pii_texts() %||% character(0)),
              length(labels)
            )
          )

          ## 2 Switch the modal to the “running” state
          module_state("running")

          # Log GLiNER start with configured labels
          log_info(
            sprintf(
              "GLiNER detection started: n_texts=%d, n_labels=%d",
              length(pii_texts()),
              length(labels)
            ),
            component = "gliner"
          )

          ## 3 Build a progress bar that the worker can update
          n_txt <- length(pii_texts())
          progress <- AsyncProgress$new(
            message = lang()$t("Detectie van entiteiten…"),
            detail = sprintf(lang()$t("0 van %d teksten"), n_txt)
          )

          ## 3.1 Start queue to update about model loading
          queue$consumer$start(millis = 250)

          session_id <- get_session_id()
          log_context <- log_context_capture(
            session_id = session_id,
            is_async = TRUE
          )

          ## 4 Spawn the mirai worker that runs GLiNER model on texts
          mirai::mirai(
            {
              kwallm_worker_bootstrap(
                task = "gliner",
                app_root = app_root,
                worker_options = worker_options,
                log_context = log_context
              )

              if (is.null(gliner_model)) {
                # If we are truly in async mode, we load the model here;
                #   because reticulate objects cannot be passed to async processes
                #   (in that case, ensure `gliner_model` is NULL when passing it on in mirai .args)
                # If not in async, then the gliner_model may already be loaded
                gliner_model <- gliner_load_model(queue = queue)
              }

              purrr::imap(pii_texts, function(txt, i) {
                res <- gliner_model$predict_entities(
                  text = txt,
                  labels = labels
                )

                # Bump progress after each text
                n_txt <- length(pii_texts)
                progress$inc(1 / n_txt, detail = sprintf("%d / %d", i, n_txt))

                res
              }) |>
                # Return a named list; original texts are names
                setNames(pii_texts)
            },
            .args = c(
              list(
                app_root = kwallm_worker_app_root(),
                worker_options = kwallm_worker_capture_options(),
                log_context = log_context,
                gliner_model = gliner_model,
                pii_texts = pii_texts(),
                labels = labels,
                progress = progress,
                queue = queue
              ),
              kwallm_worker_bootstrap_globals()
            )
          ) %...>%
            {
              ## SUCCESS ─ tidy predictions & then set state to “evaluating”
              preds <- .

              # ── clean up the raw list into a data frame
              predictions_clean <- purrr::imap_dfr(
                preds,
                function(pred, original) {
                  if (length(pred) == 0) {
                    return(NULL)
                  }
                  purrr::map_dfr(pred, function(ent) {
                    data.frame(
                      original_text = original,
                      start = ent$start,
                      end = ent$end,
                      entity_text = ent$text,
                      label = ent$label,
                      score = ent$score,
                      stringsAsFactors = FALSE
                    )
                  })
                }
              )

              if (nrow(predictions_clean) == 0) {
                # create an empty tibble with the expected columns
                predictions_clean <- tibble::tibble(
                  original_text = character(),
                  start = integer(),
                  end = integer(),
                  entity_text = character(),
                  label = character(),
                  score = double()
                )
              }

              predictions_clean <- predictions_clean |>
                dplyr::group_by(original_text, start, end, entity_text) |>
                dplyr::filter(score == max(score)) |>
                dplyr::ungroup() |>
                dplyr::arrange(dplyr::desc(score)) |>
                tibble::rowid_to_column(".row_id") |>
                dplyr::mutate(anonymize = TRUE)

              pii_predictions(predictions_clean)
              pii_eval(predictions_clean)
              module_state("evaluating")

              # Log detection results
              log_info(
                sprintf(
                  "GLiNER detection complete: n_entities_found=%d, n_unique_labels=%d",
                  nrow(predictions_clean),
                  length(unique(predictions_clean$label))
                ),
                component = "gliner"
              )

              progress$close()
              queue$consumer$stop()
            } %...!%
            {
              ## ERROR ─ notify the user and set state to “error”
              err <- .
              progress$close()
              queue$consumer$stop()

              log_error(
                paste("GLiNER error:", err$message),
                component = "gliner"
              )

              shiny::showNotification(
                paste0(
                  lang()$t("Er is een fout opgetreden bij het anonimiseren: "),
                  err$message
                ),
                type = "error"
              )
              module_state("error")
            }

          NULL # Don't block Shiny's reactive chain by ending on a future
        },
        ignoreInit = TRUE
      )

      # Gliner load message is set by queue in async process;
      #   tells some info about the model loading progress
      gliner_load_message <- reactiveVal(NULL)
      output$gliner_load_message_ui <- renderUI({
        req(gliner_load_message())
        div(
          class = "text-center",
          style = "margin-top: 20px;",
          gliner_load_message()
        )
      })

      ## 3.2 User evaluation of PII entities -------------------------
      # Reactive dataframe with entities + if user wants to anonymize them
      # Column 'anonymize' is TRUE by default, meaning the entity will be anonymized
      # User can uncheck the box to skip anonymization for that entity (setting to FALSE)
      pii_eval <- reactiveVal(NULL)

      # Reactive value to force rerender of pii_entities_ui
      pii_entities_ui_rerender <- reactiveVal(Sys.time())

      # Render the UI for PII entities evaluation
      output$pii_entities_ui <- renderUI({
        req(module_state() == "evaluating")
        df <- isolate(pii_eval())
        req(df)

        if (nrow(df) == 0) {
          return(p(
            lang()$t(
              "Geen van de opgegeven PII-entiteiten zijn gevonden in de teksten."
            ),
            br(),
            lang()$t(
              "Dat zou kunnen betekenen dat de teksten al anoniem zijn, of dat de entiteiten te beperkt zijn, of dat het model bepaaalde PII niet kan herkennen."
            ),
            hr()
          ))
        }

        tagList(
          p(
            paste0(
              lang()$t("Er zijn "),
              nrow(df),
              lang()$t(" PII-entiteiten gevonden in de teksten.")
            ),
            br(),
            lang()$t(
              "Alle checkboxes die aangevinkt zijn, worden geanonimiseerd nadat je op 'Sla op' klikt.",
            ),
            hr()
          ),
          # Max height for the table, otherwise scrollable
          div(
            style = "max-height: 80%; overflow-y: auto; overflow-x: hidden; max-width: 100%;",
            class = "pii-entities-table-container",
            DTOutput(ns("pii_entities_table"))
          )
        )
      })

      # Function to build the check-box HTML for each row
      build_cb <- function(flag, rid) {
        sprintf(
          '<input type="checkbox" class="anon-box shiny-input-checkbox"
            data-rowid="%s" id="%s" %s>',
          rid,
          ns(paste0("anon_", rid)),
          if (flag) "checked" else ""
        )
      }

      # JavaScript for row styling and click handling
      row_css <-
        "
        function(row, data) {
          // input is in the 2nd cell (index 1)
          var $cb = $('input.anon-box', row);
          if(!$cb.prop('checked')) $(row).addClass('skip-anon');
          else $(row).removeClass('skip-anon');
        }"

      # JavaScript for handling check-box clicks
      click_js <- sprintf(
        "
        initComplete = function() {
          var tbl = this.api();
          if (tbl._bound) return;
          tbl._bound = true;

          $(tbl.table().body()).on('change', 'input.anon-box', function() {
            var rowId = $(this).data('rowid');
            var val   = this.checked;
            Shiny.setInputValue('%s',
              {row: rowId, val: val, ts: Date.now()}, {priority:'event'});
          });
        }",
        ns("anon_toggle")
      )

      # Helper function to highlight the entity in the text as bold
      highlight_entity <- function(txt, start, end) {
        htmltools::HTML(paste0(
          htmltools::htmlEscape(substr(txt, 1, start - 1)),
          "<b>",
          htmltools::htmlEscape(substr(txt, start, end)),
          "</b>",
          htmltools::htmlEscape(substr(txt, end + 1, nchar(txt)))
        ))
      }

      # Render the data table with PII entities, plus check-boxes for anonymization
      output$pii_entities_table <- DT::renderDT(server = TRUE, {
        req(isTRUE(module_state() == "evaluating"))
        df <- isolate(pii_eval())
        req(nrow(df) > 0)
        pii_entities_ui_rerender()

        # add the check-boxes
        df$checkbox <- mapply(
          build_cb,
          df$anonymize,
          df$.row_id,
          USE.NAMES = FALSE
        )

        # prettified version of the text
        df$display_text <- mapply(
          highlight_entity,
          df$original_text,
          df$start,
          df$end,
          USE.NAMES = FALSE
        )

        DT::datatable(
          # NB: keep raw text as the *last* column so we can hide it & still group on it
          df[, c(
            ".row_id",
            "checkbox",
            "display_text",
            "entity_text",
            "label",
            "score",
            "original_text"
          )],
          rownames = FALSE,
          escape = FALSE, # allow our <b> tags through
          colnames = c(
            ".row_id",
            "", # hidden id
            lang()$t("Tekst"),
            lang()$t("Entiteit"),
            lang()$t("Label"),
            lang()$t("Confidence"),
            "orig_raw" # hidden column used for grouping
          ),
          selection = "none",
          options = list(
            columnDefs = list(
              list(visible = FALSE, targets = c(0, 6)) # hide .row_id & entity_text & orig_raw
            ),
            paging = FALSE,
            searching = FALSE,
            autoWidth = TRUE,
            ordering = FALSE,
            rowGroup = list(dataSrc = 6), # group on the *raw*, hidden text
            rowCallback = JS(row_css),
            initComplete = JS(click_js)
          )
        ) |>
          formatRound("score", 2)
      })

      # Use data table proxy to update the table without re-rendering
      proxy_dt <- DT::dataTableProxy(ns("pii_entities_table"))

      # Handle checkbox with which user can choose to not anonymize a specific entity
      observeEvent(
        input$anon_toggle,
        {
          info <- input$anon_toggle # $row, $val, $ts

          df <- pii_eval()
          df$anonymize[df$.row_id == info$row] <- info$val
          pii_eval(df)

          n_total <- nrow(df)
          n_selected <- sum(df$anonymize, na.rm = TRUE)
          log_action(
            "gliner_entity_toggle",
            details = sprintf(
              "val=%s n_selected=%d n_total=%d",
              as.character(isTRUE(info$val)),
              n_selected,
              n_total
            )
          )

          # Rebuild check-box HTML for the changed row(s)
          df$checkbox <- mapply(
            build_cb,
            df$anonymize,
            df$.row_id,
            USE.NAMES = FALSE
          )

          replaceData(
            proxy_dt,
            df[, c(
              ".row_id",
              "checkbox",
              "original_text",
              "entity_text",
              "label",
              "score"
            )],
            resetPaging = FALSE,
            rownames = FALSE
          )
        },
        ignoreInit = TRUE
      )

      ## 3.3 Save anonymized texts -----------------------------------
      # User clicks the save anonymization button
      observeEvent(
        input$save_anonymization,
        {
          req(isTRUE(module_state() == "evaluating"))
          req(pii_eval())
          df <- pii_eval()
          req(isTRUE(return$enabled))

          # Filter out the entities that user chose to not anonymize
          df <- df[df$anonymize, ]

          if (nrow(df) == 0) {
            # Just keep the original texts
            anonymized_texts <- pii_texts()

            # Update counts
            return$number_of_pii_entities_removed <- 0
            return$pii_label_counts <- tibble::tibble(
              label = character(),
              count = integer()
            )
          } else {
            # Anonymize the texts by replacing the PII entities with a placeholder
            anonymized_texts <- purrr::imap_chr(pii_texts(), function(txt, i) {
              ents <- df[df$original_text == txt & df$anonymize, ]
              if (nrow(ents) == 0) {
                return(txt)
              }

              ## 1. convert GLiNER’s 0-based inclusive indices to R’s 1-based, end-inclusive
              ents <- ents |>
                dplyr::mutate(start = start + 1, end = end + 1)

              ## 2. work **right-to-left** so every replacement leaves earlier indices intact
              ents <- ents[order(ents$start, decreasing = TRUE), ]

              ## 3. do the replacements
              for (k in seq_len(nrow(ents))) {
                ent <- ents[k, ]
                txt <- paste0(
                  substr(txt, 1, ent$start - 1),
                  sprintf("<< %s verwijderd >>", ent$label),
                  substr(txt, ent$end + 1, nchar(txt))
                )
              }
              txt
            })

            # Update counts
            return$number_of_pii_entities_removed <- nrow(df)
            return$pii_label_counts <- df |>
              dplyr::count(label, name = "count") |>
              dplyr::arrange(dplyr::desc(count))
          }

          # Set the result and done status
          return$anonymized_texts <- anonymized_texts
          return$done <- TRUE

          # Log anonymization saved
          log_info(
            sprintf(
              "GLiNER anonymization saved: n_texts=%d, n_entities_removed=%d",
              length(anonymized_texts),
              return$number_of_pii_entities_removed
            ),
            component = "gliner"
          )

          # Finished; close modal
          log_action("gliner_modal_closed", details = "saved")
          shiny::removeModal()
        },
        ignoreInit = TRUE
      )

      # 3.4 Reset, quit, retry ---------------------------------------
      # Function to reset module state
      reset_state <- function(close_modal = FALSE) {
        module_state("defining")
        pii_predictions(NULL)
        pii_eval(NULL)
        return$done <- FALSE
        return$anonymized_texts <- NULL
        return$number_of_pii_entities_removed <- NULL
        if (isTRUE(close_modal)) shiny::removeModal()
      }

      # Auto-reset when the source texts change
      observeEvent(
        pii_texts(),
        {
          reset_state(close_modal = FALSE)
        },
        ignoreInit = TRUE
      )

      # Quit button
      observeEvent(
        input$quit,
        {
          # Just close the modal, no state reset
          log_action("gliner_modal_closed", details = "quit")
          shiny::removeModal()
        },
        ignoreInit = TRUE
      )

      # Close button (finished state)
      observeEvent(
        input$close_modal,
        {
          log_action("gliner_modal_closed", details = "closed")
          shiny::removeModal()
        },
        ignoreInit = TRUE
      )

      # Retry/reset button
      observeEvent(
        input$retry,
        {
          # State reset, but keep modal open
          log_action("gliner_reset_clicked")
          reset_state(close_modal = FALSE) # keep modal open so user can hit Start again
        },
        ignoreInit = TRUE
      )

      return(return)
    }
  )
}


# 2 Example/development usage --------------------------------------
if (FALSE) {
  # Load core packages
  library(tidyverse)
  library(tidyprompt)
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)
  library(htmltools)
  library(mirai)
  library(promises)
  library(DT)
  # Load components in R/-folder
  r_files <- list.files(
    path = "R",
    pattern = "\\.R$",
    full.names = TRUE
  )
  for (file in r_files) {
    # Source if it isn't this file (avoid infinite recursion)
    if (!grepl("gliner_anonymization\\.R", file)) {
      source(file)
    }
  }

  if (!exists("gliner_model")) {
    if (!exists("gliner_load_model")) {
      source("R/utils_gliner.R")
    }

    # Allows to load Python & interrupt R session without fatal R crash:
    Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

    # Load model:
    gliner_model <- gliner_load_model()

    # prediction <- gliner_model$predict_entities(
    #   text = paste0(
    #     "My name is Luka Koning,",
    #     " I live on 5th avenue street in London.",
    #     " I work at Kennispunt Twente",
    #     " sometimes I visit the University of Twente"
    #   ),
    #   labels = c("person", "address")
    # )
  }

  options(
    anonymization__gliner_model = TRUE # Enable GLiNER model usage
  )

  ui <- bslib::page(
    shinyjs::useShinyjs(),
    language_ui("language")
  )

  server <- function(input, output, session) {
    lang <- language_server("language", processing = reactiveVal(FALSE))

    pii_texts <- reactiveVal(c(
      "My name is Luka Koning, I live on 5th avenue street in London.",
      "my phone number is +3125251512, call me! or mail me at bob@bobthebob.com"
    ))

    # Create the GLiNER module server
    gliner <- gliner_server(
      "gliner",
      pii_texts = pii_texts,
      lang = lang,
      # gliner_model = gliner_model
      gliner_model = NULL
    )

    # Automatically start the GLiNER module when the app starts
    observe({
      req(gliner$start)
      gliner$start()
    })
  }

  shinyApp(ui, server)
}
