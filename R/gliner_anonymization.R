#### 1 Server ####

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

gliner_server <- function(
  id,
  pii_texts = reactiveVal(
    c(
      "My name is Luka Koning, I live on 5th avenue street in London.",
      "i'm Bob and I work at Kennispunt Twente sometimes I visit the University of Twente",
      "my phone number is +3125251512, call me! or mail me at bob@bobthebob.com",
      "it's a nice and sunny day today! Let's go for a walk",
      "i am Bob de Nijs, this is a veryyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy longggggggggggggggggggggggggggggggggggggggggggggggggggggg text and here is my phone number +313243244243 by the way this text will never fit inside a cell of a datatable loooooooooooooooooooooooool",
      "lets go for a walk at 5th avenue street today! btw, my name is Kangorowits Wakka Wakka",
      " u should really check out my twitter, its at twitter.com/lukakoning",
      " im christian and i live in enschede",
      " im gay and i live in amsterdam"
    )
  ),
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  ),
  gliner_model = gliner_load_model()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #### 1 Manage state ####

      # To start/initialize the module from the main server:
      start <- function() {
        if (
          !getOption("anonymization__gliner_model", FALSE) |
            length(pii_texts()) == 0
        ) {
          return$enabled <- FALSE
          return()
        }

        # Require model to be not NULL
        if (is.null(gliner_model)) {
          shiny::showNotification(
            lang()$t("GLiNER model is not loaded"),
            type = "error"
          )
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
          uiOutput(ns("modal_content"))
        ))

        return$enabled <- TRUE
      }

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

      #### 2 Modal UI ####

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
                  phone number,
                  email address,
                  personal identification number,
                  passport number,
                  bank account number,
                  license plate code,
                  religious affiliation,
                  political affiliation,
                  sexual orientation,
                  IP address,
                  username,
                  password
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
                    lang()$t("Stoppen")
                  ),
                  class = "btn btn-danger"
                ),
                actionButton(
                  ns("start_anonymization"),
                  label = tagList(
                    icon("play"),
                    lang()$t("Start anonimiseren")
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
                lang()$t("Model is bezig met detectie van entiteiten."),
                br(),
                lang()$t(
                  "Dit kan even duren, afhankelijk van de hoeveelheid tekst en de hardware van de machine."
                ),
                br(),
                lang()$t(
                  "Bekijk de voortgang in de notificatie..."
                )
              ),
              shiny::tags$div(
                class = "text-center",
                shiny::tags$img(
                  src = "www/loading.gif",
                  alt = lang()$t("Loading..."),
                  style = "width: 50px; height: 50px;"
                )
              )
            )
          },

          # Error occurred during processing
          error = {
            tagList(
              h3(lang()$t(
                "Er is een fout opgetreden tijdens het anonimiseren"
              )),
              p(lang()$t(
                "Probeer het opnieuw of neem contact op met de beheerder."
              )),
              hr(),
              # Quit & retry buttons
              div(
                style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                actionButton(
                  ns("quit"),
                  label = tagList(
                    icon("sign-out-alt"),
                    lang()$t("Stoppen")
                  ),
                  class = "btn btn-danger"
                ),
                actionButton(
                  ns("retry"),
                  label = tagList(
                    icon("redo"),
                    lang()$t("Probeer opnieuw")
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
                    lang()$t("Stoppen")
                  ),
                  class = "btn btn-danger"
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

      #### 3 Process handlers ####

      ##### 3.1 Run model to detect PII entities #####

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
          if (length(labels) == 0 || all(labels == "")) {
            shiny::showNotification(
              lang()$t("Voer ten minste één entity in om te verwijderen."),
              type = "error"
            )
            return()
          }

          ## 2 Switch the modal to the “running” state
          module_state("running")

          ## 3 Build a progress bar that the worker can update
          n_txt <- length(pii_texts())
          progress <- ipc::AsyncProgress$new(
            message = lang()$t("Detectie van entiteiten…"),
            detail = sprintf(lang()$t("0 van %d teksten klaar"), n_txt)
          )

          print(gliner_model)
          print(pii_texts())
          print(labels)
          print(progress)

          ## 4 Spawn the future that runs GLiNER model on texts
          future(
            {
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
            globals = list(
              gliner_model = gliner_model,
              pii_texts = pii_texts(),
              labels = labels,
              progress = progress
            ),
            seed = NULL
          ) %...>%
            {
              ## SUCCESS ─ tidy predictions & then set state to “evaluating”
              preds <- .

              # ── clean up the raw list into a data frame
              predictions_clean <- purrr::imap_dfr(
                preds,
                function(pred, original) {
                  if (length(pred) == 0) return(NULL)
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
              ) |>
                dplyr::group_by(original_text, start, end, entity_text) |>
                dplyr::filter(score == max(score)) |>
                dplyr::ungroup() |>
                dplyr::arrange(dplyr::desc(score)) |>
                tibble::rowid_to_column(".row_id") |>
                dplyr::mutate(anonymize = TRUE)

              # Hand the data to the rest of the module
              pii_predictions(predictions_clean)
              pii_eval(predictions_clean)
              module_state("evaluating")

              progress$close()
            } %...!%
            {
              ## ERROR ─ notify the user and set state to “error”
              err <- .
              progress$close()

              print(err)

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

      ##### 3.2 User evaluation of PII entities #####

      # Reactive dataframe with entities + if user wants to anonymize them
      # Column 'anonymize' is TRUE by default, meaning the entity will be anonymized
      # User can uncheck the box to skip anonymization for that entity (setting to FALSE)
      pii_eval <- reactiveVal(NULL)

      # Render the UI for PII entities evaluation
      output$pii_entities_ui <- renderUI({
        req(module_state() == "evaluating")
        df <- isolate(pii_eval())
        req(df)

        if (nrow(df) == 0) {
          return(p(
            lang()$t(
              "Er zijn geen PII-entiteiten gevonden in de teksten."
            ),
            br(),
            lang()$t(
              "Dat zou kunnen betekenen dat de teksten al anoniem zijn."
            )
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
            )
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
            console.log('⇢ anon_toggle', rowId, val);          // DEBUG browser
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
      output$pii_entities_table <- renderDT(server = TRUE, {
        req(isTRUE(module_state() == "evaluating"))
        df <- isolate(pii_eval())
        req(nrow(df) > 0)

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
      proxy_dt <- dataTableProxy(ns("pii_entities_table"))

      # Handle checkbox with which user can choose to not anonymize a specific entity
      observeEvent(
        input$anon_toggle,
        {
          info <- input$anon_toggle # $row, $val, $ts

          df <- pii_eval()
          df$anonymize[df$.row_id == info$row] <- info$val
          pii_eval(df)

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

      ##### 3.3 Save anonymized texts #####

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
            anonymized_texts <- pii_texts() |>
              purrr::imap(function(txt, i) {
                ents <- df[df$original_text == txt, ]
                if (nrow(ents) == 0) return(txt)

                for (j in seq_len(nrow(ents))) {
                  ent <- ents[j, ]
                  txt <- stringr::str_sub(txt, 1, ent$start - 1) |>
                    paste0("<< removed: ", ent$label, " >>") |>
                    paste0(stringr::str_sub(txt, ent$end + 1))
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

          # Show notification about the anonymization; number of entities removed
          shiny::showNotification(
            paste0(
              lang()$t(
                "Teksten opgeslagen! ",
              ),
              return$number_of_pii_entities_removed,
              " PII-entities verwijderd"
            ),
            type = "message"
          )

          # Finished; close modal
          shiny::removeModal()
        },
        ignoreInit = TRUE
      )

      #### 3.4 Reset, quit, retry ####

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
          reset_state(close_modal = TRUE)
        },
        ignoreInit = TRUE
      )

      # Retry button (shown in the “error” state)
      observeEvent(
        input$retry,
        {
          reset_state(close_modal = FALSE) # keep modal open so user can hit Start again
        },
        ignoreInit = TRUE
      )

      return(return)
    }
  )
}


#### 2 Load model ####

gliner_load_model <- function(
  venv_name = "kwallm8",
  python_version = "3.12.10",
  model_name = "urchade/gliner_multi_pii-v1"
) {
  stopifnot(
    is.character(venv_name),
    length(venv_name) == 1,
    is.character(python_version),
    length(python_version) == 1,
    is.character(model_name),
    length(model_name) == 1
  )

  #### 1 Load/create virtual environment ####

  cli::cli_alert_info(paste0(
    "Loading/creating virtual environment (",
    venv_name,
    ") for GLiNER model..."
  ))

  if (!reticulate::virtualenv_exists(venv_name)) {
    python_version <- "3.12.10"

    # Install Python if not already installed
    reticulate::install_python(
      version = python_version
    )

    # Create virtual environment
    reticulate::virtualenv_create(
      envname = venv_name,
      python = python_version
    )
  }
  if (!reticulate::virtualenv_exists(venv_name)) {
    stop(
      "Virtual environment '",
      venv_name,
      "' does not exist; cannot load GLiNER model"
    )
  }
  reticulate::use_virtualenv(venv_name, required = TRUE)

  #### 2 Load gliner ####

  cli::cli_alert_info("Loading/installing 'gliner' Python package...")

  available_packages <- reticulate::py_list_packages(envname = venv_name)
  if (!"gliner" %in% available_packages$package) {
    reticulate::py_install(
      envname = venv_name,
      packages = "gliner"
    )
  }
  available_packages <- reticulate::py_list_packages(envname = venv_name)
  if (
    !"gliner" %in% reticulate::py_list_packages(envname = venv_name)$package
  ) {
    stop(
      "Package 'gliner' is not installed in virtual environment '",
      venv_name,
      "'; cannot load GLiNER model"
    )
  }
  gliner <- reticulate::import("gliner")

  #### 3 (Down)load model ####

  cli::cli_alert_info(paste0(
    "Loading/downloading GLiNER model ('",
    model_name,
    "')..."
  ))

  # Disable symlinks to avoid issues with copying files in some environments
  Sys.setenv(HF_HUB_DISABLE_SYMLINKS = "1")

  # (Down)load the GLiNER model from Hugging Face
  model <- gliner$GLiNER$from_pretrained(
    "urchade/gliner_multi_pii-v1",
    cache_dir = reticulate::virtualenv_python(venv_name) |> dirname()
  )

  #### 4 Test the model ####

  cli::cli_alert_info("Testing GLiNER model...")

  test_result <- tryCatch(
    {
      model$predict_entities(
        text = paste0(
          "My name is Luka Koning,",
          " I live on 5th avenue street in London.",
          " I work at Kennispunt Twente",
          " sometimes I visit the University of Twente",
          " lets go for a walk at 5th avenue street today! btw, my name is Kangorowits Wakka Wakka"
        ),
        labels = c("person", "address", "employer")
      )
    },
    error = function(e) {
      stop("Error testing GLiNER model: ", e$message)
    }
  )

  cli::cli_alert_success("GLiNER model loaded successfully!")

  return(model)
}

# model <- gliner_load_model(
#   venv_name = "kwallm__venv7",
#   python_version = "3.12.10",
#   model_name = "urchade/gliner_multi_pii-v1"
# )

#### 3 Example/development usage ####

if (FALSE) {
  # Load core packages
  library(tidyverse)
  library(tidyprompt)
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)
  library(htmltools)
  library(future)
  library(promises)
  library(DT)
  library(ipc)

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
    # Allows to load Python & interrupt R session without fatal R crash:
    Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

    # Load model:
    if (!exists("gliner_model")) gliner_model <- gliner_load_model()

    # prediction <- gliner_model$predict_entities(
    #   text = paste0(
    #     "My name is Luka Koning,",
    #     " I live on 5th avenue street in London.",
    #     " I work at Kennispunt Twente",
    #     " sometimes I visit the University of Twente"
    #   ),
    #   labels = c("person", "address", "employer")
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

    # Create the GLiNER module server
    gliner <- gliner_server(
      "gliner",
      lang = lang,
      gliner_model = gliner_model
    )

    # Automatically start the GLiNER module when the app starts
    observe({
      req(gliner$start)
      gliner$start()
    })
  }

  shinyApp(ui, server)
}
