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
  pii_texts = reactiveVal(c(
    "My name is Luka Koning, I live on 5th avenue street in London.",
    "i'm Bob and I work at Kennispunt Twente sometimes I visit the University of Twente",
    "my phone number is +3125251512, call me! or mail me at bob@bobthebob.com",
    "it's a nice and sunny day today! Let's go for a walk",
    "i am Bob de Nijs, this is a veryyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy longggggggggggggggggggggggggggggggggggggggggggggggggggggg text and here is my phone number +313243244243 by the way this text will never fit inside a cell of a datatable loooooooooooooooooooooooool",
    "lets go for a walk at 5th avenue street today! btw, my name is Kangorowits Wakka Wakka",
    " u should really check out my twitter, its at twitter.com/lukakoning"
  )),
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #### 1 Manage state ####

      # To start/initialize the module from the main server:
      start <- function() {
        showModal(modalDialog(
          title = lang()$t("Anonimiseer met GLiNER-model"),
          size = "xl",
          easyClose = FALSE,
          footer = NULL,
          uiOutput(ns("modal_content"))
        ))
      }

      # Make available to the main server:
      #   start function; done status; result (will be the anonymized texts)
      return <- reactiveValues(
        start = start,
        done = FALSE,
        result = NULL
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
                  "Voer namen in van 'entities' die je zou willen verwijderen."
                ),
                lang()$t(
                  "Bijvoorbeeld: naam, adres, werkgever, telefoonnummer, e-mail, et cetera."
                ),
                lang()$t(
                  "Het lokale GLiNER-model zal deze entities proberen te vinden in de teksten."
                )
              ),
              hr(),
              textAreaInput(
                ns("pii_labels"),
                width = "100%",
                label = lang()$t("Entities (gescheiden door komma's):"),
                value = "
                  name of person,
                  date of birth,
                  employer of person,
                  personal address,
                  personal phone number,
                  personal email address,
                  personal identification number,
                  passport number,
                  bank account number,
                  license plate code,
                  personal religious affiliation,
                  personal political affiliation,
                  personal sexual orientation,
                  link to personal web page,
                  link to personal social media profile,
                  IP address,
                  personal username,
                  personal password
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
                lang()$t("Model is bezig met detectie van entiteiten..."),
                br(),
                lang()$t(
                  "Dit kan even duren, afhankelijk van de hoeveelheid tekst en de hardware van de machine..."
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
              actionButton(ns("retry"), lang()$t("Opnieuw proberen")),
              actionButton(ns("quit"), lang()$t("Stoppen"))
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
              h3(lang()$t("Evalueer de verwijderde PII-entities")),
              uiOutput(ns("pii_entities_ui")),
              actionButton(ns("undo_removals"), lang()$t("Ongedaan maken")),
              actionButton(ns("save_anonymized_texts"), lang()$t("Opslaan"))
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

      ##### 3.1 Anonymization #####

      # Reactive value to store the PII entities predictions
      pii_predictions <- reactiveVal(NULL)

      # User clicks the start anonymization button
      observeEvent(
        input$start_anonymization,
        {
          req(input$pii_labels)

          # Get the labels from the text area input
          labels <- strsplit(input$pii_labels, ",")[[1]]
          labels <- trimws(labels) # Remove any leading/trailing whitespace

          # Check if labels are provided
          if (length(labels) == 0 || all(labels == "")) {
            shiny::showNotification(
              lang()$t("Voer ten minste één entity in om te verwijderen."),
              type = "error"
            )
            return()
          }

          # Set the module state to running
          module_state("running")

          # Predict entities using the GLiNER model (async)
          # Async prediction
          future(
            {
              model <- gliner_load_model()

              purrr::map(texts, function(text) {
                # Predict entities for each text
                model$predict_entities(
                  text = text,
                  labels = labels
                )
              }) |>
                setNames(texts)
            },
            globals = list(
              gliner_load_model = gliner_load_model,
              texts = pii_texts(),
              labels = labels
            ),
            seed = NULL
          ) %...>%
            {
              # Successful prediction
              predictions <- .

              # Every prediction is a list, containing potentially multiple entities
              # E.g., entity <- predictions[[1]][[1]],
              #   where we will have:
              #     entity$score (e.g., 0.985),
              #     entity$text (e.g., "Luka Koning"),
              #     entity$label (e.g., "name")
              #     entity$start (e.g., 11), entity$end (e.g., 22)
              # And the predictions are a named list, where the names are the original texts

              # Clean the predictions to a data frame;
              #   should contain:
              #     text (original text),
              #     start (start index of the entity in the original text),
              #     end (end index of the entity in the original text),
              #     entity_text (text of the entity),
              #     label (label of the entity),
              #     score (confidence score of the entity)
              # Discard any texts that do not have any entities
              predictions_clean <- purrr::map_df(
                predictions,
                function(pred) {
                  if (length(pred) == 0) {
                    return(NULL) # No entities found for this text
                  }
                  # Convert each entity to a data frame row
                  purrr::map_df(pred, function(entity) {
                    data.frame(
                      start = entity$start,
                      end = entity$end,
                      entity_text = entity$text,
                      label = entity$label,
                      score = entity$score,
                      stringsAsFactors = FALSE
                    )
                  })
                },
                .id = "original_text"
              )

              # If same text has same entity multiple times,
              #   take the one with the highest score
              #   and get rid of the others
              predictions_clean <- predictions_clean |>
                group_by(original_text, start, end, entity_text) |>
                filter(score == max(score)) |>
                ungroup()

              # Add some additional columns for evaluation
              predictions_clean <- predictions_clean |>
                arrange(
                  desc(score)
                ) |>
                tibble::rowid_to_column(".row_id") |> # unique id for every row
                dplyr::mutate(anonymize = TRUE) # default: anonymise

              # Store the predictions in the reactive value, update state
              pii_predictions(predictions_clean)
              pii_eval(predictions_clean)
              module_state("evaluating")
            } %...!%
            (function(e) {
              # Error during prediction
              shiny::showNotification(
                paste0(
                  lang()$t("Er is een fout opgetreden bij het anonimiseren: "),
                  e$message
                ),
                type = "error"
              )
              print(e)
              module_state("error")
            })

          print("Started entity scanning by GLiNER model...")
        }
      )

      #### 3.2 Evaluate PII entities #####

      # Store dataframe with predictions + evaluation state (user decisions)
      # User can click checkboxes to decide which entities to anonymize or not
      ###############################################################################
      ## 3·2  Evaluate PII entities – check-box version                             ##
      ###############################################################################

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
              "Er zijn geen PII-entities gevonden in de teksten."
            ),
            br(),
            lang()$t(
              "Dat zou kunnen betekenen dat de teksten al anoniem zijn."
            )
          ))
        }

        tagList(
          p(lang()$t(
            "Vink het vakje uit als je deze entiteit níet wilt anonimiseren."
          )),
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

      # Render the data table with PII entities, plus check-boxes for anonymization
      output$pii_entities_table <- renderDT(
        server = TRUE,
        {
          req(isTRUE(module_state() == "evaluating"))
          df <- isolate(pii_eval())
          req(nrow(df) > 0)

          df$checkbox <- mapply(
            build_cb,
            df$anonymize,
            df$.row_id,
            USE.NAMES = FALSE
          )

          DT::datatable(
            df[, c(
              ".row_id",
              "checkbox",
              "original_text",
              "entity_text",
              "label",
              "score"
            )],
            rownames = FALSE,
            escape = FALSE,
            colnames = c(
              ".row_id",
              "", # hide id, blank header
              lang()$t("Brontekst"),
              lang()$t("Entiteit"),
              lang()$t("Label"),
              lang()$t("Confidence")
            ),
            selection = "none",
            options = list(
              columnDefs = list(list(visible = FALSE, targets = 0)), # hide .row_id
              paging = FALSE,
              searching = FALSE,
              autoWidth = TRUE,
              ordering = FALSE,
              rowGroup = list(dataSrc = 2), # group on text
              rowCallback = JS(row_css),
              initComplete = JS(click_js)
            )
          ) |>
            formatRound("score", 2)
        }
      )

      proxy_dt <- dataTableProxy(ns("pii_entities_table"))

      observeEvent(
        input$anon_toggle,
        {
          info <- input$anon_toggle # $row, $val, $ts
          print(str(info))

          df <- pii_eval()
          df$anonymize[df$.row_id == info$row] <- info$val
          pii_eval(df)
          print(pii_eval())

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

      return(return)
    }
  )
}


#### 2 Load model ####

gliner_load_model <- function(
  venv_name = "kwallm__venv7",
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
  reticulate::use_virtualenv(venv_name)

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
library(reactable)

# Load components in R/-folder
r_files <- list.files(
  path = "R",
  pattern = "\\.R$",
  full.names = TRUE
)
for (file in r_files) {
  if (!grepl("llmQuali-package\\.R|rstudio_addin\\.R|zzz\\.R", file)) {
    source(file)
  }
}

if (FALSE) {
  model <- gliner_load_model(
    venv_name = "kwallm__venv7",
    python_version = "3.12.10",
    model_name = "urchade/gliner_multi_pii-v1"
  )
  prediction <- model$predict_entities(
    text = paste0(
      "My name is Luka Koning,",
      " I live on 5th avenue street in London.",
      " I work at Kennispunt Twente",
      " sometimes I visit the University of Twente"
    ),
    labels = c("person", "address", "employer")
  )
}

if (TRUE) {
  ui <- bslib::page(
    shinyjs::useShinyjs(),
    language_ui("language")
  )

  server <- function(input, output, session) {
    lang <- language_server("language", processing = reactiveVal(FALSE))

    # Create the GLiNER module server
    gliner <- gliner_server(
      "gliner",
      lang = lang
    )

    # Automatically start the GLiNER module when the app starts
    observe({
      req(gliner$start)
      gliner$start()
    })
  }

  shinyApp(ui, server)
}
