# User interface for selecting a large language model (LLM) and displaying its URL

# Provider mode: preconfigured/openai/ollama
#   - In preconfigured, user can choose from (named) list of preconfigured LLM
#       providers. These contain both provider & model settings. In this
#       module, user can still configure/override some further settings
#   - In openai/ollama, this is the provider object that is configured in the UI
#       Is present here in reactive value 'llm_provider_configured', comes from
#       the llm_provider module. In this module, user can configure the model
#       and some further settings

# 1 UI ---------------------------------------------------------------
model_ui <- function(
  id
) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("card"))
  )
}


# 2 Server ---------------------------------------------------------
model_server <- function(
  id,

  preconfigured_llm_provider_model_main = list(
    "gpt 4.1 mini" = tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4-turbo"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-5"
    ))
  ),

  preconfigured_llm_provider_model_large = list(
    "gpt 4.1 mini" = tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4-turbo"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-5-mini"
    )),
    tidyprompt::llm_provider_mistral()
  ),

  processing = reactiveVal(FALSE),
  mode = reactiveVal("Onderwerpextractie"),

  # From the LLM provider module:
  llm_provider_rv = reactiveValues(
    # Reactive value for the LLM provider mode
    provider_mode = "preconfigured",
    # OpenAI provider or Ollama provider as configured in UI:
    llm_provider_configured = tidyprompt::llm_provider_openai(),
    # Vector of models available for the configured LLM provider
    #   (= result from sending request to the configured provider)
    configured_models = c("gpt-4o", "gpt-4o-mini")
  ),

  lang = default_lang()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Add names to preconfigured model lists & verify unique names -----------

      preconfigured_llm_provider_model_main <- add_names_to_llm_provider_model_list(
        preconfigured_llm_provider_model_main
      )
      verify_unique_llm_provider_model_names(
        preconfigured_llm_provider_model_main
      )
      preconfigured_llm_provider_model_large <- add_names_to_llm_provider_model_list(
        preconfigured_llm_provider_model_large
      )
      verify_unique_llm_provider_model_names(
        preconfigured_llm_provider_model_large
      )

      # Reactive values --------------------------------------------------------

      # Holds the LLM providers selected for main & large, as selected by user
      # (Note that LLM provider will contain both provider and model settings)
      models <- reactiveValues(
        main = NULL,
        large = NULL
      )

      # Variables to hold state across LLM provider modes
      saved <- reactiveValues(
        preconfigured = list(
          main_choice = NULL,
          main = NULL,
          large_choice = NULL,
          large = NULL
        ),
        openai = list(
          main_choice = NULL,
          main = NULL,
          large_choice = NULL,
          large = NULL
        ),
        ollama = list(
          main_choice = NULL,
          main = NULL,
          large_choice = NULL,
          large = NULL
        )
      )
      current_mode <- reactive({
        req(llm_provider_rv$provider_mode)
        llm_provider_rv$provider_mode
      })

      save_selection <- function(which) {
        mode <- current_mode()
        cur <- saved[[mode]]
        if (identical(which, "main")) {
          cur$main_choice <- isolate(input$main_model)
          cur$main <- isolate(models$main)
        } else {
          cur$large_choice <- isolate(input$large_model)
          cur$large <- isolate(models$large)
        }
        saved[[mode]] <- cur
      }

      # --- DRY helpers --------------------------------------------------------

      `%||%` <- function(a, b) if (is.null(a)) b else a

      # Clone provider and set parameters (overwrite params entirely)
      clone_with_params <- function(prov, p) {
        cp <- prov$clone(deep = TRUE)
        cp$parameters <- p
        cp
      }

      # Touch the appropriate "updated" reactiveVal
      touch_provider_updated <- function(
        which,
        main_provider_updated,
        large_provider_updated
      ) {
        if (which == "main") {
          main_provider_updated(Sys.time())
        } else {
          large_provider_updated(Sys.time())
        }
      }

      # Read modal inputs for one side (main/large)
      read_modal_inputs <- function(input, which) {
        list(
          effort = (input[[paste0(which, "_reasoning_effort")]] %||% ""),
          verbosity = (input[[paste0(which, "_verbosity")]] %||% ""),
          temperature = input[[paste0(which, "_temperature")]], # NA/NULL => remove
          top_p = input[[paste0(which, "_top_p")]]
        )
      }

      # Build new parameters list from inputs (blank/NA => remove)
      build_parameters <- function(old_params, inps) {
        p <- old_params
        if (is.null(p)) {
          p <- list()
        }

        # reasoning.effort
        if (nzchar(inps$effort)) {
          p$reasoning <- list(effort = inps$effort)
        } else {
          p$reasoning <- NULL
        }

        # text.verbosity
        if (nzchar(inps$verbosity)) {
          p$text <- list(verbosity = inps$verbosity)
        } else {
          p$text <- NULL
        }

        # temperature (NA/NULL/length0 => remove)
        tp <- inps$temperature
        if (length(tp) == 0 || is.null(tp) || is.na(tp)) {
          p$temperature <- NULL
        } else {
          p$temperature <- as.numeric(tp)
        }

        # top_p
        tp_p <- inps$top_p
        if (length(tp_p) == 0 || is.null(tp_p) || is.na(tp_p)) {
          p$top_p <- NULL
        } else {
          p$top_p <- as.numeric(tp_p)
        }

        p
      }

      # Update Temperature input (keeps field synced with provider)
      update_temperature_ui <- function(session, input, models, which) {
        input_id <- paste0(which, "_temperature")
        # Only update if the modal control exists
        if (!is.null(input[[input_id]])) {
          prov <- models[[which]]
          if (!is.null(prov)) {
            val <- prov$parameters$temperature
            if (is.null(val)) {
              val <- NA_real_
            }
            updateNumericInput(session, input_id, value = val)
          }
        }
      }

      # Update top_p input (keeps field synced with provider)
      update_top_p_ui <- function(session, input, models, which) {
        input_id <- paste0(which, "_top_p")
        if (!is.null(input[[input_id]])) {
          prov <- models[[which]]
          if (!is.null(prov)) {
            val <- prov$parameters$top_p
            if (is.null(val)) {
              val <- NA_real_
            }
            updateNumericInput(session, input_id, value = val)
          }
        }
      }

      # Wire a single observer for main/large settings
      wire_settings_observer <- function(
        which,
        input,
        session,
        models,
        main_provider_updated,
        large_provider_updated
      ) {
        observeEvent(
          list(
            input[[paste0(which, "_reasoning_effort")]],
            input[[paste0(which, "_verbosity")]],
            input[[paste0(which, "_temperature")]],
            input[[paste0(which, "_top_p")]]
          ),
          ignoreInit = TRUE,
          handlerExpr = {
            req(models[[which]])

            old_params <- models[[which]]$parameters
            inps <- read_modal_inputs(input, which)
            p <- build_parameters(old_params, inps)

            models[[which]] <- clone_with_params(models[[which]], p)

            # keep preview + UI in sync
            touch_provider_updated(
              which,
              main_provider_updated,
              large_provider_updated
            )
            update_temperature_ui(session, input, models, which)
            update_top_p_ui(session, input, models, which)
          }
        )
      }

      # Main card UI -----------------------------------------------------------

      output$card <- renderUI({
        bslib::card(
          class = "card",
          card_header_with_tooltip(
            "Model",
            paste0(
              lang()$t(
                "Kies hier het LLM (large language model) dat je wilt gebruiken."
              ),
              lang()$t(
                " Verschillende modellen hebben verschillende kwaliteiten; het ene model is sneller/goedkoper,"
              ),
              lang()$t(
                " terwijl het andere model bijvoorbeeld duurder/langzamer is maar wel betere resultaten oplevert."
              ),
              lang()$t(
                " Hoe goed het model moet zijn is afhankelijk van de complexiteit van de analysevraag die je hebt."
              )
            )
          ),
          card_body(
            div(
              class = "d-flex flex-wrap justify-content-center gap-3",
              uiOutput(ns("main_model_selector_ui")),
              uiOutput(ns("large_model_selector_ui"))
            ),
            div(
              class = "text-center w-100",
              uiOutput(ns("url"))
            )
          )
        )
      })

      # Show URL of current LLM provider(s)
      output$url <- renderUI({
        # In Ollama/OpenAI mode, show URL of configured provider
        if (llm_provider_rv$provider_mode %in% c("openai", "ollama")) {
          return(HTML(llm_provider_rv$llm_provider_configured$url))
        }

        # Otherwise, show URL of chosen LLM providers (preconfigured)
        if (mode() == "Onderwerpextractie") {
          if (
            !is.null(models$main$url) &&
              isTRUE(models$main$url == models$large$url)
          ) {
            return(HTML(models$main$url))
          } else {
            req(models$main$url, models$large$url)
            return(HTML(paste0(
              models$main$url,
              "<br>&<br>",
              models$large$url
            )))
          }
        }

        req(models$main$url)
        return(HTML(models$main$url))
      })

      # Handle provider/model selection ----------------------------------------

      # Select main
      output$main_model_selector_ui <- renderUI({
        mode <- current_mode()

        choices <- if (mode == "preconfigured") {
          names(preconfigured_llm_provider_model_main)
        } else {
          llm_provider_rv$configured_models
        }

        # Restore previously chosen item for this mode (if still valid)
        sel <- saved[[mode]]$main_choice
        if (is.null(sel) || !(sel %in% choices)) {
          sel <- NULL
        }

        div(
          class = "selector-container text-center",
          selectInput(
            inputId = ns("main_model"),
            label = tagList(
              HTML(lang()$t("Model")),
              bslib::tooltip(
                shiny::actionLink(
                  ns("main_cog"),
                  NULL,
                  icon = shiny::icon("gear"),
                  class = "ms-2"
                ),
                lang()$t("Geavanceerde instellingen voor dit model")
              )
            ),
            choices = choices,
            selected = sel
          )
        )
      })

      # Select large (voor onderwerpen reduceren bij onderwerpextractie)
      output$large_model_selector_ui <- renderUI({
        req(mode() == "Onderwerpextractie")
        mode <- current_mode()

        choices <- if (mode == "preconfigured") {
          names(preconfigured_llm_provider_model_large)
        } else {
          llm_provider_rv$configured_models
        }

        sel <- saved[[mode]]$large_choice
        if (is.null(sel) || !(sel %in% choices)) {
          sel <- NULL
        }

        div(
          class = "selector-container text-center",
          selectInput(
            inputId = ns("large_model"),
            label = span(
              HTML(lang()$t("Model voor onderwerpreductie")),
              bslib::tooltip(
                bsicons::bs_icon("info-circle"),
                paste0(
                  lang()$t(
                    "Bij onderwerpextractie kan je een apart model kiezen dat gebruikt zal worden voor het terugbrengen van mogelijke onderwerpen tot een finale lijst van onderwerpen."
                  ),
                  lang()$t(
                    " Omdat deze stap slechts één keer gebeurt maar wel grote invloed heeft op de resultaten, wordt aangeraden om een groter model te kiezen (mogelijk een 'thinking'-model zoals OpenAI's o3 of Deepseek's R1)."
                  )
                )
              ),
              bslib::tooltip(
                shiny::actionLink(
                  ns("large_cog"),
                  NULL,
                  icon = shiny::icon("gear"),
                  class = "ms-2"
                ),
                lang()$t("Geavanceerde instellingen voor dit model")
              )
            ),
            choices = choices,
            selected = sel
          )
        )
      })

      # Keep saved selections valid if the configured provider's model list changes
      observe({
        mode <- current_mode()
        if (mode == "preconfigured") {
          return()
        }

        choices <- llm_provider_rv$configured_models
        req(choices)

        cur <- saved[[mode]]

        if (!is.null(cur$main_choice) && !(cur$main_choice %in% choices)) {
          cur$main_choice <- NULL
          cur$main <- NULL
        }
        if (!is.null(cur$large_choice) && !(cur$large_choice %in% choices)) {
          cur$large_choice <- NULL
          cur$large <- NULL
        }
        saved[[mode]] <- cur
      })

      # When main/large model is selected or changed
      observeEvent(input$main_model, {
        req(input$main_model)
        req(llm_provider_rv$provider_mode)

        if (llm_provider_rv$provider_mode == "preconfigured") {
          models$main <- preconfigured_llm_provider_model_main[[
            input$main_model
          ]]
        } else {
          models$main <- llm_provider_rv$llm_provider_configured$clone()
          models$main <- models$main$set_parameters(list(
            model = input$main_model
          ))
        }

        save_selection("main")
        update_json_mode_ui("main")
        update_temperature_ui(session, input, models, "main")
        update_top_p_ui(session, input, models, "large")
        main_provider_updated(Sys.time())
      })

      observeEvent(input$large_model, {
        req(input$large_model)
        req(llm_provider_rv$provider_mode)

        if (llm_provider_rv$provider_mode == "preconfigured") {
          models$large <- preconfigured_llm_provider_model_large[[
            input$large_model
          ]]
        } else {
          models$large <- llm_provider_rv$llm_provider_configured$clone()
          models$large <- models$large$set_parameters(list(
            model = input$large_model
          ))
        }

        save_selection("large")
        update_json_mode_ui("large")
        update_temperature_ui(session, input, models, "large")
        update_top_p_ui(session, input, models, "large")
        large_provider_updated(Sys.time())
      })

      # Advanced model configuration modal -------------------------------------

      open_settings_modal <- function(which = c("main", "large")) {
        which <- match.arg(which)
        title_txt <- if (which == "main") {
          lang()$t("Geavanceerde instellingen — primair model")
        } else {
          lang()$t("Geavanceerde instellingen — groot model")
        }

        provider <- if (which == "main") models$main else models$large
        req(provider)

        selected_effort <- provider$parameters$reasoning$effort
        selected_verbosity <- provider$parameters$text$verbosity
        if (is.null(selected_effort)) {
          selected_effort <- ""
        }
        if (is.null(selected_verbosity)) {
          selected_verbosity <- ""
        }

        selected_json_type <- provider$json_type
        if (is.null(selected_json_type) || !nzchar(selected_json_type)) {
          selected_json_type <- "auto"
        }

        # current temperature (NA renders as blank)
        selected_temperature <- provider$parameters$temperature
        if (is.null(selected_temperature)) {
          selected_temperature <- NA_real_
        }

        selected_top_p <- provider$parameters$top_p
        if (is.null(selected_top_p)) {
          selected_top_p <- NA_real_
        }

        id_prefix <- if (which == "main") "main" else "large"

        unset_lbl <- lang()$t("Niet ingesteld")

        effort_choices <- setNames(
          c("", "minimal", "low", "medium", "high"),
          c(unset_lbl, "minimal", "low", "medium", "high")
        )

        verbosity_choices <- setNames(
          c("", "low", "medium", "high"),
          c(unset_lbl, "low", "medium", "high")
        )

        showModal(
          modalDialog(
            title = title_txt,
            size = "xl",
            easyClose = TRUE,
            footer = modalButton(lang()$t("Sluiten")),
            div(
              # === Controls in a 2-col wrap ===
              bslib::layout_column_wrap(
                width = 1 / 2, # <= up to 2 per row
                gap = "1rem",
                # Reasoning effort
                div(
                  class = "d-flex flex-column",
                  tags$label(
                    class = "form-label",
                    tagList(
                      lang()$t("Reasoning effort"),
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        paste0(
                          lang()$t(
                            "Voor modellen die het ondersteunen (bijv., gpt-5) regelt dit de denkinspanning van het model (snelheid vs. diepgang)."
                          ),
                          lang()$t(
                            " Als het model het niet ondersteunt, doet deze parameter niets of kan het mogelijk tot errors leiden. Raadpleeg de API-documentatie."
                          ),
                          lang()$t(
                            " Laat het veld leeg om de parameter niet mee sturen."
                          )
                        )
                      )
                    )
                  ),
                  selectInput(
                    inputId = session$ns(paste0(
                      id_prefix,
                      "_reasoning_effort"
                    )),
                    label = NULL,
                    choices = effort_choices,
                    selected = selected_effort
                  )
                ),

                # Verbosity
                div(
                  class = "d-flex flex-column",
                  tags$label(
                    class = "form-label",
                    tagList(
                      lang()$t("Verbosity"),
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        paste0(
                          lang()$t(
                            "Voor modellen die het ondersteunen (bijv., gpt-5) regelt dit de verbositeit van het model (beknopt vs. uitgebreid)."
                          ),
                          lang()$t(
                            " Als het model het niet ondersteunt, doet deze parameter niets of kan het mogelijk tot errors leiden. Raadpleeg de API-documentatie."
                          ),
                          lang()$t(
                            " Laat het veld leeg om de parameter niet mee sturen."
                          )
                        )
                      )
                    )
                  ),
                  selectInput(
                    inputId = session$ns(paste0(id_prefix, "_verbosity")),
                    label = NULL,
                    choices = verbosity_choices,
                    selected = selected_verbosity
                  )
                ),

                # Temperature
                div(
                  class = "d-flex flex-column",
                  tags$label(
                    class = "form-label",
                    tagList(
                      lang()$t("Temperature"),
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        paste0(
                          lang()$t(
                            "Stel de sampling-temperatuur in (0–2, hoger = creatiever). "
                          ),
                          lang()$t(
                            "Laat leeg om de provider-standaard te gebruiken."
                          ),
                          lang()$t(
                            " Het wordt afgeraden om deze parameter samen met een 'top p'-parameter te gebruiken."
                          )
                        )
                      )
                    )
                  ),
                  numericInput(
                    inputId = session$ns(paste0(id_prefix, "_temperature")),
                    label = NULL,
                    value = selected_temperature,
                    min = 0,
                    max = 2,
                    step = 0.1
                  )
                ),

                # Top-p
                div(
                  class = "d-flex flex-column",
                  tags$label(
                    class = "form-label",
                    tagList(
                      lang()$t("Top-p (nucleus sampling)"),
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        paste0(
                          lang()$t(
                            "Beperk sampling tot de kleinste token set met cumulatieve kans p (0–1). "
                          ),
                          lang()$t(
                            "Laat leeg om de provider-standaard te gebruiken."
                          ),
                          lang()$t(
                            " Het wordt afgeraden om deze parameter samen met een 'temperature'-parameter te gebruiken."
                          )
                        )
                      )
                    )
                  ),
                  numericInput(
                    inputId = session$ns(paste0(id_prefix, "_top_p")),
                    label = NULL,
                    value = selected_top_p,
                    min = 0,
                    max = 1,
                    step = 0.01
                  )
                ),

                # JSON mode
                div(
                  class = "d-flex flex-column",
                  tags$label(
                    class = "form-label",
                    tagList(
                      lang()$t("JSON-modus"),
                      bslib::tooltip(
                        bsicons::bs_icon("info-circle"),
                        paste0(
                          lang()$t(
                            "Sommige prompts tijdens de analyse vragen om JSON-output (structured output)."
                          ),
                          lang()$t(
                            " Voor OpenAI-type API's & Ollama, wordt dit automatisch gedaan via specifieke parameters in de API-requests."
                          ),
                          lang()$t(
                            " Een vereiste is dan wel dat het model deze structured-output-parameters ondersteunt (de meeste moderne modellen doen dit)."
                          ),
                          lang()$t(
                            " Maar als het model dit niet ondersteunt, kan dit ('auto') tot errors leiden."
                          ),
                          lang()$t(
                            " In dat geval kan je hier 'text-based' kiezen, wat werkt voor alle modellen."
                          )
                        )
                      )
                    )
                  ),
                  selectInput(
                    inputId = session$ns(paste0(id_prefix, "_json_mode")),
                    label = NULL,
                    choices = c("auto", "text-based"),
                    selected = selected_json_type
                  )
                )
              ),

              tags$hr(),
              h6(lang()$t("Huidige parameters:")),
              verbatimTextOutput(session$ns(paste0(
                which,
                "_provider_preview"
              ))),

              tags$hr(),
              div(
                style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 10px;",
                actionButton(
                  label = lang()$t("Test provider (regular output)"),
                  inputId = ns(paste0(which, "_test_provider")),
                  class = "btn btn-primary"
                ),
                actionButton(
                  label = lang()$t("Test provider (JSON output)"),
                  inputId = ns(paste0(which, "_test_provider_json")),
                  class = "btn btn-primary"
                )
              )
            )
          )
        )
      }

      # Function to test LLM provider + listen for test button click
      do_test <- function(which, use_json = FALSE) {
        provider <- if (which == "main") models$main else models$large
        req(provider)

        shinyjs::disable(paste0(which, "_test_provider"))
        shinyjs::disable(paste0(which, "_test_provider_json"))

        nid <- paste0(which, if (use_json) "_test_json" else "_test")
        showNotification(
          "Testing provider...",
          type = "message",
          duration = NULL,
          id = nid
        )

        promises::future_promise(
          {
            if (use_json) {
              json_schema <- list(
                name = "thought_steps",
                description = NULL,
                schema = list(
                  type = "object",
                  properties = list(
                    steps = list(type = "array", items = list(type = "string")),
                    final_answer = list(type = "string")
                  ),
                  required = c("steps", "final_answer"),
                  additionalProperties = FALSE
                )
              )

              "Hi!?" %>%
                tidyprompt::answer_as_json(
                  schema = json_schema,
                  type = "auto"
                ) %>%
                tidyprompt::send_prompt(provider) %>%
                jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)
            } else {
              provider$complete_chat("Hi!") %>%
                .$completed %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::pull(content)
            }
          },
          globals = list(provider = provider, use_json = use_json),
          packages = c("tidyprompt", "dplyr", "jsonlite", "magrittr")
        ) %...>%
          (function(res) {
            removeNotification(nid)
            showNotification(
              paste0("Provider responded (success):\n'", res, "'"),
              type = "message",
              duration = 5
            )
            shinyjs::enable(paste0(which, "_test_provider"))
            shinyjs::enable(paste0(which, "_test_provider_json"))
          }) %...!%
          (function(e) {
            removeNotification(nid)
            app_error(
              e,
              when = "sending test message to provider",
              fatal = FALSE,
              lang = lang()
            )
            shinyjs::enable(paste0(which, "_test_provider"))
            shinyjs::enable(paste0(which, "_test_provider_json"))
          })

        invisible(NULL)
      }
      # Wire test buttons to listeners
      for (which in c("main", "large")) {
        local({
          w <- which

          observeEvent(
            input[[paste0(w, "_test_provider")]],
            ignoreInit = TRUE,
            {
              do_test(w, use_json = FALSE)
            }
          )

          observeEvent(
            input[[paste0(w, "_test_provider_json")]],
            ignoreInit = TRUE,
            {
              do_test(w, use_json = TRUE)
            }
          )
        })
      }

      # Open modals
      observeEvent(input$main_cog, ignoreInit = TRUE, {
        open_settings_modal("main")
      })
      observeEvent(input$large_cog, ignoreInit = TRUE, {
        open_settings_modal("large")
      })

      # Render JSON preview of provider fields/parameters ----------------------

      get_llm_provider_fields <- function(llm_provider) {
        # Get parameters from the LLM provider
        p <- llm_provider$parameters
        if (is.null(p)) {
          p <- list()
        }

        # Also get JSON mode, URL; add to separate list
        fields <- list(
          json_type = llm_provider$json_type,
          url = llm_provider$url,
          parameters = p
        )

        return(fields)
      }
      main_provider_updated <- reactiveVal(NULL)
      output$main_provider_preview <- renderText({
        main_provider_updated()
        jsonlite::toJSON(
          get_llm_provider_fields(models$main),
          pretty = TRUE,
          auto_unbox = TRUE,
          null = "null"
        )
      })
      large_provider_updated <- reactiveVal(NULL)
      output$large_provider_preview <- renderText({
        large_provider_updated()
        jsonlite::toJSON(
          get_llm_provider_fields(models$large),
          pretty = TRUE,
          auto_unbox = TRUE,
          null = "null"
        )
      })

      # Live update — JSON mode (MAIN)
      observeEvent(input$main_json_mode, ignoreInit = TRUE, {
        req(models$main)
        prov <- models$main$clone(deep = TRUE)
        prov$json_type <- input$main_json_mode
        models$main <- prov
        main_provider_updated(Sys.time())
        update_json_mode_ui("main")
        update_temperature_ui(session, input, models, "main")
        update_top_p_ui(session, input, models, "main")
      })

      # Live update — JSON mode (LARGE)
      observeEvent(input$large_json_mode, ignoreInit = TRUE, {
        req(models$large)
        prov <- models$large$clone(deep = TRUE)
        prov$json_type <- input$large_json_mode
        models$large <- prov
        large_provider_updated(Sys.time())
        update_json_mode_ui("large")
        update_temperature_ui(session, input, models, "large")
        update_top_p_ui(session, input, models, "large")
      })

      # Update the JSON mode select input in the modal
      update_json_mode_ui <- function(which = c("main", "large")) {
        which <- match.arg(which)
        input_id <- paste0(which, "_json_mode")
        # Only update if the modal control exists
        if (!is.null(input[[input_id]])) {
          provider <- if (which == "main") models$main else models$large
          if (!is.null(provider)) {
            sel <- provider$json_type
            if (is.null(sel) || !nzchar(sel)) {
              sel <- "auto"
            }
            updateSelectInput(session, input_id, selected = sel)
          }
        }
      }

      # Disable inputs upon processing -----------------------------------------

      observe({
        req(mode())
        if (processing()) {
          shinyjs::disable("main_model")
          shinyjs::disable("main_cog")
          if (mode() == "Onderwerpextractie") {
            shinyjs::disable("large_model")
            shinyjs::disable("large_cog")
          }
        } else {
          shinyjs::enable("main_model")
          shinyjs::enable("main_cog")
          if (mode() == "Onderwerpextractie") {
            shinyjs::enable("large_model")
            shinyjs::enable("large_cog")
          }
        }
      })

      # Wire the DRY observers for settings -----------------------------------

      lapply(c("main", "large"), function(w) {
        wire_settings_observer(
          which = w,
          input = input,
          session = session,
          models = models,
          main_provider_updated = main_provider_updated,
          large_provider_updated = large_provider_updated
        )
      })

      # Return reactive values -------------------------------------------------
      return(models)
    }
  )
}


# 3 Helpers --------------------------------------------------------
# Helper to add names to preconfigured llm_provider_model list
# If item already has a name in the list, keep that
# If unnamed, use the model name as the name
add_names_to_llm_provider_model_list <- function(
  llm_provider_model_list = list(
    "gpt 4.1 mini" = tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    ))
  )
) {
  if (length(llm_provider_model_list) == 0) {
    return(llm_provider_model_list)
  }

  nm <- names(llm_provider_model_list)

  for (i in seq_along(llm_provider_model_list)) {
    # Handle NULL/NA/""
    if (is.null(nm) || is.na(nm[i]) || nm[i] == "") {
      model_name <- llm_provider_model_list[[i]]$parameters$model
      nm[i] <- model_name
    }
  }

  names(llm_provider_model_list) <- nm
  return(llm_provider_model_list)
}


# Helper to verify that names of model list are unique
verify_unique_llm_provider_model_names <- function(
  llm_provider_model_list = list(
    "gpt 4.1 mini" = tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    ))
  )
) {
  # Check if the list is empty
  if (length(llm_provider_model_list) == 0) {
    return(TRUE)
  }

  # Get the names of the llm_provider_model_list
  llm_provider_model_names <- names(llm_provider_model_list)

  # Check for duplicates
  if (
    length(llm_provider_model_names) != length(unique(llm_provider_model_names))
  ) {
    stop(
      "Names in llm_provider_model_list are not unique: ",
      paste(llm_provider_model_names, collapse = ", ")
    )
  }

  return(invisible(TRUE))
}


# 4 Example/development usage --------------------------------------
if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)

  source("R/module_config_llm_provider.R")

  shiny::addResourcePath("www", "www")

  ui <- bslib::page_fluid(
    css_js_head(),
    shinyjs::useShinyjs(),
    language_ui("language"),
    llm_provider_ui("llm_provider"),
    model_ui("model"),
    tagList(
      textOutput("main_model"),
      textOutput("large_model")
    )
  )

  server <- function(input, output, session) {
    lang <- language_server("language", processing = reactiveVal(FALSE))

    llm_provider <- llm_provider_server(
      "llm_provider",
      lang = lang,
      processing = reactiveVal(FALSE),
      has_preconfigured_llm_provider = TRUE,
      can_configure_oai = TRUE,
      can_configure_ollama = TRUE
    )

    model <- model_server(
      "model",
      lang = lang,
      llm_provider_rv = llm_provider
    )

    output$main_model <- renderText({
      paste("Primair model:", model$main$parameters$model)
    })

    output$large_model <- renderText({
      paste("Groot model:", model$large$parameters$model)
    })
  }

  shinyApp(ui = ui, server = server)
}
