# Module for selecting a large language model (LLM) provider and configuring it
# This module displays the preconfigured LLM provider or allows the user to
#   configure a new one (OpenAI-API compatible API or Ollama)

#### 1 UI ####

llm_provider_ui <- function(
  id
) {
  ns <- NS(id)
  uiOutput(ns("llm_provider_card"))
}


#### 2 Server ####

llm_provider_server <- function(
  id,
  processing = reactiveVal(FALSE),
  preconfigured_llm_provider = NULL,
  preconfigured_main_models = NULL,
  preconfigured_large_models = NULL,
  can_configure_oai = getOption("llm_provider__can_configure_oai", TRUE),
  can_configure_ollama = getOption("llm_provider__can_configure_ollama", TRUE),
  lang = reactiveVal(
    shiny.i18n::Translator$new(
      translation_json_path = "language/language.json"
    )
  )
) {
  if (
    is.null(preconfigured_llm_provider) &
      !can_configure_oai &
      !can_configure_ollama
  ) {
    stop(paste0(
      "At least one LLM provider must be preconfigured or configurable.",
      " Set `preconfigured_llm_provider` to a valid LLM provider,",
      " or set `can_configure_oai` or `can_configure_ollama` to TRUE."
    ))
  }

  if (
    !is.null(preconfigured_llm_provider) &&
      (length(preconfigured_main_models) == 0 ||
        length(preconfigured_large_models) == 0)
  ) {
    stop(
      "You must provide at least one main and one large model",
      " when using a preconfigured LLM provider."
    )
  }

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Decide to render the card; only render if can at least configure Ollama or OpenAI
      output$llm_provider_card <- renderUI({
        req(isTRUE(can_configure_oai) || isTRUE(can_configure_ollama))

        tagList(
          tags$style(HTML(
            "
            .llm-icon {
              padding: 2px;
              border-radius: 2px;
              transition: all 0.2s ease;
              cursor: pointer;
            }

            .llm-icon:hover {
              background-color: #f0f0f0;
              box-shadow: 0 0 5px rgba(0,0,0,0.15);
              transform: scale(1.05);
            }

            .llm-icon-active {
              background-color: #f0f0f0;
              box-shadow: 0 0 5px rgba(0,0,0,0.15);
              transform: scale(1.05);
              cursor: default;
            }

            .llm-narrow-container {
              max-width: 500px;
              width: 100%;
              margin: 0 auto;
              display: flex;
              flex-direction: column;
              align-items: center;
            }

            .llm-narrow-container .shiny-input-container.form-group {
              margin-bottom: 0.25rem !important;
              text-align: center; /* Center the label text */
              width: 100%;
            }

            /* Specifically reduce margin from Shiny textInput containers */
            .llm-narrow-container .shiny-input-container.form-group {
              margin-bottom: 0.25rem !important;
            }

            .llm-narrow-container .input-group {
              width: 100%;
            }

            .llm-description-box {
              max-width: 500px;
              margin: 0 auto;
              text-align: center;
            }

            .llm-narrow-container .btn,
            .llm-narrow-container .action-button,
            .llm-narrow-container .input-group,
            .llm-narrow-container .form-control {
              width: 100% !important;
              max-width: 100%;
            }

            .llm-narrow-container .btn {
              text-align: center;
            }

            .llm-narrow-container a {
              word-break: normal;
              overflow-wrap: normal;
              white-space: nowrap;
            }
            "
          )),

          bslib::card(
            class = "card",
            card_header(
              div(
                class = "d-flex justify-content-between align-items-center w-100",
                span(lang()$t("LLM-provider")),
                uiOutput(ns("provider_mode_selection")),
              )
            ),
            card_body(
              div(
                class = "d-flex flex-column align-items-center",
                uiOutput(ns("mode_description")),
                if (llm_provider_rv$provider_mode != "preconfigured") {
                  tagList(
                    div(class = "mb-2 w-100", uiOutput(ns("url_input"))),
                    div(class = "mb-2 w-100", uiOutput(ns("api_key_input"))),
                    div(class = "mb-2 w-100", uiOutput(ns("models_output")))
                  )
                }
              )
            )
          )
        )
      })

      # Default URLs and state
      openai_url <- reactiveVal("https://api.openai.com/v1")
      ollama_url <- reactiveVal("http://localhost:11434/api")
      api_key_input <- reactiveVal(Sys.getenv("OPENAI_API_KEY"))
      available_models_openai <- reactiveVal(character(0))
      available_models_ollama <- reactiveVal(character(0))
      last_model_request_time <- reactiveVal(Sys.time() - 10) # initialize as 10 seconds ago

      initial_provider_mode <- if (!is.null(preconfigured_llm_provider)) {
        "preconfigured"
      } else if (can_configure_oai) {
        "openai"
      } else {
        "ollama"
      }

      initial_llm_provider <- if (initial_provider_mode == "preconfigured") {
        preconfigured_llm_provider$clone()
      } else if (initial_provider_mode == "openai") {
        tidyprompt::llm_provider_openai(
          parameters = list(model = "gpt-4o-mini", stream = FALSE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = "https://api.openai.com/v1/chat/completions",
          api_key = Sys.getenv("OPENAI_API_KEY")
        )
      } else {
        tidyprompt::llm_provider_ollama(
          parameters = list(model = "llama3.1:8b", stream = FALSE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = "http://localhost:11434/api/chat"
        )
      }

      # Reactive values to store the current LLM provider and mode
      llm_provider_rv <- reactiveValues(
        llm_provider = initial_llm_provider,
        provider_mode = initial_provider_mode,
        available_models_main = NULL,
        available_models_large = NULL
      )

      # Render the provider mode selection UI
      #   3 action icons for switching between modes, preconfigured, openai, and ollama
      #   Render according to can_configure_oai and can_configure_ollama
      output$provider_mode_selection <- renderUI({
        current_mode <- llm_provider_rv$provider_mode

        div(
          class = "d-flex justify-content-center gap-3",

          if (!is.null(preconfigured_llm_provider)) {
            div(
              id = ns("select_preconfigured"),
              class = paste(
                "llm-icon",
                if (current_mode == "preconfigured") "llm-icon-active"
              ),
              title = lang()$t("Pregeconfigureerd"),
              onclick = sprintf(
                "Shiny.setInputValue('%s', Math.random())",
                ns("select_preconfigured")
              ),
              bs_icon(
                "arrow-90deg-left",
                class = "llm-icon-img",
                style = "height: 20px;"
              )
            ) |>
              bslib::tooltip(
                lang()$t("Pregeconfigureerd"),
                placement = "bottom"
              )
          },

          if (can_configure_oai) {
            div(
              id = ns("select_openai"),
              class = paste(
                "llm-icon",
                if (current_mode == "openai") "llm-icon-active"
              ),
              title = "OpenAI",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Math.random())",
                ns("select_openai")
              ),
              tags$img(
                src = "www/openai_avatar.svg",
                height = "20",
                alt = "OpenAI"
              )
            ) |>
              bslib::tooltip(
                lang()$t("OpenAI-compatible"),
                placement = "bottom"
              )
          },

          if (can_configure_ollama) {
            div(
              id = ns("select_ollama"),
              class = paste(
                "llm-icon",
                if (current_mode == "ollama") "llm-icon-active"
              ),
              title = "Ollama",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Math.random())",
                ns("select_ollama")
              ),
              tags$img(
                src = "www/ollama_avatar.png",
                height = "20",
                alt = "Ollama"
              )
            ) |>
              bslib::tooltip("Ollama", placement = "bottom")
          }
        )
      })

      # Provider switching logic
      observeEvent(input$select_preconfigured, {
        req(!is.null(preconfigured_llm_provider))
        llm_provider_rv$provider_mode <- "preconfigured"
        llm_provider_rv$llm_provider <- preconfigured_llm_provider$clone()
      })

      observeEvent(input$select_openai, {
        req(can_configure_oai)
        llm_provider_rv$provider_mode <- "openai"
        llm_provider_rv$llm_provider <- tidyprompt::llm_provider_openai(
          parameters = list(model = "gpt-4o-mini", stream = FALSE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = paste0(
            openai_url(),
            "/chat/completions"
          ),
          api_key = api_key_input()
        )
      })

      observeEvent(input$select_ollama, {
        req(can_configure_ollama)
        llm_provider_rv$provider_mode <- "ollama"
        llm_provider_rv$llm_provider <- tidyprompt::llm_provider_ollama(
          parameters = list(model = "llama3.1:8b", stream = FALSE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = paste0(
            ollama_url(),
            "/chat"
          )
        )
      })

      output$mode_description <- renderUI({
        mode <- llm_provider_rv$provider_mode

        description_text <- switch(
          mode,
          "preconfigured" = lang()$t(
            "Je gebruikt nu een vooraf ingestelde LLM-API,<br>zoals vastgelegd in de appconfiguratie.<br>De URL en de beschikbare modellen zijn vooraf ingesteld."
          ),
          "openai" = lang()$t(
            "Configureer hier een OpenAI-compatibele API.<br>Dit soort API-endpoints worden niet alleen door OpenAI aangeboden,<br>maar ook door diverse andere providers.<br>Haal na het instellen de beschikbare modellen op met de button."
          ),
          "ollama" = lang()$t(
            "Configureer hier een Ollama-API.<br>Host bijvoorbeeld Ollama op je eigen systeem<br>(zie: https://ollama.com/).<br>Haal na het instellen de beschikbare modellen op met de button."
          ),
          ""
        )

        div(
          class = "llm-narrow-container",
          style = "
            margin: 10px auto 15px auto;
            padding: 15px 20px;
            background-color: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 5px;
            font-size: 0.9em;
            color: #495057;
            text-align: center;
            word-break: normal;
            overflow-wrap: normal;
          ",
          HTML(description_text)
        )
      })

      # Custom inputs
      # Reactively update URLs and API key
      observeEvent(input$openai_url, openai_url(input$openai_url))
      observeEvent(input$ollama_url, ollama_url(input$ollama_url))
      observeEvent(input$api_key_text, api_key_input(input$api_key_text))

      # UI Inputs based on mode
      output$url_input <- renderUI({
        if (llm_provider_rv$provider_mode == "openai") {
          return(div(
            class = "llm-narrow-container mb-1",
            textInput(
              ns("openai_url"),
              lang()$t("OpenAI-API-compatible endpoint URL:"),
              value = openai_url(),
              width = "100%"
            )
          ))
        }
        if (llm_provider_rv$provider_mode == "ollama") {
          return(div(
            class = "llm-narrow-container mb-1",
            textInput(
              ns("ollama_url"),
              lang()$t("Ollama-API endpoint URL:"),
              value = ollama_url(),
              width = "100%"
            )
          ))
        }
        return(NULL)
      })

      output$api_key_input <- renderUI({
        req(llm_provider_rv$provider_mode == "openai")

        ns_api <- ns("api_key_text")
        ns_btn <- ns("toggle_api_key_visibility")

        tagList(
          shinyjs::useShinyjs(),
          div(
            class = "llm-narrow-container mb-2",
            tags$div(
              class = "form-group mb-2 w-100",
              tags$label(
                `for` = ns_api,
                lang()$t("API-key:"),
                style = "display: block; width: 100%; text-align: center; margin-bottom: 0.5rem;"
              ),
              tags$div(
                class = "input-group w-100",
                tags$input(
                  id = ns_api,
                  type = "password",
                  class = "form-control",
                  value = api_key_input(),
                  style = "width: 100%;" # Ensure full width inside the input group
                ),
                tags$button(
                  id = ns_btn,
                  type = "button",
                  class = "btn btn-outline-secondary",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', Math.random())",
                    ns_btn
                  ),
                  `data-state` = "hidden",
                  bs_icon("eye-slash", id = ns("eye_icon"))
                )
              )
            )
          ),
          tags$script(HTML(sprintf(
            "
            Shiny.addCustomMessageHandler('%s-togglePassword', function(id) {
              var input = document.getElementById(id);
              var icon = document.getElementById('%s');

              if (input.type === 'password') {
                input.type = 'text';
                icon.classList.remove('bi-eye-slash');
                icon.classList.add('bi-eye');
              } else {
                input.type = 'password';
                icon.classList.remove('bi-eye');
                icon.classList.add('bi-eye-slash');
              }
            });
            ",
            ns_api,
            ns("eye_icon")
          )))
        )
      })

      observeEvent(api_key_input(), {
        if (llm_provider_rv$provider_mode == "openai") {
          llm_provider_rv$llm_provider <- tidyprompt::llm_provider_openai(
            parameters = list(model = "gpt-4o-mini", stream = FALSE),
            verbose = getOption("tidyprompt.verbose", TRUE),
            url = paste0(openai_url(), "/chat/completions"),
            api_key = api_key_input()
          )
        }
      })

      observeEvent(input$toggle_api_key_visibility, {
        session$sendCustomMessage(
          type = paste0(ns("api_key_text"), "-togglePassword"),
          message = paste0(ns("api_key_text"))
        )
      })

      # Button to trigger model fetch
      output$models_output <- renderUI({
        req(llm_provider_rv$provider_mode %in% c("openai", "ollama"))
        div(
          class = "llm-narrow-container mb-1",
          actionButton(
            ns("get_models"),
            lang()$t("Ping beschikbare modellen"),
            class = "btn-primary"
          )
        )
      })

      # Model fetching on button click
      observeEvent(input$get_models, {
        now <- Sys.time()
        if (difftime(now, last_model_request_time(), units = "secs") < 5) {
          showNotification(
            lang()$t(
              "Wacht even voordat je het opnieuw probeert (min. 5 seconden tussen aanvragen)"
            ),
            type = "warning"
          )
          return(NULL)
        }
        last_model_request_time(now)
        shinyjs::disable("get_models")
        showNotification(
          lang()$t("Modellen ophalen..."),
          type = "default",
          duration = 3
        )

        provider_mode <- llm_provider_rv$provider_mode

        future(
          {
            if (provider_mode == "openai") {
              res <- httr::GET(
                paste0(openai_url, "/models"),
                httr::add_headers(
                  Authorization = paste("Bearer", api_key_input)
                )
              )
              if (httr::http_error(res)) {
                stop(sprintf(
                  "Error (%s): %s",
                  httr::status_code(res),
                  httr::content(res, as = "text", encoding = "UTF-8")
                ))
              }
              httr::content(res)$data |> purrr::map_chr("id")
            } else if (provider_mode == "ollama") {
              res <- httr::GET(
                url = paste0(ollama_url, "/tags"),
                httr::add_headers(`Content-Type` = "application/json")
              )
              if (httr::http_error(res)) {
                stop(sprintf(
                  "Error (%s): %s",
                  httr::status_code(res),
                  httr::content(res, as = "text", encoding = "UTF-8")
                ))
              }
              content <- httr::content(
                res,
                as = "parsed",
                type = "application/json"
              )
              vapply(content$models, function(x) x$name, character(1))
            } else {
              character(0)
            }
          },
          globals = list(
            openai_url = openai_url(),
            api_key_input = api_key_input(),
            ollama_url = ollama_url(),
            provider_mode = provider_mode
          )
        ) %...>%
          (function(models) {
            if (provider_mode == "openai") {
              available_models_openai(models)
            } else if (provider_mode == "ollama") {
              available_models_ollama(models)
            }

            showNotification(
              lang()$t("Succes: modellen opgehaald"),
              type = "message",
              duration = 3
            )
          }) %...!%
          (function(e) {
            showNotification(
              paste(
                lang()$t("Error: modellen niet opgehaald -"),
                conditionMessage(e)
              ),
              type = "error",
              duration = 8
            )
          }) %>%
          finally(function() {
            shinyjs::enable("get_models")
          })
      })

      # Set available models based on mode
      observe({
        provider_mode <- llm_provider_rv$provider_mode
        if (provider_mode == "openai") {
          llm_provider_rv$available_models_main <- available_models_openai()
          llm_provider_rv$available_models_large <- available_models_openai()
        } else if (provider_mode == "ollama") {
          llm_provider_rv$available_models_main <- available_models_ollama()
          llm_provider_rv$available_models_large <- available_models_ollama()
        } else if (provider_mode == "preconfigured") {
          # Use preconfigured models if available
          llm_provider_rv$available_models_main <- preconfigured_main_models
          llm_provider_rv$available_models_large <- preconfigured_large_models
        } else {
          llm_provider_rv$available_models_main <- character(0)
          llm_provider_rv$available_models_large <- character(0)
        }
      })

      # output$model_dropdown <- renderUI({
      #   provider_mode <- llm_provider_rv$provider_mode
      #   models <- if (provider_mode == "openai") {
      #     available_models_openai()
      #   } else if (provider_mode == "ollama") {
      #     available_models_ollama()
      #   } else {
      #     character(0)
      #   }
      #
      #   if (length(models)) {
      #     selectInput(
      #       ns("available_models"),
      #       "Beschikbare modellen",
      #       choices = models
      #     )
      #   } else {
      #     helpText("...")
      #   }
      # })

      # Disable/enable inputs when processing status changes
      observe({
        if (isTRUE(processing())) {
          shinyjs::disable("openai_url")
          shinyjs::disable("ollama_url")
          shinyjs::disable("api_key_text")
          shinyjs::disable("get_models")
          shinyjs::disable("select_preconfigured")
          shinyjs::disable("select_openai")
          shinyjs::disable("select_ollama")
        }
      })

      return(llm_provider_rv)
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(bsicons)

  # Make images in www folder available to the app
  shiny::addResourcePath("www", "www")

  ui <- bslib::page_fluid(
    css_js_head(),
    shinyjs::useShinyjs(),
    llm_provider_ui("llm_provider")
  )

  server <- function(input, output, session) {
    processing <- reactiveVal(FALSE)

    llm_provider_rv <- llm_provider_server(
      "llm_provider",
      processing
      # preconfigured_llm_provider = tidyprompt::llm_provider_openai(),
      # preconfigured_main_models = c("gpt-4o-mini", "gpt-3.5-turbo"),
      # preconfigured_large_models = c("gpt-4o", "o3")
    )
  }

  shinyApp(ui = ui, server = server)
}
