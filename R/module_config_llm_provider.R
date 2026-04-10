# Module for selecting a large language model (LLM) provider and configuring it
# This module displays the preconfigured LLM provider or allows the user to
#   configure a new one (OpenAI-API compatible API or Ollama)

# 1 UI ---------------------------------------------------------------
llm_provider_ui <- function(
  id
) {
  ns <- NS(id)
  uiOutput(ns("llm_provider_card"))
}


# 2 Server ---------------------------------------------------------
llm_provider_server <- function(
  id,
  processing = reactiveVal(FALSE),
  has_preconfigured_llm_provider = TRUE,
  can_configure_oai = getOption("llm_provider__can_configure_oai", TRUE),
  can_configure_ollama = getOption("llm_provider__can_configure_ollama", TRUE),
  lang = default_lang()
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Main card UI -----------------------------------------------------------

      # Decide to render the card; render if there is a preconfigured provider
      #   or if the user can configure at least one provider
      output$llm_provider_card <- renderUI({
        req(
          isTRUE(has_preconfigured_llm_provider) ||
            isTRUE(can_configure_oai) ||
            isTRUE(can_configure_ollama)
        )

        tagList(
          tags$style(HTML(
            "
            .llm-icon {
              padding: 2px;
              border: none;
              background: transparent;
              outline: none;
              appearance: none;
              border-radius: 2px;
              transition: all 0.2s ease;
              cursor: pointer;
            }

            .llm-icon:hover:not(:disabled) {
              background-color: #f0f0f0;
              box-shadow: 0 0 5px rgba(0,0,0,0.15);
              transform: scale(1.05);
            }

            .llm-icon:disabled {
              opacity: 0.4;
              cursor: not-allowed;
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
                span(
                  lang()$t("LLM-provider"),
                  tooltip(
                    bsicons::bs_icon("info-circle"),
                    paste0(
                      lang()$t(
                        "Hier staan details over de geconfigureerde LLM-provider.",
                      ),
                      lang()$t(
                        " Een LLM-provider is een API die toegang biedt tot een taalmodel (LLM). Dit kan een lokale API of externe API zijn."
                      ),
                      lang()$t(
                        " Er kan hier een vooraf geconfigureerde LLM-provider gebruikt worden, of er kan tijdens gebruik van de app een verbinding gelegd worden met een OpenAI-compatible API of Ollama."
                      )
                    )
                  )
                ),
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

      # Reactive values --------------------------------------------------------

      # Reactive values to store the current LLM provider and mode;
      #   these values will be passed on to the 'model' module
      # In preconfigured mode, LLM provider will be NULL; one will
      #   be chosen in the model module
      # In Ollama/OpenAI mode, llm_provider will be built here and passed
      #   on to the model module for further configuration
      #   In this module, we ping the API to get the available models
      #   and pass those on to the model module

      # Default URLs and state
      openai_url <- reactiveVal(getOption(
        "llm_provider__default_oai_url",
        "https://api.openai.com/v1"
      ))
      ollama_url <- reactiveVal(getOption(
        "llm_provider__default_ollama_url",
        "http://localhost:11434/api/chat"
      ))

      initial_provider_mode <- if (has_preconfigured_llm_provider) {
        "preconfigured"
      } else if (can_configure_oai) {
        "openai"
      } else {
        "ollama"
      }

      initial_llm_provider <- if (initial_provider_mode == "preconfigured") {
        NULL
      } else if (initial_provider_mode == "openai") {
        tidyprompt::llm_provider_openai(
          parameters = list(model = "gpt-4o-mini", stream = TRUE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = paste0(
            getOption(
              "llm_provider__default_oai_url",
              "https://api.openai.com/v1/chat/completions"
            )
          ),
          api_key = Sys.getenv("OPENAI_API_KEY")
        )
      } else {
        tidyprompt::llm_provider_ollama(
          parameters = list(model = "llama3.1:8b", stream = TRUE),
          verbose = getOption("tidyprompt.verbose", TRUE),
          url = paste0(
            getOption(
              "llm_provider__default_ollama_url",
              "http://localhost:11434/api/chat"
            )
          )
        )
      }

      llm_provider_rv <- reactiveValues(
        llm_provider_configured = initial_llm_provider,
        provider_mode = initial_provider_mode,
        configured_models = NULL
      )

      # Provider mode selection UI & listeners ---------------------------------

      # Render the provider mode selection UI
      #   3 action icons for switching between modes, preconfigured, openai, and ollama
      #   Render according to can_configure_oai and can_configure_ollama

      output$provider_mode_selection <- renderUI({
        current_mode <- llm_provider_rv$provider_mode

        div(
          class = "d-flex justify-content-center gap-3",

          if (has_preconfigured_llm_provider) {
            icon_toggle_button(
              ns = ns,
              id_suffix = "preconfigured",
              icon_name = "arrow-90deg-left",
              title = lang()$t("Pregeconfigureerd"),
              tooltip_text = lang()$t("Pregeconfigureerd"),
              is_active = (current_mode == "preconfigured"),
              css_prefix = "llm-icon"
            )
          },

          if (can_configure_oai) {
            icon_toggle_button(
              ns = ns,
              id_suffix = "openai",
              img_src = "www/openai_avatar.svg",
              title = "OpenAI",
              tooltip_text = lang()$t("OpenAI-compatible"),
              is_active = (current_mode == "openai"),
              css_prefix = "llm-icon"
            )
          },

          if (can_configure_ollama) {
            icon_toggle_button(
              ns = ns,
              id_suffix = "ollama",
              img_src = "www/ollama_avatar.png",
              title = "Ollama",
              tooltip_text = "Ollama",
              is_active = (current_mode == "ollama"),
              css_prefix = "llm-icon"
            )
          }
        )
      })

      # Provider switching logic
      observeEvent(input$select_preconfigured, {
        req(has_preconfigured_llm_provider)
        req(!isTRUE(processing()))
        llm_provider_rv$provider_mode <- "preconfigured"
        log_action("llm_provider_changed", details = "preconfigured")
      })
      observeEvent(input$select_openai, {
        req(can_configure_oai)
        req(!isTRUE(processing()))
        llm_provider_rv$provider_mode <- "openai"
        log_action("llm_provider_changed", details = "openai")
      })

      observeEvent(input$select_ollama, {
        req(can_configure_ollama)
        req(!isTRUE(processing()))
        llm_provider_rv$provider_mode <- "ollama"
        log_action("llm_provider_changed", details = "ollama")
      })
      observe({
        req(llm_provider_rv$provider_mode)
        req(!isTRUE(processing()))

        if (llm_provider_rv$provider_mode == "preconfigured") {
          llm_provider_rv$llm_provider_configured <- NULL
        } else if (llm_provider_rv$provider_mode == "openai") {
          req(isTRUE(can_configure_oai))
          req(api_key_input())
          req(openai_url())

          llm_provider_rv$llm_provider_configured <- tidyprompt::llm_provider_openai(
            parameters = list(model = "gpt-4o-mini", stream = TRUE),
            verbose = getOption("tidyprompt.verbose", TRUE),
            url = paste0(
              openai_url()
            ),
            api_key = api_key_input()
          )
        } else if (llm_provider_rv$provider_mode == "ollama") {
          req(isTRUE(can_configure_ollama))
          req(ollama_url())

          llm_provider_rv$llm_provider_configured <- tidyprompt::llm_provider_ollama(
            parameters = list(model = "llama3.1:8b", stream = TRUE),
            verbose = getOption("tidyprompt.verbose", TRUE),
            url = paste0(
              ollama_url()
            )
          )
        }
      })

      # UI Inputs based on mode
      output$url_input <- renderUI({
        if (llm_provider_rv$provider_mode == "openai") {
          return(div(
            class = "llm-narrow-container mb-1",
            textInput(
              ns("openai_url"),
              label = span(
                lang()$t("OpenAI-API-compatible endpoint URL:"),
                tooltip(
                  bsicons::bs_icon("info-circle"),
                  paste0(
                    lang()$t(
                      "Dit is de URL van de OpenAI-compatibele API om te gebruiken."
                    ),
                    lang()$t(
                      " Je kan hier zowel de het chat completions endpoint (/chat/completions) als het responses endpoint (/responses) gebruiken."
                    ),
                    lang()$t(
                      " Het responses endpoint biedt soms meer mogelijkheden (bijv., 'reasoning effort' parameter), maar wordt niet altijd ondersteund."
                    ),
                    lang()$t(
                      " Raadpleeg de documentatie van je LLM-provider."
                    )
                  )
                )
              ),
              value = isolate(openai_url()),
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
              value = isolate(ollama_url()),
              width = "100%"
            )
          ))
        }
        return(NULL)
      })

      # Keep URL reactiveVals in sync with the text inputs so the
      # configured provider updates immediately, not only on Ping.
      observeEvent(
        input$openai_url,
        {
          openai_url(input$openai_url)
        },
        ignoreInit = TRUE
      )
      observeEvent(
        input$ollama_url,
        {
          ollama_url(input$ollama_url)
        },
        ignoreInit = TRUE
      )

      # Mode description UI ----------------------------------------------------

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

        description_box(description_text, use_html = TRUE)
      })

      # API key  ---------------------------------------------------------------

      env_api_key <- Sys.getenv("OPENAI_API_KEY")
      # Server-side effective key; never rendered into the browser DOM.
      api_key_input <- reactiveVal(env_api_key)
      prev_api_key_has_value <- reactiveVal(nchar(env_api_key %||% "") > 0)

      # Track whether the user has explicitly entered a key so the initial
      # empty-string from the rendered input does not overwrite the env var.
      user_entered_api_key <- reactiveVal(FALSE)

      # Reactively update API key (updating URLs only when 'get models' is clicked)
      observeEvent(input$api_key_text, {
        if (user_entered_api_key() || nchar(input$api_key_text %||% "") > 0) {
          user_entered_api_key(TRUE)
          api_key_input(input$api_key_text)
        }
      })

      # Log set/cleared transitions (avoid per-keystroke logging)
      observeEvent(
        api_key_input(),
        {
          api_key_len <- nchar(api_key_input() %||% "")
          has_value <- api_key_len > 0

          # Ignore initialization; only record transitions after first user interaction
          prev_has_value <- prev_api_key_has_value()
          if (isTRUE(has_value != prev_has_value)) {
            if (isTRUE(has_value)) {
              log_action(
                "api_key_set",
                details = sprintf(
                  "provider=%s has_key=%s key_len=%d",
                  llm_provider_rv$provider_mode %||% "unknown",
                  has_value,
                  api_key_len
                )
              )
            } else {
              log_action(
                "api_key_cleared",
                details = sprintf(
                  "provider=%s has_key=%s",
                  llm_provider_rv$provider_mode %||% "unknown",
                  has_value
                )
              )
            }
            prev_api_key_has_value(has_value)
          }
        },
        ignoreInit = TRUE
      )

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
                  value = "",
                  placeholder = if (nchar(env_api_key) > 0) {
                    "\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022 (env)"
                  } else {
                    ""
                  },
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
                  bsicons::bs_icon("eye-slash", id = ns("eye_icon"))
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
          llm_provider_rv$llm_provider_configured <- tidyprompt::llm_provider_openai(
            parameters = list(model = "gpt-4o-mini", stream = TRUE),
            verbose = getOption("tidyprompt.verbose", TRUE),
            url = paste0(openai_url()),
            api_key = api_key_input()
          )
        }
      })

      observeEvent(input$toggle_api_key_visibility, {
        log_action(
          "api_key_visibility_toggled",
          details = sprintf(
            "provider=%s",
            llm_provider_rv$provider_mode %||% "unknown"
          )
        )
        session$sendCustomMessage(
          type = paste0(ns("api_key_text"), "-togglePassword"),
          message = paste0(ns("api_key_text"))
        )
      })

      # Fetch available models -------------------------------------------------

      available_models_openai <- reactiveVal(NULL)
      available_models_ollama <- reactiveVal(NULL)

      # Expose model lists for automated tests (reactive, so they update)
      shiny::exportTestValues(
        available_models_openai = available_models_openai(),
        available_models_ollama = available_models_ollama()
      )

      # Keep track of requests for available models
      last_model_request_time <- reactiveVal(Sys.time() - 10)

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
        provider_mode <- llm_provider_rv$provider_mode

        if (provider_mode == "openai") {
          api_key_len <- nchar(api_key_input() %||% "")
          log_action(
            "api_key_used_for_models_ping",
            details = sprintf(
              "provider=openai has_key=%s key_len=%d",
              api_key_len > 0,
              api_key_len
            )
          )
        }

        log_action(
          "models_ping_clicked",
          details = sprintf("provider=%s", provider_mode %||% "unknown")
        )

        request_started_at <- Sys.time()

        # Disable button, set available models to empty, show notification
        shinyjs::disable("get_models")
        if (provider_mode == "openai") {
          available_models_openai(character(0))
        } else if (provider_mode == "ollama") {
          available_models_ollama(character(0))
        }
        showNotification(
          lang()$t("Modellen ophalen..."),
          type = "default",
          duration = 3
        )

        log_context <- log_context_capture(is_async = TRUE)

        mirai::mirai(
          {
            kwallm_worker_bootstrap(
              task = "llm_provider_models_fetch",
              app_root = app_root,
              worker_options = worker_options,
              log_context = log_context
            )

            # IMPORTANT: in mirai workers, do not let raw httr/curl error conditions
            # escape, because they can be non-serializable. Always return a simple list.

            safe_trim <- function(x, max_chars = 2000) {
              x <- as.character(x %||% "")
              if (nchar(x) > max_chars) {
                paste0(substr(x, 1, max_chars), "…")
              } else {
                x
              }
            }

            tryCatch(
              {
                if (provider_mode == "openai") {
                  # For OpenAI url, reduce the URL first to base URL
                  # e.g., "https://api.openai.com/v1"
                  # (remove everything after version; no trailing slash)
                  openai_base_url <- openai_url |>
                    stringr::str_replace("(.*?/v\\d+).*", "\\1") |>
                    stringr::str_remove("/+$")

                  request_url <- paste0(openai_base_url, "/models")

                  res <- httr::GET(
                    request_url,
                    httr::add_headers(
                      Authorization = paste("Bearer", api_key_input)
                    )
                  )

                  if (httr::http_error(res)) {
                    status <- httr::status_code(res)
                    body <- httr::content(res, as = "text", encoding = "UTF-8")
                    return(list(
                      ok = FALSE,
                      provider = "openai",
                      request_url = request_url,
                      status_code = status,
                      response_body = safe_trim(body),
                      error = sprintf("HTTP %s", status)
                    ))
                  }

                  models <- httr::content(res)$data |> purrr::map_chr("id")
                  return(list(
                    ok = TRUE,
                    provider = "openai",
                    request_url = request_url,
                    models = models
                  ))
                }

                if (provider_mode == "ollama") {
                  # Make base URL for Ollama too
                  # (also here: no trailing slash)
                  ollama_base_url <- ollama_url |>
                    stringr::str_replace("(.*?/api).*", "\\1") |>
                    stringr::str_remove("/+$")

                  request_url <- paste0(ollama_base_url, "/tags")

                  res <- httr::GET(
                    url = request_url,
                    httr::add_headers(`Content-Type` = "application/json")
                  )

                  if (httr::http_error(res)) {
                    status <- httr::status_code(res)
                    body <- httr::content(res, as = "text", encoding = "UTF-8")
                    return(list(
                      ok = FALSE,
                      provider = "ollama",
                      request_url = request_url,
                      status_code = status,
                      response_body = safe_trim(body),
                      error = sprintf("HTTP %s", status)
                    ))
                  }

                  content <- httr::content(
                    res,
                    as = "parsed",
                    type = "application/json"
                  )

                  models <- vapply(
                    content$models,
                    function(x) x$name,
                    character(1)
                  )
                  return(list(
                    ok = TRUE,
                    provider = "ollama",
                    request_url = request_url,
                    models = models
                  ))
                }

                list(
                  ok = FALSE,
                  provider = provider_mode %||% "unknown",
                  request_url = NA_character_,
                  error = "Unsupported provider mode"
                )
              },
              error = function(e) {
                # Network / TLS / DNS errors etc. (no HTTP response available)
                list(
                  ok = FALSE,
                  provider = provider_mode %||% "unknown",
                  request_url = NA_character_,
                  error = safe_trim(conditionMessage(e)),
                  error_class = paste(class(e), collapse = "|")
                )
              }
            )
          },
          .args = c(
            list(
              app_root = kwallm_worker_app_root(),
              worker_options = kwallm_worker_capture_options(),
              log_context = log_context,
              openai_url = openai_url(),
              api_key_input = api_key_input(),
              ollama_url = ollama_url(),
              provider_mode = provider_mode
            ),
            kwallm_worker_bootstrap_globals()
          )
        ) %...>%
          (function(result) {
            elapsed_ms <- as.integer(round(
              1000 *
                as.numeric(difftime(
                  Sys.time(),
                  request_started_at,
                  units = "secs"
                ))
            ))

            # Success path
            if (isTRUE(result$ok)) {
              models <- result$models %||% character(0)
              if (provider_mode == "openai") {
                available_models_openai(models)
              } else if (provider_mode == "ollama") {
                available_models_ollama(models)
              }

              log_action(
                "models_ping_succeeded",
                details = sprintf(
                  "provider=%s n_models=%d elapsed_ms=%d",
                  provider_mode %||% "unknown",
                  length(models),
                  elapsed_ms
                )
              )
              showNotification(
                lang()$t("Succes: modellen opgehaald"),
                type = "message",
                duration = 3
              )
              log_info(
                sprintf(
                  "Models fetched: provider=%s, n_models=%d",
                  provider_mode,
                  length(models)
                ),
                component = "llm"
              )
              return(NULL)
            }

            # Failure path with optional HTTP response details
            status_part <- if (
              !is.null(result$status_code) && !is.na(result$status_code)
            ) {
              paste0("HTTP ", result$status_code)
            } else {
              NULL
            }
            url_part <- if (
              !is.null(result$request_url) &&
                !is.na(result$request_url) &&
                nzchar(result$request_url)
            ) {
              paste0("URL: ", result$request_url)
            } else {
              NULL
            }
            body_part <- if (
              !is.null(result$response_body) && nzchar(result$response_body)
            ) {
              paste0("Response: ", result$response_body)
            } else {
              NULL
            }
            err_text <- result$error %||% "Unknown error"
            err_text_short <- substr(err_text, 1, 200)

            log_action(
              "models_ping_failed",
              details = sprintf(
                "provider=%s elapsed_ms=%d error=%s status=%s url=%s",
                provider_mode %||% "unknown",
                elapsed_ms,
                err_text_short,
                result$status_code %||% "",
                result$request_url %||% ""
              )
            )
            log_warn(
              sprintf(
                "Models fetch failed: provider=%s, error=%s",
                provider_mode,
                err_text_short
              ),
              component = "llm"
            )

            showNotification(
              paste(
                lang()$t("Error: modellen niet opgehaald -"),
                paste(
                  c(status_part, err_text, url_part, body_part),
                  collapse = " | "
                )
              ),
              type = "error",
              duration = 12
            )
          }) %...!%
          (function(e) {
            # This should be rare now; keep as a last-resort fallback.
            elapsed_ms <- as.integer(round(
              1000 *
                as.numeric(difftime(
                  Sys.time(),
                  request_started_at,
                  units = "secs"
                ))
            ))
            err_msg <- tryCatch(conditionMessage(e), error = function(.) {
              as.character(e)
            })
            err_msg <- substr(err_msg, 1, 200)
            err_class <- paste(class(e), collapse = "|")
            log_action(
              "models_ping_failed",
              details = sprintf(
                "provider=%s elapsed_ms=%d error_class=%s error=%s",
                provider_mode %||% "unknown",
                elapsed_ms,
                err_class,
                err_msg
              )
            )
            showNotification(
              paste(
                lang()$t("Error: modellen niet opgehaald -"),
                err_msg
              ),
              type = "error",
              duration = 12
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
          llm_provider_rv$configured_models <- available_models_openai()
        } else if (provider_mode == "ollama") {
          llm_provider_rv$configured_models <- available_models_ollama()
        } else {
          llm_provider_rv$configured_models <- character(0)
        }
      })

      # Disable inputs when processing -----------------------------------------

      disable_when_processing(
        processing,
        c(
          "openai_url",
          "ollama_url",
          "api_key_text",
          "get_models",
          "select_preconfigured",
          "select_openai",
          "select_ollama"
        )
      )

      # Return reactive values --------------------------------------------------

      return(llm_provider_rv)
    }
  )
}


# 3 Example/development usage --------------------------------------
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
    )
  }

  shinyApp(ui = ui, server = server)
}
