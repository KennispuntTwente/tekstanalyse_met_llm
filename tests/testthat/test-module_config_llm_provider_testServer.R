library(testthat)
library(shiny)
library(shinyjs)
suppressWarnings(library(promises))

testthat::skip_if_not_installed("later")

test_that("llm_provider_server: switches modes and fetches OpenAI models (mocked)", {
  # Store original mirai function
  original_mirai <- mirai::mirai

  # Deterministic async stub: ignore expr, return models based on provider_mode.
  # This keeps tests fast and avoids real network.
  stub_mirai <- function(expr, .args = NULL, ...) {
    if (is.null(.args)) {
      .args <- list()
    }

    provider_mode <- .args$provider_mode %||% "openai"

    models <- if (identical(provider_mode, "openai")) {
      c("gpt-4.1-nano-2025-04-14", "gpt-test")
    } else if (identical(provider_mode, "ollama")) {
      c("llama3.1:8b", "qwen2.5:7b")
    } else {
      character(0)
    }

    # Return the same structure that the real code expects
    promises::promise_resolve(list(
      ok = TRUE,
      provider = provider_mode,
      request_url = "mock://test",
      models = models
    ))
  }

  # Replace mirai::mirai with our stub
  assignInNamespace("mirai", stub_mirai, ns = "mirai")

  # Restore on exit
  on.exit(
    {
      assignInNamespace("mirai", original_mirai, ns = "mirai")
    },
    add = TRUE
  )

  # Source locally so the module sees our stubbed async code.
  source(here::here("R", "module_core_processing.R"), local = TRUE) # disable_when_processing
  source(here::here("R", "component_icon_button.R"), local = TRUE) # icon_toggle_button used in UI
  source(here::here("R", "component_card_header_with_tooltip.R"), local = TRUE)
  source(here::here("R", "component_description_box.R"), local = TRUE)
  source(here::here("R", "module_config_llm_provider.R"), local = TRUE)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      llm_provider_rv <- llm_provider_server(
        id = "llm_provider",
        processing = processing,
        has_preconfigured_llm_provider = TRUE,
        can_configure_oai = TRUE,
        can_configure_ollama = TRUE,
        lang = lang
      )

      list(
        lang = lang,
        processing = processing,
        llm_provider_rv = llm_provider_rv
      )
    },
    {
      expect_equal(llm_provider_rv$provider_mode, "preconfigured")
      expect_null(llm_provider_rv$llm_provider_configured)

      # Switch to OpenAI mode.
      session$setInputs(`llm_provider-select_openai` = 1)
      session$flushReact()
      expect_equal(llm_provider_rv$provider_mode, "openai")

      # Provide API key so provider can be configured.
      session$setInputs(`llm_provider-api_key_text` = "test-key")
      session$flushReact()
      expect_true(!is.null(llm_provider_rv$llm_provider_configured))

      # Fetch models (mocked via future stub).
      session$setInputs(`llm_provider-openai_url` = "https://example.com/v1")
      session$flushReact()
      session$setInputs(`llm_provider-get_models` = 1)
      session$flushReact()
      later::run_now(0.25)
      session$flushReact()

      expect_equal(
        llm_provider_rv$configured_models,
        c("gpt-4.1-nano-2025-04-14", "gpt-test")
      )

      # Switch back to preconfigured mode.
      session$setInputs(`llm_provider-select_preconfigured` = 1)
      session$flushReact()
      expect_equal(llm_provider_rv$provider_mode, "preconfigured")
      expect_null(llm_provider_rv$llm_provider_configured)
    }
  )
})


test_that("llm_provider_server: switches modes and fetches Ollama models (mocked)", {
  # Store original mirai function
  original_mirai <- mirai::mirai

  # Deterministic async stub: ignore expr, return models based on provider_mode.
  stub_mirai <- function(expr, .args = NULL, ...) {
    if (is.null(.args)) {
      .args <- list()
    }

    provider_mode <- .args$provider_mode %||% "ollama"

    models <- if (identical(provider_mode, "openai")) {
      c("gpt-4.1-nano-2025-04-14", "gpt-test")
    } else if (identical(provider_mode, "ollama")) {
      c("llama3.1:8b", "qwen2.5:7b")
    } else {
      character(0)
    }

    # Return the same structure that the real code expects
    promises::promise_resolve(list(
      ok = TRUE,
      provider = provider_mode,
      request_url = "mock://test",
      models = models
    ))
  }

  # Replace mirai::mirai with our stub
  assignInNamespace("mirai", stub_mirai, ns = "mirai")

  # Restore on exit
  on.exit(
    {
      assignInNamespace("mirai", original_mirai, ns = "mirai")
    },
    add = TRUE
  )

  source(here::here("R", "module_core_processing.R"), local = TRUE)
  source(here::here("R", "component_icon_button.R"), local = TRUE)
  source(here::here("R", "component_card_header_with_tooltip.R"), local = TRUE)
  source(here::here("R", "component_description_box.R"), local = TRUE)
  source(here::here("R", "module_config_llm_provider.R"), local = TRUE)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      llm_provider_rv <- llm_provider_server(
        id = "llm_provider",
        processing = processing,
        has_preconfigured_llm_provider = TRUE,
        can_configure_oai = TRUE,
        can_configure_ollama = TRUE,
        lang = lang
      )

      list(
        lang = lang,
        processing = processing,
        llm_provider_rv = llm_provider_rv
      )
    },
    {
      session$setInputs(`llm_provider-select_ollama` = 1)
      session$flushReact()
      expect_equal(llm_provider_rv$provider_mode, "ollama")

      session$setInputs(
        `llm_provider-ollama_url` = "http://localhost:11434/api/chat"
      )
      session$flushReact()
      session$setInputs(`llm_provider-get_models` = 1)
      session$flushReact()
      later::run_now(0.25)
      session$flushReact()

      expect_equal(
        llm_provider_rv$configured_models,
        c("llama3.1:8b", "qwen2.5:7b")
      )
    }
  )
})


test_that("llm_provider_server: env OPENAI_API_KEY is never rendered into the browser", {
  secret <- "sk-secret-test-key-do-not-leak-12345"
  withr::local_envvar(OPENAI_API_KEY = secret)

  source(here::here("R", "module_core_processing.R"), local = TRUE)
  source(here::here("R", "component_icon_button.R"), local = TRUE)
  source(here::here("R", "component_card_header_with_tooltip.R"), local = TRUE)
  source(here::here("R", "component_description_box.R"), local = TRUE)
  source(here::here("R", "module_config_llm_provider.R"), local = TRUE)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      llm_provider_rv <- llm_provider_server(
        id = "llm_provider",
        processing = processing,
        has_preconfigured_llm_provider = TRUE,
        can_configure_oai = TRUE,
        can_configure_ollama = TRUE,
        lang = lang
      )

      list(
        lang = lang,
        processing = processing,
        llm_provider_rv = llm_provider_rv
      )
    },
    {
      # Switch to OpenAI mode to trigger the API key input renderUI.
      session$setInputs(`llm_provider-select_openai` = 1)
      session$flushReact()

      # The env key must be active server-side (provider is configured).
      expect_true(!is.null(llm_provider_rv$llm_provider_configured))

      # Rendered HTML must NOT contain the secret.
      rendered_html <- output$`llm_provider-api_key_input`$html
      expect_false(
        grepl(secret, rendered_html, fixed = TRUE),
        info = "Server-side OPENAI_API_KEY must never appear in browser HTML"
      )

      # The placeholder should hint that an env key is present.
      expect_true(
        grepl("env", rendered_html, fixed = TRUE),
        info = "Placeholder should indicate an env key is configured"
      )

      # A user-entered key should override the env key.
      session$setInputs(`llm_provider-api_key_text` = "user-provided-key")
      session$flushReact()
      expect_true(!is.null(llm_provider_rv$llm_provider_configured))
    }
  )
})
