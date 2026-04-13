library(testthat)
library(shiny)
library(shinyjs)

testthat::skip_if_not_installed("tidyprompt")

test_that("run_model_provider_test uses send_prompt_with_retries for both modes", {
  captured_calls <- list()

  send_prompt_with_retries <- function(prompt, llm_provider, ...) {
    captured_calls <<- c(
      captured_calls,
      list(list(
        prompt = prompt,
        llm_provider = llm_provider
      ))
    )

    if (length(captured_calls) == 1L) {
      return("pong")
    }

    list(
      steps = c("one", "two"),
      final_answer = "pong"
    )
  }

  source(here::here("R", "module_config_model.R"), local = TRUE)

  provider <- list(parameters = list(model = "test-model"))

  regular_result <- run_model_provider_test(provider, use_json = FALSE)
  json_result <- run_model_provider_test(provider, use_json = TRUE)

  expect_identical(regular_result, "pong")
  expect_match(json_result, '"final_answer": "pong"')
  expect_length(captured_calls, 2L)
  expect_identical(captured_calls[[1]]$llm_provider, provider)
  expect_identical(captured_calls[[2]]$llm_provider, provider)
})

test_that("model_server: selects preconfigured vs configured-provider models", {
  source(here::here("R", "module_config_model.R"), local = TRUE)

  # Minimal preconfigured providers.
  pre_main <- list(
    "pre-main-1" = tidyprompt::llm_provider_openai(
      url = "https://example.com/v1",
      api_key = "test-key",
      parameters = list(model = "pre-main-1", stream = TRUE),
      verbose = FALSE
    )
  )
  pre_large <- list(
    "pre-large-1" = tidyprompt::llm_provider_openai(
      url = "https://example.com/v1",
      api_key = "test-key",
      parameters = list(model = "pre-large-1", stream = TRUE),
      verbose = FALSE
    )
  )

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)
      mode <- reactiveVal("Categorisatie")

      llm_provider_rv <- reactiveValues(
        provider_mode = "preconfigured",
        llm_provider_configured = NULL,
        configured_models = NULL
      )

      models <- model_server(
        id = "model",
        preconfigured_llm_provider_model_main = pre_main,
        preconfigured_llm_provider_model_large = pre_large,
        processing = processing,
        mode = mode,
        llm_provider_rv = llm_provider_rv,
        lang = lang
      )

      list(
        models = models,
        llm_provider_rv = llm_provider_rv,
        mode = mode
      )
    },
    {
      # Preconfigured mode: selecting main_model picks from list.
      session$setInputs(`model-main_model` = "pre-main-1")
      session$flushReact()

      expect_true(!is.null(models$main))
      expect_equal(models$main$parameters$model, "pre-main-1")

      # Configured provider mode: selecting main_model clones provider and sets model.
      llm_provider_rv$provider_mode <- "openai"
      llm_provider_rv$configured_models <- c("cfg-1", "cfg-2")
      llm_provider_rv$llm_provider_configured <- tidyprompt::llm_provider_openai(
        url = "https://configured.example/v1",
        api_key = "test-key",
        parameters = list(model = "placeholder", stream = TRUE),
        verbose = FALSE
      )
      session$flushReact()

      session$setInputs(`model-main_model` = "cfg-1")
      session$flushReact()

      expect_true(!is.null(models$main))
      expect_equal(models$main$parameters$model, "cfg-1")
      expect_false(identical(
        models$main,
        llm_provider_rv$llm_provider_configured
      ))
    }
  )
})

test_that("model_server: provider URL change after model selection updates models", {
  source(here::here("R", "module_config_model.R"), local = TRUE)

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)
      mode <- reactiveVal("Categorisatie")

      llm_provider_rv <- reactiveValues(
        provider_mode = "openai",
        llm_provider_configured = tidyprompt::llm_provider_openai(
          url = "https://old.example/v1",
          api_key = "key-1",
          parameters = list(model = "placeholder", stream = TRUE),
          verbose = FALSE
        ),
        configured_models = c("gpt-4o", "gpt-4o-mini")
      )

      models <- model_server(
        id = "model",
        preconfigured_llm_provider_model_main = list(),
        preconfigured_llm_provider_model_large = list(),
        processing = processing,
        mode = mode,
        llm_provider_rv = llm_provider_rv,
        lang = lang
      )

      list(
        models = models,
        llm_provider_rv = llm_provider_rv
      )
    },
    {
      # Select a model from the old provider.
      session$setInputs(`model-main_model` = "gpt-4o")
      session$flushReact()

      expect_equal(models$main$url, "https://old.example/v1")
      expect_equal(models$main$api_key, "key-1")

      # Simulate the provider module rebuilding llm_provider_configured
      # after the user changes URL and/or API key.
      llm_provider_rv$llm_provider_configured <- tidyprompt::llm_provider_openai(
        url = "https://new.example/v1",
        api_key = "key-2",
        parameters = list(model = "placeholder", stream = TRUE),
        verbose = FALSE
      )
      session$flushReact()

      # models$main should have been resynced to the new URL and API key
      # while preserving the selected model id.
      expect_equal(models$main$parameters$model, "gpt-4o")
      expect_equal(models$main$url, "https://new.example/v1")
      expect_equal(models$main$api_key, "key-2")
    }
  )
})

test_that("model_server: advanced settings survive provider-mode round-trip", {
  source(here::here("R", "module_config_model.R"), local = TRUE)

  pre_main <- list(
    "pre-main-1" = tidyprompt::llm_provider_openai(
      url = "https://example.com/v1",
      api_key = "test-key",
      parameters = list(model = "pre-main-1", stream = TRUE),
      verbose = FALSE
    )
  )

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)
      mode <- reactiveVal("Categorisatie")

      llm_provider_rv <- reactiveValues(
        provider_mode = "openai",
        llm_provider_configured = tidyprompt::llm_provider_openai(
          url = "https://configured.example/v1",
          api_key = "test-key",
          parameters = list(model = "placeholder", stream = TRUE),
          verbose = FALSE
        ),
        configured_models = c("cfg-1", "cfg-2")
      )

      models <- model_server(
        id = "model",
        preconfigured_llm_provider_model_main = pre_main,
        preconfigured_llm_provider_model_large = list(),
        processing = processing,
        mode = mode,
        llm_provider_rv = llm_provider_rv,
        lang = lang
      )

      list(
        models = models,
        llm_provider_rv = llm_provider_rv,
        mode = mode
      )
    },
    {
      # Step 1: Select a model in OpenAI mode.
      session$setInputs(`model-main_model` = "cfg-1")
      session$flushReact()

      expect_equal(models$main$parameters$model, "cfg-1")

      # Step 2: Apply advanced settings (temperature) via the modal inputs.
      session$setInputs(
        `model-main_reasoning_effort` = "",
        `model-main_verbosity` = "",
        `model-main_temperature` = 0.7,
        `model-main_top_p` = NA_real_
      )
      session$flushReact()

      expect_equal(models$main$parameters$temperature, 0.7)

      # Step 3: Switch to preconfigured mode.
      llm_provider_rv$provider_mode <- "preconfigured"
      session$flushReact()

      session$setInputs(`model-main_model` = "pre-main-1")
      session$flushReact()
      expect_equal(models$main$parameters$model, "pre-main-1")

      # Step 4: Switch back to openai mode. The selector should restore
      # "cfg-1" (saved choice) and the model object should still have
      # temperature = 0.7.
      llm_provider_rv$provider_mode <- "openai"
      session$flushReact()

      session$setInputs(`model-main_model` = "cfg-1")
      session$flushReact()

      expect_equal(models$main$parameters$model, "cfg-1")
      expect_equal(
        models$main$parameters$temperature,
        0.7,
        info = "temperature should survive provider-mode round-trip"
      )
    }
  )
})
