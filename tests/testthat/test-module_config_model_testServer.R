library(testthat)
library(shiny)
library(shinyjs)

testthat::skip_if_not_installed("tidyprompt")

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
