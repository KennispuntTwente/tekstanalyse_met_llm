#### 1 Load dependencies ####

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


#### 2 Settings ####

# Set asynchronous processing
# - Asynchronous processing is recommended when deploying the app to a server,
#     where multiple users can use the app simultaneously
# - To enable asynchronous processing, you need to use `future::plan()`, e.g.,
#     `future::plan(multisession)`
# - When you asynchronous processing is not needed, you can use
#     `future::plan("sequential")`
# - See the documentation for `future::plan()` for more details
future::plan(multisession, .skip = TRUE)

# Set preconfigured LLM provider and available models (optional)
# - You can preconfigure the LLM provider and available models here
#   It is also possible for users to configure their own LLM provider
#     in the interface of the app (OpenAI compatible or Ollama; see options below)
# - This example uses the OpenAI API; you can configure any other LLM provider
#     (e.g., Ollama, Azure OpenAI API, OpenRouter, etc.)
# - See: https://tjarkvandemerwe.github.io/tidyprompt/articles/getting_started.html#setup-an-llm-provider
# - Note: your system may need to have the relevant environment variables set
#     for the LLM provider to work, e.g., `OPENAI_API_KEY` for OpenAI
# - Note: currently, context window size for models is hardcoded
#     in function `get_context_window_size_in_tokens` in R/context_window.R
#   You may want to replace this function with a more dynamic one,
#     or add your own hardcoded values for the models you use
#   The function will default to 2048 if a model is not recognised
preconfigured_llm_provider <- NULL
preconfigured_models_main <- NULL
preconfigured_models_large <- NULL
if (FALSE) {
  preconfigured_llm_provider <- tidyprompt::llm_provider_openai()
  preconfigured_llm_provider$parameters$model <- "gpt-4o-mini-2024-07-18"
  preconfigured_llm_provider$parameters$stream <- FALSE
  preconfigured_models_main <- c(
    "gpt-4o-mini-2024-07-18",
    "gpt-4.1-mini-2025-04-14",
    "gpt-4.1-2025-04-14"
  )
  preconfigured_models_large <- c(
    "gpt-4o-mini-2024-07-18",
    "gpt-4.1-mini-2025-04-14",
    "gpt-4.1-2025-04-14",
    "o3-2025-04-16",
    "o4-mini-2025-04-16"
  )
}

# Optionally set other options
options(
  # - How the Shiny app is served;
  # shiny.port = 8100,
  # shiny.host = "0.0.0.0",

  # - Retry behaviour upon LLM API errors;
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries___max_tries = 10,
  send_prompt_with_retries___retry_delay_seconds = 3,

  # - Maximum number of texts to process at once;
  #     see: R/processing.R
  processing___max_texts = 3000,

  # - Configuration of LLM provider by user;
  #   these enable the user to set their own OpenAI-compatible or Ollama APIs,
  #   as alternative to the preconfigured LLM provider;
  #     see: R/llm_provider.R
  llm_provider__can_configure_oai = TRUE,
  llm_provider__can_configure_ollama = TRUE,

  # - Language for app interface & results (Dutch (nl) or English (en));
  #   see R/language.R
  language = "en", # Default language
  language__can_toggle = TRUE # If user can switch language in the app
)


#### 3 Run app ####

# Make images in www folder available to the app
shiny::addResourcePath("www", "www")

# Set Shiny port; read from arguments passed to this script
args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) > 0) as.numeric(args[1]) else 3838
options(shiny.port = port)

shiny::shinyApp(
  ui = main_ui(),
  server = main_server(
    preconfigured_llm_provider = preconfigured_llm_provider,
    preconfigured_main_models = preconfigured_models_main,
    preconfigured_large_models = preconfigured_models_large
  )
)
