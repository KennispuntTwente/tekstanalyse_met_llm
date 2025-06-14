#### 1 Load dependencies ####

# This project uses renv to manage package dependencies;
#   see https://rstudio.github.io/renv/articles/renv.html
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Install packages with renv
renv::restore()

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
#     `future::plan("sequential")`; note that the progress bar may lag behind
#     in that case, as this is built around asynchronous processing
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

# Optionally set other options
options(
  # - How the Shiny app is served;
  shiny.port = 8100,
  shiny.host = "0.0.0.0",

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
  llm_provider__default_oai_url = "https://api.openai.com/v1",
  llm_provider__default_oai_url_chat_suffix = "/chat/completions",
  llm_provider__can_configure_ollama = TRUE,
  llm_provider__default_ollama_url = "http://localhost:11434/api",
  llm_provider__default_ollama_url_chat_suffix = "/chat",

  # - Language for app interface & results (Dutch (nl) or English (en));
  #     see R/language.R
  language = "en", # Default language
  language__can_toggle = TRUE, # If user can switch language in the app

  # - Default setting for anonymization of texts, and if user
  #   can toggle this setting;
  #     see R/text_management.R
  anonymization__default = "regex", # Default anonymization method, either "none', "regex", or "gliner"
  anonymization__none = TRUE, # If the "none" anonymization method is available
  anonymization__regex = TRUE, # If the "regex" anonymization method is available
  anonymization__gliner_model = TRUE, # If the "gliner" anonymization method is available
  anonymization__gliner_test = FALSE # If gliner model should be tested before launching the app. If test fails, app won't launch
)

if (getOption("anonymization__gliner_test", FALSE)) {
  invisible(gliner_load_model(test_model = TRUE))
}


#### 3 Run app ####

# Make images in www folder available to the app
shiny::addResourcePath("www", "www")

shiny::shinyApp(
  ui = main_ui(),
  server = main_server(
    preconfigured_llm_provider = preconfigured_llm_provider,
    preconfigured_main_models = preconfigured_models_main,
    preconfigured_large_models = preconfigured_models_large
  )
)
