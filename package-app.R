#### 1 Load dependencies ####

# Set library path explicitly to portable R library
portable_lib <- file.path(dirname(R.home()), "library")
.libPaths(portable_lib)
print(paste("Using library path:", portable_lib))

# Download portable WinPython
try({
  url <- "https://github.com/winpython/winpython/releases/download/16.6.20250620final/Winpython64-3.12.10.1dot.zip"
  expected_sha256 <- "7a1f004aec39615977b2b245423a50115530d16af3418df77977186a555d0a40"
  zip_file <- "WinPython.zip"
  extract_dir <- "winpython"

  if (!file.exists(extract_dir)) {
    download.file(url, zip_file, mode = "wb")
    actual_sha256 <- digest::digest(file = zip_file, algo = "sha256")

    cat("WinPython: downloaded SHA-256:", actual_sha256, "\n")
    if (tolower(actual_sha256) != tolower(expected_sha256)) {
      stop("SHA-256 hash mismatch! File may be corrupted or tampered")
    }

    dir.create(extract_dir, showWarnings = FALSE)
    unzip(zip_file, exdir = extract_dir)
  }

  python_paths <- list.files(
    extract_dir,
    pattern = "python.exe$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Filter out venv-related paths
  valid_python_paths <- python_paths[
    !grepl("venv|scripts|nt", tolower(python_paths))
  ]

  # Pick the first valid path (or throw an error if none found)
  if (length(valid_python_paths) == 0) {
    stop("No valid base python.exe found")
  }

  python_path <- valid_python_paths[1]

  if (is.na(python_path) || !file.exists(python_path)) {
    stop("WinPython: executable not found")
  }

  cat("WinPython: using Python at", python_path, "\n")
  Sys.setenv(UV_PYTHON = normalizePath(python_path))
})

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
load_all <- function(except = c()) {
  r_files <- list.files(
    path = "R",
    pattern = "\\.R$",
    full.names = TRUE
  )
  for (file in r_files) {
    if (file %in% except) {
      next
    }
    source(file)
  }
}
load_all()


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
if (!getOption("shiny.testmode", FALSE)) {
  future::plan(multisession, .skip = TRUE)
}

# Set preconfigured LLM provider and available models (optional)
# - You can preconfigure the LLM provider and available models here
#   It is also possible for users to configure their own LLM provider
#     in the interface of the app (OpenAI compatible or Ollama; see options below)
# - This example uses the OpenAI API; you can configure any other LLM provider
#     (e.g., Ollama, Azure OpenAI API, OpenRouter, etc.)
# - See: https://kennispunttwente.github.io/tidyprompt/articles/getting_started.html#setup-an-llm-provider
# - Note: your system may need to have the relevant environment variables set
#     for the LLM provider to work, e.g., `OPENAI_API_KEY` for OpenAI
# - Note: currently, context window size for models is hardcoded
#     in function `get_context_window_size_in_tokens` in R/context_window.R
#   You may want to replace this function with a more dynamic one,
#     or add your own hardcoded values for the models you use
#     The function will default to 2048 if a model is not recognised
# - Note: if you make the 'preconfigured_models_...' object a named list,
#     the names will be shown in the dropdown for the user. If you do not provide names,
#     the model names will be shown. Names must be unique. If you want to use
#     a specific model twice but with different settings, a named list is
#     then required
preconfigured_models_main <- NULL
preconfigured_models_large <- NULL
if (FALSE) {
  preconfigured_models_main <- list(
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-5-mini",
      reasoning = list(
        effort = "low"
      )
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    ))
  )
  preconfigured_models_large <- list(
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-mini"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "gpt-4.1-nano"
    )),
    tidyprompt::llm_provider_openai()$set_parameters(list(
      model = "o4-mini"
    ))
  )
}

# Optionally set other options
options(
  # - How the Shiny app is served;
  # shiny.port = 8100,
  # shiny.host = "0.0.0.0",

  # Set max file upload size
  # - This is the maximum size of the file that can be uploaded to the app;
  shiny.maxRequestSize = 100 * 1024^2, # 100 MB

  # Set max size of memory transfer between main & async processes
  future.globals.maxSize = 3 * 1024^3, # 3 GB

  # - Retry behaviour upon LLM API errors;
  #   max tries defines the maximum number of retries
  #   in connecting to the LLM API, while max interactions
  #   defines the maximum number of messages sent to the LLM API
  #   to evaluate the prompt once connected
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries__max_tries = 5,
  send_prompt_with_retries__retry_delay_seconds = 3,
  send_prompt_with_retries__max_interactions = 10,

  # - Prompt logging;
  #   if prompts & LLM replies should be written to folder 'prompt_logs'; for debugging purposes;
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries__log_prompts = FALSE,

  # - Maximum number of texts to process at once;
  #     see: R/processing.R
  processing__max_texts = 3000,

  # - Configuration of LLM provider by user;
  #   these enable the user to set their own OpenAI-compatible or Ollama APIs,
  #   as alternative to the preconfigured LLM provider;
  #     see: R/llm_provider.R
  llm_provider__can_configure_oai = TRUE,
  llm_provider__default_oai_url = "https://api.openai.com/v1/chat/completions",
  llm_provider__can_configure_ollama = TRUE,
  llm_provider__default_ollama_url = "http://localhost:11434/api/chat",

  # - Language for app interface & results (Dutch (nl) or English (en));
  #   see R/language.R
  language = "en", # Default language
  language__can_toggle = TRUE, # If user can switch language in the app

  # - Default setting for anonymization of texts, and if user
  #   can toggle this setting;
  #     see R/text_management.R
  anonymization__default = "regex", # Default anonymization method, either "none', "regex", or "gliner"
  anonymization__none = TRUE, # If the "none" anonymization method is available
  anonymization__regex = TRUE, # If the "regex" anonymization method is available
  anonymization__gliner_model = TRUE, # If the "gliner" anonymization method is available
  anonymization__gliner_test = FALSE, # If gliner model should be tested before launching the app. If test fails, app won't launch

  # - If text splitting via semantic chunking can be used
  #   to split texts into smaller chunks for LLM processing;
  #     see R/text_split.R
  text_split__enabled = TRUE,

  # - If a topic 'unknown/not applicable' should always be added
  #   to to the list of candiate topics during topic modelling;
  #   this may be useful to avoid LLM failure in the topic assignment process;
  #     see R/topic_modelling.R
  topic_modelling__always_add_not_applicable = TRUE,
  # - Parameters for text chunking;
  #     see R/context_window.R
  topic_modelling__chunk_size_default = 25,
  topic_modelling__chunk_size_limit = 100,
  topic_modelling__number_of_chunks_limit = 50,
  topic_modelling__draws_default = 1,
  topic_modelling__draws_limit = 5
)

if (getOption("anonymization__gliner_test", FALSE)) {
  invisible(gliner_load_model(test_model = TRUE))
}

if (!getOption("shiny.testmode", FALSE)) {
  try(tiktoken_load_tokenizer())
}


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
    preconfigured_main_models = preconfigured_models_main,
    preconfigured_large_models = preconfigured_models_large
  )
)
