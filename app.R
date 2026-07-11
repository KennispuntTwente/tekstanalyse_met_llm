# 1 Load dependencies ----------------------------------------------------------

source("R/load_dependencies.R")
load_dependencies("regular")


# 2 Settings -------------------------------------------------------------------

# 2.1 Asynchronous processing --------------------------------------------------

# Set asynchronous processing
# - Asynchronous processing is recommended when deploying the app to a server,
#     where multiple users can use the app simultaneously
# - Asynchronous processing also enables progress bar updates in the UI
#     during the analysis of texts, and live streaming of LLM output
#     when the LLM is writing summarizing paragraphs
# - To enable asynchronous processing, you need to use `mirai::daemons()`, e.g.,
#     `mirai::daemons(n)` where n is the number of parallel workers
# - When asynchronous processing is not needed, you can use
#     `mirai::daemons(0)`; note that the progress bar may lag behind
#     in that case, as this is built around asynchronous processing
# - See the documentation for `mirai::daemons()` for more details

test_mode <- getOption("shiny.testmode", FALSE)
test_async <- isTRUE(getOption("kwallm.test_async", FALSE)) ||
  tolower(Sys.getenv("KWALLM_TEST_ASYNC", "false")) %in% c("true", "1", "yes")

if (!test_mode || test_async) {
  daemon_status <- kwallm_ensure_mirai_daemons()

  if (isTRUE(daemon_status$recycled_pool)) {
    log_warn(
      sprintf(
        "Recycled stale mirai daemons; using %s async workers",
        daemon_status$status$connections
      ),
      component = "startup"
    )
  } else {
    log_info(
      sprintf(
        "Using %s async workers (mirai daemons)",
        daemon_status$status$connections
      ),
      component = "startup"
    )
  }
} else {
  log_info(
    paste0(
      "Using no async workers",
      " (note: progress bar & LLM streaming may lag behind;",
      " concurrent app users also not supported)"
    ),
    component = "startup"
  )
}


# 2.2 Set LLM provider & models ------------------------------------------------

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
#     in function `get_context_window_size_in_tokens` in R/utils_context_window.R
#   You may want to replace this function with a more dynamic one,
#     or add your own hardcoded values for the models you use
#     The function will return NULL if a model is not recognised;
#     the context window module then falls back to 2048
# - Note: if you make the 'preconfigured_models_...' object a named list,
#     the names will be shown in the dropdown for the user. If you do not provide names,
#     the model names will be shown. Names must be unique. If you want to use
#     a specific model twice but with different settings, a named list is
#     then required
# - Note: LLM providers are configured with stream = TRUE by default
#     (see R/module_config_llm_provider.R); this enables live streaming
#     when writing paragraphs (see paragraph_streaming option below)
if (kwallm_test_llm_enabled()) {
  test_models <- kwallm_test_llm_models()
  preconfigured_models_main <- test_models$main
  preconfigured_models_large <- test_models$large
} else {
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


## 2.3 Other options -----------------------------------------------------------

options(
  # - Optionally set a port and host for the Shiny app;
  #   this is useful when deploying the app to a server
  # shiny.port = 8100,
  # shiny.host = "0.0.0.0",

  # Set max file upload size
  # - This is the maximum size of the file that can be uploaded to the app;
  shiny.maxRequestSize = 100 * 1024^2, # 100 MB

  # Silence tidyprompt warning about auto-detecting JSON mode.
  tidyprompt.warn.auto.json = FALSE,

  # - Retry behaviour upon LLM API errors;
  #   max tries defines the maximum number of retries
  #   in connecting to the LLM API, while max interactions
  #   defines the maximum number of messages sent to the LLM API
  #   to evaluate the prompt once connected
  #     see: R/send_prompt_with_retries.R
  send_prompt_with_retries__max_tries = 5,
  send_prompt_with_retries__retry_delay_seconds = 3,
  send_prompt_with_retries__max_interactions = 10,

  # - Prompt/response tracing (privacy-sensitive; disabled by default)
  #   Log prompts & replies to a separate trace file (under logs/prompt_logs/)
  #   Optional: override the trace file location
  #     see: R/utils_send_prompt_with_retries.R
  #   Note: can be enabled via KWALLM_LOG_PROMPTS_TO_FILE env var
  send_prompt_with_retries__log_prompts_to_file = tolower(Sys.getenv(
    "KWALLM_LOG_PROMPTS_TO_FILE",
    "false"
  )) %in%
    c("true", "1", "yes"),
  send_prompt_with_retries__prompt_trace_file = NULL,
  send_prompt_with_retries__prompt_trace_retention_files = 30, # Keep last N prompt log files (NULL = indefinite)

  # - Maximum number of texts to process at once;
  #     see: R/processing.R
  processing__max_texts = 3000,

  # - Maximum number of (text-chunk x code) combinations in marking mode;
  #     see: R/analysis_marking.R
  marking__max_combinations = 50000,

  # - Configuration of LLM provider by user;
  #   these enable the user to set their own OpenAI-compatible or Ollama APIs,
  #   as alternative to the preconfigured LLM provider;
  #     see: R/llm_provider.R
  llm_provider__can_configure_oai = TRUE,
  llm_provider__default_oai_url = "https://api.openai.com/v1/chat/completions",
  llm_provider__can_configure_ollama = TRUE,
  llm_provider__default_ollama_url = "http://localhost:11434/api/chat",

  # - Language for app interface & results (Dutch (nl) or English (en));
  #     see R/language.R
  language = "en", # Default language
  language__can_toggle = TRUE, # If user can switch language in the app

  # - Enable live streaming of LLM output when writing paragraphs;
  #   only works when LLM provider has stream = TRUE;
  #     see R/component_llm_streaming.R
  paragraph_streaming = TRUE,

  # - How to summarize a category/topic when all of its texts do not fit in
  #   one context window: "batch" summarizes every randomized, balanced batch
  #   and recursively reduces those summaries; "sample" summarizes one
  #   randomized context-sized sample.
  paragraph_summary_strategy = "batch", # "batch" or "sample"
  paragraph_summary_max_reduction_iterations = 8,

  # - Default setting for anonymization of texts, and if user
  #   can toggle this setting;
  #     see R/text_management.R
  anonymization__default = "none", # Default anonymization method ("none', "regex", or "gliner")
  anonymization__none = TRUE, # If the "none" anonymization method is available
  anonymization__regex = TRUE, # If the "regex" anonymization method is available
  anonymization__gliner_model = TRUE, # If the "gliner" anonymization method is available
  anonymization__gliner_test = FALSE, # If gliner model should be tested before launching the app

  # - If text splitting via semantic chunking can be used
  #   to split texts into smaller chunks for LLM processing;
  #     see R/text_split.R
  text_split__enabled = TRUE,

  # - If a topic 'unknown/not applicable' should always be added
  #   to to the list of candiate topics during topic modelling;
  #   this may be useful to avoid LLM failure in the topic assignment process;
  #     see R/analysis_inductive_topic_modelling.R
  topic_modelling__always_add_not_applicable = TRUE,
  # - Parameters for topic batching;
  #     see R/module_misc_context_window.R
  topic_modelling__batch_size_default = 25,
  topic_modelling__batch_size_limit = 100,
  topic_modelling__number_of_batches_limit = getOption(
    "topic_modelling__number_of_batches_limit",
    50
  ),
  # - Safety caps for topic reduction;
  #   these apply only while reducing the candidate-topic list, not while
  #   generating the initial text batches above.
  topic_modelling__reduction_max_prompt_batches = getOption(
    "topic_modelling__reduction_max_prompt_batches",
    16
  ),
  topic_modelling__reduction_max_iterations = getOption(
    "topic_modelling__reduction_max_iterations",
    4
  ),
  topic_modelling__draws_default = 1,
  topic_modelling__draws_limit = 5,

  # - Logging settings;
  #   logs are written to the 'logs/' directory;
  #     see R/utils_logger.R
  logger__level = "DEBUG", # DEBUG, INFO, WARN, ERROR
  logger__dir = "logs", # Directory for log files
  logger__retention = 30 # Keep last N log files (NULL = indefinite)
)


## 2.4 Handle test settings -----------------------------------------------------

# These settings are mainly intended for automated testing of the app

if (getOption("anonymization__gliner_test", FALSE)) {
  invisible(gliner_load_model(test_model = TRUE))
}

if (!test_mode || test_async) {
  tryCatch(
    tiktoken_load_tokenizer(),
    error = function(e) {
      log_warn(
        paste0(
          "Tokenizer preload failed: ",
          conditionMessage(e),
          ". Token counting will be unavailable until Python is working."
        ),
        component = "startup"
      )
    }
  )
}


## 2.5 App version -------------------------------------------------------------

options(
  kwallm__app_version = tryCatch(
    jsonlite::fromJSON("package.json")$version,
    error = function(e) NULL
  )
)


# 3 Run app -----------------------------------------------------------------

# Make images in 'www/' folder available to the app
shiny::addResourcePath("www", "www")

shiny::shinyApp(
  ui = main_ui(),
  server = main_server(
    preconfigured_main_models = preconfigured_models_main,
    preconfigured_large_models = preconfigured_models_large
  )
)
