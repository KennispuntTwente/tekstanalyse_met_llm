# Helpers to keep async worker setup out of the module flow.

#' Resolve the app root used by async workers
#'
#' @param path Path to the app root.
#'
#' @return Normalized absolute path.
kwallm_worker_app_root <- function(path = ".") {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}


#' Capture worker-relevant options from the main process
#'
#' Async workers run in separate R processes and do not reliably inherit the
#' app's runtime options. Keep the propagated set explicit so worker behavior is
#' deterministic and testable.
#'
#' @return Named list of option values.
kwallm_worker_capture_options <- function() {
  option_names <- c(
    "app__mode",
    "app_admin_name",
    "app_admin_email",
    "logger__level",
    "logger__dir",
    "logger__retention",
    "paragraph_streaming",
    "marking__max_combinations",
    "topic_modelling__always_add_not_applicable",
    "tidyprompt.verbose",
    "tidyprompt.warn.auto.json",
    "kwallm.test_fake_llm",
    "send_prompt_with_retries__max_tries",
    "send_prompt_with_retries__retry_delay_seconds",
    "send_prompt_with_retries__max_interactions",
    "send_prompt_with_retries__log_prompts",
    "send_prompt_with_retries__log_prompts_to_file",
    "send_prompt_with_retries__prompt_trace_file",
    "send_prompt_with_retries__prompt_trace_retention_files"
  )

  option_values <- lapply(option_names, getOption)
  names(option_values) <- option_names

  option_values[!vapply(option_values, is.null, logical(1))]
}


#' Load the core packages needed inside async workers
#'
#' @param packages Character vector of package names.
#'
#' @return Invisible NULL.
kwallm_worker_load_core_packages <- function(
  packages = c(
    "tidyverse",
    "tidyprompt",
    "shiny",
    "shinyjs",
    "bslib",
    "htmltools",
    "mirai",
    "promises"
  )
) {
  missing_packages <- packages[
    !vapply(
      packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  if (length(missing_packages) > 0) {
    stop(
      paste0(
        "Missing worker packages: ",
        paste(missing_packages, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }

  invisible(NULL)
}


#' Source the app's R files into an async worker environment
#'
#' @param app_root Absolute or relative app root path.
#' @param env Environment to source files into.
#'
#' @return Invisible character vector of sourced files.
kwallm_worker_source <- function(
  app_root = kwallm_worker_app_root(),
  env = parent.frame()
) {
  app_root <- kwallm_worker_app_root(app_root)
  r_dir <- file.path(app_root, "R")

  if (!dir.exists(r_dir)) {
    stop("Async worker bootstrap could not find the R directory", call. = FALSE)
  }

  setwd(app_root)
  kwallm_worker_load_core_packages()

  load_dependencies_path <- normalizePath(
    file.path(r_dir, "load_dependencies.R"),
    winslash = "/",
    mustWork = TRUE
  )
  r_files <- list.files(
    path = r_dir,
    pattern = "\\.R$",
    full.names = TRUE
  )
  r_files <- normalizePath(r_files, winslash = "/", mustWork = TRUE)
  ordered_files <- c(
    load_dependencies_path,
    setdiff(sort(r_files), load_dependencies_path)
  )

  for (file in ordered_files) {
    sys.source(file, envir = env)
  }

  invisible(ordered_files)
}


#' Bootstrap an async worker with app code and runtime options
#'
#' @param task Optional task label for debugging.
#' @param app_root Absolute or relative app root path.
#' @param worker_options Named list of options to apply in the worker.
#' @param log_context Optional logging context captured in the main process.
#' @param env Environment to bootstrap.
#'
#' @return Invisible NULL.
kwallm_worker_bootstrap <- function(
  task = NULL,
  app_root = kwallm_worker_app_root(),
  worker_options = list(),
  log_context = NULL,
  env = parent.frame()
) {
  if (length(worker_options) > 0) {
    options(worker_options)
  }

  kwallm_worker_source(app_root = app_root, env = env)

  if (!is.null(task)) {
    options(kwallm__worker_task = as.character(task)[1])
  }

  if (
    !is.null(log_context) &&
      exists("log_context_apply", envir = env, inherits = TRUE)
  ) {
    get("log_context_apply", envir = env, inherits = TRUE)(log_context)
  }

  invisible(NULL)
}


#' Export a self-contained worker bootstrap function for async workers
#'
#' @param env Environment used to resolve helper bindings.
#'
#' @return Named list suitable for `.args =`.
kwallm_worker_bootstrap_globals <- function(env = parent.frame()) {
  app_root_fn <- get("kwallm_worker_app_root", envir = env, inherits = TRUE)
  load_packages_fn <- get(
    "kwallm_worker_load_core_packages",
    envir = env,
    inherits = TRUE
  )
  source_fn <- get("kwallm_worker_source", envir = env, inherits = TRUE)
  bootstrap_fn <- get("kwallm_worker_bootstrap", envir = env, inherits = TRUE)

  source_env <- new.env(parent = environment(source_fn))
  source_env$kwallm_worker_app_root <- app_root_fn
  source_env$kwallm_worker_load_core_packages <- load_packages_fn
  environment(source_fn) <- source_env

  bootstrap_env <- new.env(parent = environment(bootstrap_fn))
  bootstrap_env$kwallm_worker_source <- source_fn
  environment(bootstrap_fn) <- bootstrap_env

  list(kwallm_worker_bootstrap = bootstrap_fn)
}
