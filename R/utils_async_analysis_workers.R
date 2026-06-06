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
    "mori__enabled",
    "mori__max_mb",
    "logger__level",
    "logger__dir",
    "logger__retention",
    "paragraph_streaming",
    "marking__max_combinations",
    "topic_modelling__always_add_not_applicable",
    "topic_modelling__reduction_max_prompt_batches",
    "topic_modelling__reduction_max_iterations",
    # Backward-compatibility fallback for older custom deployments.
    "topic_modelling__max_groups",
    "topic_modelling__max_iterations",
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


#' Check whether mori-backed worker payloads are enabled
#'
#' @return TRUE when mori should be used for worker payload references.
kwallm_mori_enabled <- function(
  get_option = getOption,
  require_namespace = requireNamespace
) {
  isTRUE(get_option("mori__enabled", TRUE)) &&
    isTRUE(require_namespace("mori", quietly = TRUE)) &&
    isTRUE(require_namespace("openssl", quietly = TRUE)) &&
    isTRUE(require_namespace("digest", quietly = TRUE))
}


#' Resolve the configured mori shared-payload size cap
#'
#' @return Numeric size cap in MB, or NULL when uncapped.
kwallm_mori_max_mb <- function(
  get_option = getOption,
  getenv = Sys.getenv
) {
  configured <- suppressWarnings(as.numeric(
    getenv("KWALLM_MORI_MAX_MB", unset = NA_character_)
  ))

  if (is.na(configured)) {
    configured <- suppressWarnings(as.numeric(
      get_option("mori__max_mb", NA_real_)
    ))
  }

  if (length(configured) != 1L || is.na(configured) || configured <= 0) {
    return(NULL)
  }

  configured
}


kwallm_mori_max_bytes <- function(max_mb = kwallm_mori_max_mb()) {
  if (is.null(max_mb)) {
    return(NULL)
  }

  max_mb * 1024^2
}


kwallm_mori_value_size_bytes <- function(
  x,
  object_size = utils::object.size
) {
  suppressWarnings(as.numeric(object_size(x)))
}


#' Generate a cryptographic token for mori worker capabilities
#'
#' @param bytes Number of random bytes.
#'
#' @return Hex-encoded random token.
kwallm_mori_random_token <- function(bytes = 32L) {
  bytes <- suppressWarnings(as.integer(bytes))
  if (is.na(bytes) || bytes < 16L) {
    bytes <- 32L
  }

  paste0(sprintf("%02x", as.integer(openssl::rand_bytes(bytes))), collapse = "")
}


kwallm_mori_ref_signature_payload <- function(name, key, nonce) {
  paste(
    "kwallm_mori_ref_v1",
    as.character(name)[1],
    if (is.null(key)) "" else as.character(key)[1],
    as.character(nonce)[1],
    sep = "\n"
  )
}


kwallm_mori_sign_ref <- function(scope_key, name, key, nonce) {
  digest::hmac(
    key = scope_key,
    object = kwallm_mori_ref_signature_payload(name, key, nonce),
    algo = "sha256",
    serialize = FALSE
  )
}


kwallm_mori_validate_scope_key <- function(scope_key) {
  is.character(scope_key) &&
    length(scope_key) == 1L &&
    grepl("^[0-9a-f]{64}$", scope_key)
}


kwallm_mori_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}


kwallm_mori_validate_ref_shape <- function(x) {
  kwallm_mori_scalar_string(x$name) &&
    (is.null(x$key) || kwallm_mori_scalar_string(x$key)) &&
    kwallm_mori_scalar_string(x$nonce) &&
    grepl("^[0-9a-f]{32}$", x$nonce) &&
    kwallm_mori_scalar_string(x$signature) &&
    grepl("^[0-9a-f]{64}$", x$signature) &&
    identical(x$signature_algorithm, "hmac-sha256")
}


kwallm_mori_constant_time_equal <- function(a, b) {
  if (!kwallm_mori_scalar_string(a) || !kwallm_mori_scalar_string(b)) {
    return(FALSE)
  }

  a_raw <- charToRaw(enc2utf8(a))
  b_raw <- charToRaw(enc2utf8(b))
  if (length(a_raw) != length(b_raw)) {
    return(FALSE)
  }

  diff <- 0L
  for (i in seq_along(a_raw)) {
    diff <- bitwOr(
      diff,
      bitwXor(as.integer(a_raw[[i]]), as.integer(b_raw[[i]]))
    )
  }

  identical(diff, 0L)
}


#' Create a worker-safe reference to a mori shared object
#'
#' @param name Shared memory name returned by `mori::shared_name()`.
#' @param key Optional payload key used only for diagnostics.
#' @param scope_key Per-dispatch secret used to sign refs.
#'
#' @return A small serializable reference object.
kwallm_mori_make_ref <- function(name, key = NULL, scope_key) {
  if (!kwallm_mori_validate_scope_key(scope_key)) {
    stop("Invalid mori worker scope key.", call. = FALSE)
  }

  nonce <- kwallm_mori_random_token(16L)

  structure(
    list(
      name = name,
      key = key,
      nonce = nonce,
      signature = kwallm_mori_sign_ref(scope_key, name, key, nonce),
      signature_algorithm = "hmac-sha256"
    ),
    class = "kwallm_mori_ref"
  )
}


kwallm_mori_is_ref <- function(x) {
  inherits(x, "kwallm_mori_ref")
}


#' Share selected payload fields for a worker
#'
#' The returned `args` list contains normal values for fields that cannot be
#' shared and small `kwallm_mori_ref` objects for fields backed by shared memory.
#' The `guard` list must remain referenced in the main process until the worker
#' has resolved, otherwise R's garbage collector may release the shared region.
#'
#' @param payload Named list of worker arguments.
#' @param keys Names in `payload` that may be shared.
#' @param enabled Logical toggle, mostly for tests.
#'
#' @return A list with `args`, `guard`, and `shared_names`.
kwallm_mori_share_worker_payload <- function(
  payload,
  keys = names(payload),
  enabled = kwallm_mori_enabled(),
  max_mb = kwallm_mori_max_mb(),
  object_size = utils::object.size,
  share_fn = mori::share,
  shared_name_fn = mori::shared_name
) {
  if (!is.list(payload) || is.null(names(payload))) {
    stop("`payload` must be a named list.", call. = FALSE)
  }

  args <- payload
  guard <- list()
  shared_names <- character()
  scope_key <- NULL
  remaining_bytes <- kwallm_mori_max_bytes(max_mb)

  if (!isTRUE(enabled)) {
    return(structure(
      list(
        args = args,
        guard = guard,
        scope_key = scope_key,
        shared_names = shared_names
      ),
      class = "kwallm_mori_worker_payload"
    ))
  }

  scope_key <- kwallm_mori_random_token(32L)
  keys <- intersect(as.character(keys), names(payload))

  for (key in keys) {
    payload_size <- kwallm_mori_value_size_bytes(
      payload[[key]],
      object_size = object_size
    )

    if (!is.null(remaining_bytes)) {
      if (is.na(payload_size) || payload_size > remaining_bytes) {
        args[[key]] <- payload[[key]]
        next
      }
    }

    shared <- tryCatch(
      share_fn(payload[[key]]),
      error = function(e) payload[[key]]
    )
    shared_name <- tryCatch(
      shared_name_fn(shared),
      error = function(e) NULL
    )

    if (!is.character(shared_name) || length(shared_name) != 1L) {
      args[[key]] <- payload[[key]]
      next
    }

    guard[[key]] <- shared
    shared_names[[key]] <- shared_name
    args[[key]] <- kwallm_mori_make_ref(
      shared_name,
      key = key,
      scope_key = scope_key
    )

    if (!is.null(remaining_bytes)) {
      remaining_bytes <- max(0, remaining_bytes - payload_size)
    }
  }

  structure(
    list(
      args = args,
      guard = guard,
      scope_key = scope_key,
      shared_names = shared_names
    ),
    class = "kwallm_mori_worker_payload"
  )
}


#' Resolve a mori worker argument inside the worker process
#'
#' @param x A regular object or `kwallm_mori_ref`.
#' @param scope_key Per-dispatch secret that must verify `x`.
#'
#' @return The mapped shared object for refs, otherwise `x`.
kwallm_mori_resolve_worker_arg <- function(x, scope_key = NULL) {
  if (!kwallm_mori_is_ref(x)) {
    return(x)
  }

  if (!kwallm_mori_validate_scope_key(scope_key)) {
    stop("Invalid or missing mori worker scope key.", call. = FALSE)
  }

  if (!kwallm_mori_validate_ref_shape(x)) {
    stop("Rejected invalid mori worker payload capability.", call. = FALSE)
  }

  expected_signature <- kwallm_mori_sign_ref(
    scope_key = scope_key,
    name = x$name,
    key = x$key,
    nonce = x$nonce
  )
  if (!kwallm_mori_constant_time_equal(x$signature, expected_signature)) {
    stop("Rejected invalid mori worker payload capability.", call. = FALSE)
  }

  if (!requireNamespace("mori", quietly = TRUE)) {
    stop(
      "Package `mori` is required to resolve shared worker payloads.",
      call. = FALSE
    )
  }

  mapped <- tryCatch(
    mori::map_shared(x$name),
    error = function(e) {
      stop(
        paste0(
          "Could not map shared worker payload",
          if (!is.null(x$key)) paste0(" `", x$key, "`") else "",
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (is.null(mapped)) {
    stop(
      paste0(
        "Invalid shared worker payload reference",
        if (!is.null(x$key)) paste0(" for `", x$key, "`") else "",
        "."
      ),
      call. = FALSE
    )
  }

  mapped
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
  ),
  require_namespace = requireNamespace,
  library_fn = library
) {
  missing_packages <- packages[
    !vapply(
      packages,
      require_namespace,
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
      library_fn(pkg, character.only = TRUE)
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
