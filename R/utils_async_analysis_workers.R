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
    "paragraph_summary_strategy",
    "paragraph_summary_max_reduction_iterations",
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


.kwallm_mori_metrics_state <- new.env(parent = emptyenv())
.kwallm_mori_metrics_state$shared_fields <- 0L
.kwallm_mori_metrics_state$fallback_fields <- 0L
.kwallm_mori_metrics_state$fallback_reasons <- integer()


kwallm_mori_record_outcome <- function(
  shared,
  reason = NULL,
  state = .kwallm_mori_metrics_state
) {
  if (isTRUE(shared)) {
    shared_fields <- state$shared_fields
    if (is.null(shared_fields)) {
      shared_fields <- 0L
    }
    state$shared_fields <- as.integer(shared_fields) + 1L
    return(invisible(NULL))
  }

  if (is.null(reason)) {
    reason <- "unknown"
  }
  reason <- as.character(reason)[[1L]]
  fallback_fields <- state$fallback_fields
  if (is.null(fallback_fields)) {
    fallback_fields <- 0L
  }
  state$fallback_fields <- as.integer(fallback_fields) + 1L
  reasons <- state$fallback_reasons
  if (is.null(reasons)) {
    reasons <- integer()
  }
  reason_count <- if (reason %in% names(reasons)) reasons[[reason]] else 0L
  reasons[[reason]] <- as.integer(reason_count) + 1L
  state$fallback_reasons <- reasons
  invisible(NULL)
}


kwallm_mori_metrics <- function(state = .kwallm_mori_metrics_state) {
  shared_fields <- state$shared_fields
  fallback_fields <- state$fallback_fields
  fallback_reasons <- state$fallback_reasons
  list(
    shared_fields = as.integer(if (is.null(shared_fields)) 0L else shared_fields),
    fallback_fields = as.integer(
      if (is.null(fallback_fields)) 0L else fallback_fields
    ),
    fallback_reasons = if (is.null(fallback_reasons)) {
      integer()
    } else {
      fallback_reasons
    }
  )
}


.kwallm_mori_warning_state <- new.env(parent = emptyenv())
.kwallm_mori_warning_state$keys <- character()


kwallm_mori_warn_once <- function(
  key,
  message,
  state = .kwallm_mori_warning_state,
  warn_fn = NULL
) {
  key <- as.character(key)[[1L]]
  if (key %in% state$keys) {
    return(invisible(FALSE))
  }
  state$keys <- c(state$keys, key)

  if (is.null(warn_fn)) {
    warn_fn <- if (exists("log_warn", mode = "function", inherits = TRUE)) {
      function(value) log_warn(value, component = "async")
    } else {
      function(value) warning(value, call. = FALSE)
    }
  }
  warn_fn(message)
  invisible(TRUE)
}


kwallm_mori_prune_orphans <- function(
  require_namespace = requireNamespace,
  namespace_exports = getNamespaceExports,
  prune_fn = NULL,
  warn_fn = NULL
) {
  if (!isTRUE(require_namespace("mori", quietly = TRUE))) {
    return(invisible(FALSE))
  }
  if (!("prune_shared" %in% namespace_exports("mori"))) {
    return(invisible(FALSE))
  }
  if (is.null(prune_fn)) {
    prune_fn <- mori::prune_shared
  }

  tryCatch(
    {
      prune_fn()
      invisible(TRUE)
    },
    error = function(e) {
      kwallm_mori_warn_once(
        key = "prune_shared",
        message = paste("Could not prune orphaned mori regions:", conditionMessage(e)),
        warn_fn = warn_fn
      )
      invisible(FALSE)
    }
  )
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


kwallm_mori_total_max_mb <- function(
  get_option = getOption,
  getenv = Sys.getenv
) {
  configured <- suppressWarnings(as.numeric(
    getenv("KWALLM_MORI_TOTAL_MAX_MB", unset = NA_character_)
  ))

  if (is.na(configured)) {
    configured <- suppressWarnings(as.numeric(
      get_option("mori__total_max_mb", 512)
    ))
  }

  if (length(configured) != 1L || is.na(configured) || configured <= 0) {
    return(NULL)
  }

  configured
}


.kwallm_mori_budget_state <- new.env(parent = emptyenv())
.kwallm_mori_budget_state$used_bytes <- 0


kwallm_mori_budget_try_reserve <- function(
  bytes,
  max_mb = kwallm_mori_total_max_mb(),
  state = .kwallm_mori_budget_state
) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (length(bytes) != 1L || is.na(bytes) || bytes < 0) {
    return(FALSE)
  }

  used_bytes <- suppressWarnings(as.numeric(state$used_bytes))
  if (length(used_bytes) != 1L || is.na(used_bytes) || used_bytes < 0) {
    used_bytes <- 0
  }

  max_bytes <- kwallm_mori_max_bytes(max_mb)
  if (!is.null(max_bytes) && used_bytes + bytes > max_bytes) {
    return(FALSE)
  }

  state$used_bytes <- used_bytes + bytes
  TRUE
}


kwallm_mori_budget_release <- function(
  bytes,
  state = .kwallm_mori_budget_state
) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (length(bytes) != 1L || is.na(bytes) || bytes <= 0) {
    return(invisible(NULL))
  }

  used_bytes <- suppressWarnings(as.numeric(state$used_bytes))
  if (length(used_bytes) != 1L || is.na(used_bytes) || used_bytes < 0) {
    used_bytes <- 0
  }
  state$used_bytes <- max(0, used_bytes - bytes)
  invisible(NULL)
}


kwallm_mori_new_lease <- function(state = .kwallm_mori_budget_state) {
  lease <- new.env(parent = emptyenv())
  lease$bytes <- 0
  lease$released <- FALSE
  lease$state <- state

  reg.finalizer(lease, function(x) {
    if (!isTRUE(x$released)) {
      kwallm_mori_budget_release(x$bytes, state = x$state)
      x$released <- TRUE
    }
  }, onexit = TRUE)

  lease
}


kwallm_mori_release_guard <- function(guard) {
  lease <- attr(guard, "kwallm_mori_lease", exact = TRUE)
  if (!is.environment(lease) || isTRUE(lease$released)) {
    return(invisible(NULL))
  }

  kwallm_mori_budget_release(lease$bytes, state = lease$state)
  lease$released <- TRUE
  invisible(NULL)
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
  if (!is.list(x) || is.null(names(x))) {
    return(FALSE)
  }

  required_fields <- c("name", "nonce", "signature", "signature_algorithm")
  if (!all(required_fields %in% names(x))) {
    return(FALSE)
  }

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
  total_max_mb = kwallm_mori_total_max_mb(),
  object_size = utils::object.size,
  share_fn = mori::share,
  shared_name_fn = mori::shared_name,
  budget_state = .kwallm_mori_budget_state,
  metrics_state = .kwallm_mori_metrics_state,
  warn_fn = NULL
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

  lease <- kwallm_mori_new_lease(state = budget_state)
  scope_key <- kwallm_mori_random_token(32L)
  keys <- intersect(as.character(keys), names(payload))

  for (key in keys) {
    payload_size <- kwallm_mori_value_size_bytes(
      payload[[key]],
      object_size = object_size
    )

    if (!is.null(remaining_bytes)) {
      if (is.na(payload_size) || payload_size > remaining_bytes) {
        kwallm_mori_record_outcome(
          FALSE,
          reason = "dispatch_size_cap",
          state = metrics_state
        )
        args[[key]] <- payload[[key]]
        next
      }
    }

    if (!kwallm_mori_budget_try_reserve(
      payload_size,
      max_mb = total_max_mb,
      state = budget_state
    )) {
      kwallm_mori_record_outcome(
        FALSE,
        reason = "aggregate_budget",
        state = metrics_state
      )
      args[[key]] <- payload[[key]]
      next
    }

    shared <- tryCatch(share_fn(payload[[key]]), error = function(e) e)
    if (inherits(shared, "error")) {
      kwallm_mori_budget_release(payload_size, state = budget_state)
      kwallm_mori_record_outcome(
        FALSE,
        reason = "share_error",
        state = metrics_state
      )
      kwallm_mori_warn_once(
        key = paste("share_error", conditionMessage(shared), sep = ":"),
        message = paste0(
          "mori could not share worker payload `",
          key,
          "`; using regular serialization instead: ",
          conditionMessage(shared)
        ),
        warn_fn = warn_fn
      )
      args[[key]] <- payload[[key]]
      next
    }
    shared_name <- tryCatch(
      shared_name_fn(shared),
      error = function(e) e
    )

    if (inherits(shared_name, "error")) {
      kwallm_mori_budget_release(payload_size, state = budget_state)
      kwallm_mori_record_outcome(
        FALSE,
        reason = "shared_name_error",
        state = metrics_state
      )
      kwallm_mori_warn_once(
        key = paste("shared_name_error", conditionMessage(shared_name), sep = ":"),
        message = paste0(
          "mori could not create a worker reference for `",
          key,
          "`; using regular serialization instead: ",
          conditionMessage(shared_name)
        ),
        warn_fn = warn_fn
      )
      args[[key]] <- payload[[key]]
      next
    }

    if (!is.character(shared_name) || length(shared_name) != 1L) {
      kwallm_mori_budget_release(payload_size, state = budget_state)
      kwallm_mori_record_outcome(
        FALSE,
        reason = "unsupported_type",
        state = metrics_state
      )
      args[[key]] <- payload[[key]]
      next
    }

    lease$bytes <- lease$bytes + payload_size
    kwallm_mori_record_outcome(TRUE, state = metrics_state)
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

  if (lease$bytes > 0) {
    attr(guard, "kwallm_mori_lease") <- lease
  } else {
    lease$released <- TRUE
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


#' Submit mirai work without blocking an event-loop thread
#'
#' A bounded mirai dispatcher makes `mirai()` block while its queue is full.
#' Shiny must keep its main event loop responsive, so submissions use
#' `try_mirai()` and retry asynchronously until capacity becomes available.
#'
#' @param .expr Expression to evaluate in a mirai worker.
#' @param .args Named worker arguments.
#' @param .timeout Optional mirai execution timeout in milliseconds.
#' @param .compute Optional mirai compute profile.
#' @param queue_timeout_ms Maximum time to wait for dispatcher capacity.
#' @param retry_delay_seconds Delay between non-blocking submission attempts.
#'
#' @return A promise that resolves to the mirai result.
kwallm_mirai_submit <- function(
  .expr,
  .args = list(),
  .timeout = NULL,
  .compute = NULL,
  queue_timeout_ms = getOption("kwallm__mirai_queue_wait_timeout_ms", 30000L),
  retry_delay_seconds = getOption("kwallm__mirai_queue_retry_seconds", 0.05),
  try_mirai_fn = NULL,
  later_fn = later::later,
  clock = function() proc.time()[["elapsed"]],
  promise_fn = promises::promise,
  then_fn = promises::then
) {
  expr <- substitute(.expr)
  if (is.null(try_mirai_fn)) {
    try_mirai_fn <- if (isTRUE(getOption("kwallm.test_sync_mirai", FALSE))) {
      mirai::mirai
    } else {
      mirai::try_mirai
    }
  }

  queue_timeout_ms <- suppressWarnings(as.numeric(queue_timeout_ms))
  if (length(queue_timeout_ms) != 1L || is.na(queue_timeout_ms) ||
    queue_timeout_ms < 0) {
    queue_timeout_ms <- 30000
  }

  retry_delay_seconds <- suppressWarnings(as.numeric(retry_delay_seconds))
  if (length(retry_delay_seconds) != 1L || is.na(retry_delay_seconds) ||
    retry_delay_seconds <= 0) {
    retry_delay_seconds <- 0.05
  }

  deadline <- clock() + queue_timeout_ms / 1000

  promise_fn(function(resolve, reject) {
    attempt <- function() {
      worker <- tryCatch(
        do.call(
          try_mirai_fn,
          list(
            .expr = expr,
            .args = .args,
            .timeout = .timeout,
            .compute = .compute
          )
        ),
        error = function(e) e
      )

      if (inherits(worker, "error")) {
        reject(worker)
        return(invisible(NULL))
      }

      if (is.null(worker)) {
        if (clock() >= deadline) {
          reject(simpleError(paste0(
            "Timed out waiting for capacity in the async worker queue after ",
            queue_timeout_ms,
            " ms."
          )))
        } else {
          later_fn(attempt, retry_delay_seconds)
        }
        return(invisible(NULL))
      }

      then_fn(
        worker,
        onFulfilled = resolve,
        onRejected = reject
      )
      invisible(NULL)
    }

    attempt()
  })
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
