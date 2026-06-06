# Helpers for validating and recycling the shared mirai daemon pool.

#' Compute the configured mirai worker count
#'
#' @return Integer worker count, always at least 1.
kwallm_mirai_default_workers <- function(
  detect_cores = parallel::detectCores,
  getenv = Sys.getenv
) {
  max_cores <- suppressWarnings(as.integer(detect_cores()))
  if (is.na(max_cores) || max_cores < 1L) {
    max_cores <- 1L
  }

  requested_workers <- suppressWarnings(as.integer(
    getenv("KWALLM_N_ASYNC_WORKERS", unset = "2")
  ))
  if (is.na(requested_workers) || requested_workers < 1L) {
    requested_workers <- 2L
  }

  min(max_cores, requested_workers)
}


#' Compute the configured mirai dispatcher queue memory cap
#'
#' @return Numeric memory cap in MB, or NULL to leave mirai unbounded.
kwallm_mirai_default_queue_memory_mb <- function(
  getenv = Sys.getenv,
  system_memory = function() {
    if (!requireNamespace("ps", quietly = TRUE)) {
      return(NA_real_)
    }
    ps::ps_system_memory()[["avail"]]
  }
) {
  configured <- suppressWarnings(as.numeric(
    getenv("KWALLM_MIRAI_QUEUE_MEMORY_MB", unset = NA_character_)
  ))
  if (!is.na(configured)) {
    if (configured <= 0) {
      return(NULL)
    }
    return(configured)
  }

  available_bytes <- suppressWarnings(as.numeric(system_memory()))
  if (is.na(available_bytes) || available_bytes <= 0) {
    return(NULL)
  }

  # mirai expects MB. Keep roughly half of currently available RAM for queued
  # payloads and leave the rest for the Shiny process plus local daemons.
  max(64, floor(available_bytes / 2e6))
}


kwallm_mirai_status_memory_matches <- function(status, memory) {
  status_memory <- status$memory
  capacity <- NA_real_
  if (!is.null(status_memory) && "capacity" %in% names(status_memory)) {
    capacity <- suppressWarnings(as.numeric(status_memory[["capacity"]]))
  }

  if (is.null(memory)) {
    return(is.na(capacity))
  }

  !is.na(capacity) && abs(capacity - memory) < .Machine$double.eps^0.5
}


#' Probe the current mirai daemon pool
#'
#' @param timeout_ms Timeout in milliseconds for the worker ping.
#'
#' @return TRUE when a worker responds successfully, FALSE otherwise.
kwallm_mirai_probe <- function(
  timeout_ms = getOption("kwallm__mirai_probe_timeout_ms", 1000L),
  mirai_fn = mirai::mirai,
  is_error_value = mirai::is_error_value
) {
  timeout_ms <- suppressWarnings(as.integer(timeout_ms))
  if (is.na(timeout_ms) || timeout_ms < 1L) {
    timeout_ms <- 1000L
  }

  ping <- tryCatch(
    mirai_fn(
      {
        TRUE
      },
      .timeout = timeout_ms
    ),
    error = function(e) e
  )
  if (inherits(ping, "error")) {
    return(FALSE)
  }

  result <- tryCatch(
    ping[],
    error = function(e) e
  )

  !inherits(result, "error") &&
    !isTRUE(is_error_value(result)) &&
    identical(result, TRUE)
}


#' Ensure the shared mirai daemon pool is alive
#'
#' A configured daemon pool can outlive a stopped Shiny app in the same R
#' session. This helper treats `daemons_set()` as configuration only, verifies
#' liveness with a cheap worker ping, and recycles the pool when the ping fails.
#'
#' @param n_workers Desired number of workers.
#' @param probe_timeout_ms Timeout in milliseconds for the worker ping.
#'
#' @return Named list describing the daemon state after validation.
kwallm_ensure_mirai_daemons <- function(
  n_workers = kwallm_mirai_default_workers(),
  memory = kwallm_mirai_default_queue_memory_mb(),
  probe_timeout_ms = getOption("kwallm__mirai_probe_timeout_ms", 1000L),
  daemons_set = mirai::daemons_set,
  daemons = mirai::daemons,
  status = mirai::status,
  probe = kwallm_mirai_probe,
  sleep = Sys.sleep
) {
  n_workers <- suppressWarnings(as.integer(n_workers))
  if (is.na(n_workers) || n_workers < 1L) {
    n_workers <- 1L
  }

  memory <- suppressWarnings(as.numeric(memory))
  if (length(memory) != 1L || is.na(memory) || memory <= 0) {
    memory <- NULL
  }

  current_status <- tryCatch(
    status(),
    error = function(e) list(connections = 0L, daemons = 0L)
  )
  current_connections <- suppressWarnings(as.integer(current_status$connections[[
    1
  ]]))
  if (is.na(current_connections)) {
    current_connections <- 0L
  }

  had_daemons <- isTRUE(tryCatch(daemons_set(), error = function(e) FALSE))
  pool_healthy <- FALSE
  recycled_pool <- FALSE
  reconfigured_pool <- FALSE
  memory_matches <- kwallm_mirai_status_memory_matches(current_status, memory)
  needs_reconfigure <- had_daemons &&
    current_connections > 0L &&
    !isTRUE(memory_matches)

  if (had_daemons && current_connections > 0L && !needs_reconfigure) {
    pool_healthy <- isTRUE(probe(timeout_ms = probe_timeout_ms))
  }

  if (had_daemons && (!pool_healthy || needs_reconfigure)) {
    recycled_pool <- !needs_reconfigure
    reconfigured_pool <- needs_reconfigure
    tryCatch(daemons(0), error = function(e) NULL)
    sleep(0.1)
  }

  if (!had_daemons || !pool_healthy) {
    if (is.null(memory)) {
      daemons(n_workers)
    } else {
      daemons(n_workers, memory = memory)
    }

    if (!isTRUE(probe(timeout_ms = probe_timeout_ms))) {
      stop("mirai daemons started but failed the health probe")
    }
  }

  final_status <- tryCatch(
    status(),
    error = function(e) list(connections = 0L, daemons = 0L)
  )

  list(
    requested_workers = n_workers,
    had_daemons = had_daemons,
    recycled_pool = recycled_pool,
    reconfigured_pool = reconfigured_pool,
    reused_pool = had_daemons && pool_healthy && !needs_reconfigure,
    memory = memory,
    status = final_status
  )
}
