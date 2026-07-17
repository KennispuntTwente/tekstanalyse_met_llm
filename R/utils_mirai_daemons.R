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
kwallm_cgroup_available_memory_bytes <- function(
  file_exists = file.exists,
  read_lines = readLines,
  v2_max_path = "/sys/fs/cgroup/memory.max",
  v2_current_path = "/sys/fs/cgroup/memory.current",
  v1_max_path = "/sys/fs/cgroup/memory/memory.limit_in_bytes",
  v1_current_path = "/sys/fs/cgroup/memory/memory.usage_in_bytes"
) {
  read_value <- function(path) {
    if (!isTRUE(file_exists(path))) {
      return(NA_character_)
    }

    tryCatch(
      trimws(read_lines(path, warn = FALSE, n = 1L)[[1L]]),
      error = function(e) NA_character_
    )
  }

  calculate_available <- function(max_path, current_path) {
    max_value <- read_value(max_path)
    current_value <- read_value(current_path)
    if (is.na(max_value) || identical(tolower(max_value), "max")) {
      return(NA_real_)
    }

    max_bytes <- suppressWarnings(as.numeric(max_value))
    current_bytes <- suppressWarnings(as.numeric(current_value))
    # cgroup v1 represents an unlimited value using a number near INT64_MAX.
    if (
      is.na(max_bytes) ||
        max_bytes >= 2^60 ||
        max_bytes <= 0 ||
        is.na(current_bytes) ||
        current_bytes < 0
    ) {
      return(NA_real_)
    }

    max(0, max_bytes - current_bytes)
  }

  v2_available <- calculate_available(v2_max_path, v2_current_path)
  if (!is.na(v2_available)) {
    return(v2_available)
  }

  v1_available <- calculate_available(v1_max_path, v1_current_path)
  if (!is.na(v1_available)) {
    return(v1_available)
  }

  NULL
}


kwallm_mirai_default_queue_memory_mb <- function(
  getenv = Sys.getenv,
  system_memory = function() {
    if (!requireNamespace("ps", quietly = TRUE)) {
      return(NA_real_)
    }
    ps::ps_system_memory()[["avail"]]
  },
  cgroup_memory = kwallm_cgroup_available_memory_bytes
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

  available_candidates <- suppressWarnings(as.numeric(c(
    system_memory(),
    cgroup_memory()
  )))
  available_candidates <- available_candidates[
    !is.na(available_candidates) & available_candidates > 0
  ]
  if (!length(available_candidates)) {
    return(NULL)
  }
  available_bytes <- min(available_candidates)

  # mirai expects MB. Keep roughly half of currently available RAM for queued
  # payloads and leave the rest for the Shiny process plus local daemons.
  max(1, floor(available_bytes / 2e6))
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
