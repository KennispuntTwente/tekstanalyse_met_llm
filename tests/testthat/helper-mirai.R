kwallm_test_stop_mirai_daemons <- function() {
  tryCatch(mirai::daemons(0), error = function(e) NULL)
  invisible(NULL)
}


kwallm_test_mirai_probe <- function(timeout_ms = 5000L) {
  timeout_ms <- suppressWarnings(as.integer(timeout_ms))
  if (is.na(timeout_ms) || timeout_ms < 1L) {
    timeout_ms <- 5000L
  }

  worker <- tryCatch(
    mirai::mirai(
      {
        TRUE
      },
      .timeout = timeout_ms
    ),
    error = function(e) e
  )
  if (inherits(worker, "error")) {
    return(FALSE)
  }

  result <- tryCatch(worker[], error = function(e) e)
  !inherits(result, "error") &&
    !isTRUE(mirai::is_error_value(result)) &&
    identical(result, TRUE)
}


kwallm_test_start_mirai_daemons <- function(
  n = 1L,
  timeout = 10,
  probe_timeout_ms = 5000L
) {
  testthat::skip_if_not_installed("mirai")

  n <- suppressWarnings(as.integer(n))
  if (is.na(n) || n < 1L) {
    n <- 1L
  }

  kwallm_test_stop_mirai_daemons()

  start_error <- tryCatch(
    {
      mirai::daemons(n)
      NULL
    },
    error = function(e) e
  )
  if (inherits(start_error, "error")) {
    testthat::skip(paste(
      "mirai daemons not available in this environment:",
      conditionMessage(start_error)
    ))
  }

  withr::defer(
    kwallm_test_stop_mirai_daemons(),
    testthat::teardown_env()
  )

  deadline <- proc.time()[["elapsed"]] + timeout
  repeat {
    if (kwallm_test_mirai_probe(timeout_ms = probe_timeout_ms)) {
      return(invisible(TRUE))
    }

    if (proc.time()[["elapsed"]] >= deadline) {
      kwallm_test_stop_mirai_daemons()
      testthat::skip(sprintf(
        "mirai daemons did not become ready within %.1f seconds",
        timeout
      ))
    }

    Sys.sleep(0.1)
  }
}
