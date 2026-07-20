library(testthat)


test_that("kwallm_mirai_submit stays non-blocking with a saturated dispatcher", {
  testthat::skip_if_not_installed("mirai", minimum_version = "2.7.0")
  testthat::skip_if_not_installed("later")
  testthat::skip_if_not_installed("promises")
  withr::local_options(list(kwallm.test_sync_mirai = FALSE))

  kwallm_test_stop_mirai_daemons()
  start_error <- tryCatch(
    {
      mirai::daemons(1L, memory = 1)
      NULL
    },
    error = function(e) e
  )
  if (inherits(start_error, "error")) {
    testthat::skip(paste(
      "bounded mirai dispatcher not available:",
      conditionMessage(start_error)
    ))
  }
  withr::defer(
    kwallm_test_stop_mirai_daemons(),
    testthat::teardown_env()
  )

  wait_until <- function(predicate, timeout = 5) {
    deadline <- proc.time()[["elapsed"]] + timeout
    repeat {
      status <- tryCatch(mirai::status(), error = function(e) NULL)
      if (!is.null(status) && isTRUE(predicate(status))) {
        return(invisible(status))
      }
      if (proc.time()[["elapsed"]] >= deadline) {
        stop("Timed out waiting for mirai dispatcher state", call. = FALSE)
      }
      Sys.sleep(0.02)
    }
  }

  wait_until(function(status) status$connections[[1L]] >= 1L)
  blocker <- mirai::mirai({
    Sys.sleep(1.5)
    TRUE
  })
  wait_until(function(status) status$mirai[["executing"]] >= 1L)

  queued <- mirai::mirai(
    length(payload),
    payload = raw(2e6)
  )
  wait_until(function(status) {
    status$memory[["used"]] >= 1 && status$mirai[["awaiting"]] >= 1L
  })

  marker <- tempfile("kwallm-mirai-backpressure-")
  withr::defer(unlink(marker), testthat::teardown_env())
  started <- proc.time()[["elapsed"]]
  submitted <- kwallm_mirai_submit(
    {
      writeLines("executed", marker)
      42L
    },
    .args = list(marker = marker),
    queue_timeout_ms = 5000,
    retry_delay_seconds = 0.02
  )
  submission_elapsed <- proc.time()[["elapsed"]] - started
  expect_lt(submission_elapsed, 0.5)

  deadline <- proc.time()[["elapsed"]] + 8
  while (!file.exists(marker) && proc.time()[["elapsed"]] < deadline) {
    later::run_now(timeoutSecs = 0.05)
  }
  force(submitted)

  expect_true(file.exists(marker))
  expect_identical(readLines(marker, warn = FALSE), "executed")
  expect_identical(blocker[], TRUE)
  expect_identical(queued[], 2000000L)
})
