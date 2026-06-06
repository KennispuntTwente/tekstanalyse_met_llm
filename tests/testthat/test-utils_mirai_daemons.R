library(testthat)

source(here::here("R", "utils_mirai_daemons.R"), local = TRUE)


test_that("kwallm_mirai_default_workers clamps invalid configuration", {
  expect_identical(
    kwallm_mirai_default_workers(
      detect_cores = function() 4L,
      getenv = function(...) "12"
    ),
    4L
  )

  expect_identical(
    kwallm_mirai_default_workers(
      detect_cores = function() NA_integer_,
      getenv = function(...) "0"
    ),
    1L
  )
})


test_that("kwallm_ensure_mirai_daemons reuses a healthy daemon pool", {
  testthat::skip_if_not_installed("mirai")

  kwallm_test_start_mirai_daemons(n = 1L)

  result <- kwallm_ensure_mirai_daemons(n_workers = 1L)

  expect_true(result$had_daemons)
  expect_false(result$recycled_pool)
  expect_true(result$reused_pool)
  expect_gte(as.integer(result$status$connections[[1]]), 1L)
})


test_that("kwallm_ensure_mirai_daemons recycles stale daemons when probe fails", {
  state <- new.env(parent = emptyenv())
  state$has_daemons <- TRUE
  state$connections <- 1L
  daemon_calls <- integer()

  result <- kwallm_ensure_mirai_daemons(
    n_workers = 2L,
    daemons_set = function() state$has_daemons,
    daemons = function(n) {
      daemon_calls <<- c(daemon_calls, n)
      state$has_daemons <- n > 0L
      state$connections <- as.integer(n)
      invisible(NULL)
    },
    status = function() {
      list(
        connections = state$connections,
        daemons = state$connections
      )
    },
    probe = function(timeout_ms) {
      if (state$connections == 1L) {
        return(FALSE)
      }
      TRUE
    },
    sleep = function(seconds) NULL
  )

  expect_identical(daemon_calls, c(0, 2))
  expect_true(result$had_daemons)
  expect_true(result$recycled_pool)
  expect_false(result$reused_pool)
  expect_identical(as.integer(result$status$connections[[1]]), 2L)
})
