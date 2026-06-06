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


test_that("kwallm_mirai_default_queue_memory_mb reads configuration", {
  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) "256",
      system_memory = function() stop("should not inspect memory")
    ),
    256
  )

  expect_null(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) "0",
      system_memory = function() stop("should not inspect memory")
    )
  )

  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) unset,
      system_memory = function() 400e6
    ),
    200
  )
})


test_that("kwallm_mirai_status_memory_matches handles bounded and unbounded pools", {
  expect_true(kwallm_mirai_status_memory_matches(
    list(memory = c(used = 0, peak = 0, capacity = NA_real_)),
    NULL
  ))
  expect_true(kwallm_mirai_status_memory_matches(
    list(memory = c(used = 0, peak = 0, capacity = 128)),
    128
  ))
  expect_false(kwallm_mirai_status_memory_matches(
    list(memory = c(used = 0, peak = 0, capacity = NA_real_)),
    128
  ))
})


test_that("kwallm_ensure_mirai_daemons reuses a healthy daemon pool", {
  testthat::skip_if_not_installed("mirai")

  kwallm_test_start_mirai_daemons(n = 1L)

  result <- kwallm_ensure_mirai_daemons(n_workers = 1L, memory = NULL)

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
    memory = NULL,
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


test_that("kwallm_ensure_mirai_daemons reconfigures memory-mismatched pools", {
  state <- new.env(parent = emptyenv())
  state$has_daemons <- TRUE
  state$connections <- 1L
  state$memory <- NA_real_
  daemon_calls <- list()

  result <- kwallm_ensure_mirai_daemons(
    n_workers = 2L,
    memory = 128,
    daemons_set = function() state$has_daemons,
    daemons = function(n, memory = NULL) {
      daemon_calls[[length(daemon_calls) + 1L]] <<- list(
        n = n,
        memory = memory
      )
      state$has_daemons <- n > 0L
      state$connections <- as.integer(n)
      state$memory <- memory %||% NA_real_
      invisible(NULL)
    },
    status = function() {
      list(
        connections = state$connections,
        daemons = state$connections,
        memory = c(used = 0, peak = 0, capacity = state$memory)
      )
    },
    probe = function(timeout_ms) TRUE,
    sleep = function(seconds) NULL
  )

  expect_length(daemon_calls, 2)
  expect_identical(daemon_calls[[1]]$n, 0)
  expect_equal(daemon_calls[[2]]$n, 2)
  expect_identical(daemon_calls[[2]]$memory, 128)
  expect_false(result$recycled_pool)
  expect_true(result$reconfigured_pool)
  expect_false(result$reused_pool)
})
