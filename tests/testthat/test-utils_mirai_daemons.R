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
      system_memory = function() stop("should not inspect memory"),
      cgroup_memory = function() stop("should not inspect cgroup memory")
    ),
    256
  )

  expect_null(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) "0",
      system_memory = function() stop("should not inspect memory"),
      cgroup_memory = function() stop("should not inspect cgroup memory")
    )
  )

  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) unset,
      system_memory = function() 400e6,
      cgroup_memory = function() NULL
    ),
    200
  )
})


test_that("queue memory uses the lower cgroup availability", {
  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) unset,
      system_memory = function() 8e9,
      cgroup_memory = function() 1e9
    ),
    500
  )

  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) unset,
      system_memory = function() 8e9,
      cgroup_memory = function() 40e6
    ),
    20
  )

  expect_identical(
    kwallm_mirai_default_queue_memory_mb(
      getenv = function(name, unset = NA_character_) unset,
      system_memory = function() 8e9,
      cgroup_memory = function() 0
    ),
    1
  )
})


test_that("cgroup memory detection supports v2 and v1", {
  values <- c(
    "/v2/max" = "1000000000",
    "/v2/current" = "250000000",
    "/v1/max" = "2000000000",
    "/v1/current" = "500000000"
  )
  read_lines <- function(path, warn = FALSE, n = -1L) values[[path]]
  file_exists <- function(path) path %in% names(values)

  expect_identical(
    kwallm_cgroup_available_memory_bytes(
      file_exists = file_exists,
      read_lines = read_lines,
      v2_max_path = "/v2/max",
      v2_current_path = "/v2/current",
      v1_max_path = "/v1/max",
      v1_current_path = "/v1/current"
    ),
    750000000
  )

  values[["/v2/max"]] <- "max"
  expect_identical(
    kwallm_cgroup_available_memory_bytes(
      file_exists = file_exists,
      read_lines = read_lines,
      v2_max_path = "/v2/max",
      v2_current_path = "/v2/current",
      v1_max_path = "/v1/max",
      v1_current_path = "/v1/current"
    ),
    1500000000
  )

  values[["/v2/max"]] <- "100000000"
  values[["/v2/current"]] <- "150000000"
  expect_identical(
    kwallm_cgroup_available_memory_bytes(
      file_exists = file_exists,
      read_lines = read_lines,
      v2_max_path = "/v2/max",
      v2_current_path = "/v2/current",
      v1_max_path = "/v1/max",
      v1_current_path = "/v1/current"
    ),
    0
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

  expect_true(isTRUE(getOption("mori__enabled")))
  expect_false(isTRUE(getOption("kwallm.test_sync_mirai")))

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
  probe_calls <- 0L

  result <- kwallm_ensure_mirai_daemons(
    n_workers = 1L,
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
      probe_calls <<- probe_calls + 1L
      probe_calls > 1L
    },
    sleep = function(seconds) NULL
  )

  expect_identical(daemon_calls, c(0, 1))
  expect_true(result$had_daemons)
  expect_true(result$recycled_pool)
  expect_false(result$reused_pool)
  expect_identical(as.integer(result$status$connections[[1]]), 1L)
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


test_that("kwallm_ensure_mirai_daemons reconfigures worker-count mismatches", {
  state <- new.env(parent = emptyenv())
  state$has_daemons <- TRUE
  state$connections <- 1L
  daemon_calls <- integer()

  result <- kwallm_ensure_mirai_daemons(
    n_workers = 2L,
    memory = NULL,
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
        daemons = state$connections,
        memory = c(used = 0, peak = 0, capacity = NA_real_)
      )
    },
    probe = function(timeout_ms) TRUE,
    sleep = function(seconds) NULL
  )

  expect_identical(daemon_calls, c(0, 2))
  expect_true(result$had_daemons)
  expect_true(result$reconfigured_pool)
  expect_false(result$recycled_pool)
  expect_false(result$reused_pool)
  expect_identical(as.integer(result$status$connections[[1L]]), 2L)
})
