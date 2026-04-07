library(testthat)

source(here::here("R", "load_dependencies.R"), local = TRUE)


reset_python_environment_state <- function() {
  options(kwallm__python_environment_state = NULL)
}


test_that("initialize_python_environment caches sync and install work", {
  reset_python_environment_state()

  calls <- character()

  local_mocked_bindings(
    uv_exec = function(command) {
      calls <<- c(calls, paste("uv", command))
      invisible(NULL)
    },
    use_virtualenv = function(path) {
      calls <<- c(calls, paste("venv", path))
      invisible(NULL)
    },
    .package = "reticulate"
  )

  expect_invisible(initialize_python_environment(
    sync_uv = TRUE,
    install_python = TRUE
  ))
  expect_invisible(initialize_python_environment(
    sync_uv = TRUE,
    install_python = TRUE
  ))

  expect_identical(
    calls,
    c("uv python install", "uv sync", "venv ./.venv")
  )

  state <- getOption("kwallm__python_environment_state")
  expect_true(isTRUE(state$initialized))
  expect_true(isTRUE(state$install_python_ran))
  expect_true(isTRUE(state$sync_ran))
  expect_identical(state$virtualenv, "./.venv")
})


test_that("initialize_python_environment reruns sync only when requested later", {
  reset_python_environment_state()

  calls <- character()

  local_mocked_bindings(
    uv_exec = function(command) {
      calls <<- c(calls, paste("uv", command))
      invisible(NULL)
    },
    use_virtualenv = function(path) {
      calls <<- c(calls, paste("venv", path))
      invisible(NULL)
    },
    .package = "reticulate"
  )

  expect_invisible(initialize_python_environment())
  expect_invisible(initialize_python_environment())
  expect_invisible(initialize_python_environment(sync_uv = TRUE))
  expect_invisible(initialize_python_environment(sync_uv = TRUE))

  expect_identical(
    calls,
    c("venv ./.venv", "uv sync", "venv ./.venv")
  )

  state <- getOption("kwallm__python_environment_state")
  expect_true(isTRUE(state$initialized))
  expect_false(isTRUE(state$install_python_ran))
  expect_true(isTRUE(state$sync_ran))
})
