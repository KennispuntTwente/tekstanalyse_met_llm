library(testthat)
library(shiny)
library(htmltools)

# Stub UI side effects.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(...) invisible(NULL)

source(here::here("R", "utils_app_error.R"), local = TRUE)

make_translator <- function(lang_code = "nl") {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  tr$set_translation_language(lang_code)
  tr
}

make_fake_session <- function() {
  closed <- FALSE
  list(
    token = "deadbeefcafebabe",
    close = function() {
      closed <<- TRUE
    },
    is_closed = function() closed
  )
}


test_that("app_error: nonfatal logs to nonfatal folder and does not close session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})


test_that("app_error: fatal logs to fatal folder and closes session", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  expect_output(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_true(sess$is_closed())
})


test_that("app_error: with NULL session stops after logging", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  expect_error(
    app_error(
      simpleError("boom"),
      when = "unit",
      fatal = FALSE,
      shiny_session = NULL,
      lang = make_translator("nl")
    ),
    "boom",
    fixed = TRUE
  )
})


test_that("app_error: downgrades IPC interrupt error from fatal to nonfatal", {
  test_dir <- withr::local_tempdir()
  withr::local_dir(test_dir)

  sess <- make_fake_session()

  ipc_err <- simpleError("Cannot pop from destroyed TextFileSource")
  expect_output(
    app_error(
      ipc_err,
      when = "unit",
      fatal = TRUE,
      shiny_session = sess,
      lang = make_translator("nl")
    ),
    regexp = "Error:"
  )
  expect_false(sess$is_closed())
})
