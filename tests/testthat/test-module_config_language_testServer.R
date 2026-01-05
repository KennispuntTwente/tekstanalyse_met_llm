library(testthat)
library(shiny)

source(here::here("R", "module_core_processing.R")) # for disable_when_processing
source(here::here("R", "module_config_language.R"))


test_that("language_server: respects toggle when can_toggle=TRUE", {
  withr::local_options(list(language = "nl"))
  withr::local_dir(here::here())

  shiny::testServer(
    function(input, output, session) {
      processing <- reactiveVal(FALSE)

      lang <- language_server(
        id = "language",
        processing = processing,
        can_toggle = TRUE
      )

      list(lang = lang)
    },
    {
      expect_equal(lang()$get_translation_language(), "nl")

      session$setInputs(`language-toggle` = "en")
      session$flushReact()
      expect_equal(lang()$get_translation_language(), "en")

      session$setInputs(`language-toggle` = "nl")
      session$flushReact()
      expect_equal(lang()$get_translation_language(), "nl")
    }
  )
})


test_that("language_server: ignores toggle when can_toggle=FALSE", {
  withr::local_options(list(language = "nl"))
  withr::local_dir(here::here())

  shiny::testServer(
    function(input, output, session) {
      lang <- language_server(
        id = "language",
        processing = reactiveVal(FALSE),
        can_toggle = FALSE
      )

      list(lang = lang)
    },
    {
      expect_equal(lang()$get_translation_language(), "nl")

      session$setInputs(`language-toggle` = "en")
      session$flushReact()
      expect_equal(lang()$get_translation_language(), "nl")
    }
  )
})
