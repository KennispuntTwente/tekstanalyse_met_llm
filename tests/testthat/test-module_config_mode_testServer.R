library(testthat)
library(shiny)
library(shinyWidgets)
library(bslib)

source(here::here("R", "module_core_processing.R")) # for disable_when_processing
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "component_description_box.R"))
source(here::here("R", "module_config_mode.R"))


test_that("mode_server: default and input mapping", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      mode <- mode_server(
        id = "mode",
        processing = processing,
        lang = lang
      )

      list(mode = mode, lang = lang)
    },
    {
      expect_equal(mode(), "Categorisatie")

      session$setInputs(`mode-mode` = "Scoren")
      session$flushReact()
      expect_equal(mode(), "Scoren")

      session$setInputs(`mode-mode` = "Onderwerpextractie")
      session$flushReact()
      expect_equal(mode(), "Onderwerpextractie")

      session$setInputs(`mode-mode` = "Markeren")
      session$flushReact()
      expect_equal(mode(), "Markeren")
    }
  )
})


test_that("mode_server preserves selected mode across language re-render", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      mode <- mode_server(
        id = "mode",
        processing = processing,
        lang = lang
      )

      list(mode = mode, lang = lang)
    },
    {
      session$setInputs(`mode-mode` = "Scoren")
      session$flushReact()

      lang(make_test_lang("en")())
      session$flushReact()

      expect_equal(mode(), "Scoren")
      expect_match(
        output$`mode-card`$html,
        'value="Scoren" checked="checked"',
        fixed = TRUE
      )
      expect_match(output$`mode-card`$html, "Scoring", fixed = TRUE)
    }
  )
})
