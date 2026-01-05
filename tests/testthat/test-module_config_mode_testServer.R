library(testthat)
library(shiny)

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

      session$setInputs(`mode-mode` = lang()$t("Scoren"))
      session$flushReact()
      expect_equal(mode(), "Scoren")

      session$setInputs(`mode-mode` = lang()$t("Onderwerpextractie"))
      session$flushReact()
      expect_equal(mode(), "Onderwerpextractie")

      session$setInputs(`mode-mode` = lang()$t("Markeren"))
      session$flushReact()
      expect_equal(mode(), "Markeren")
    }
  )
})
