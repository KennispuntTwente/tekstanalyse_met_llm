library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

source(here::here("R", "module_core_processing.R")) # disable_when_processing
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "module_input_research_background.R"))


test_that("research_background_server: default empty and updates from input", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      processing <- reactiveVal(FALSE)

      rb <- research_background_server(
        id = "research_background",
        processing = processing,
        lang = lang
      )

      list(rb = rb)
    },
    {
      expect_equal(rb(), "")

      session$setInputs(
        `research_background-research_background` = "My background"
      )
      session$flushReact()

      expect_equal(rb(), "My background")
    }
  )
})
