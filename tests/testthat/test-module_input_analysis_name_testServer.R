library(testthat)
library(shiny)
library(shinyjs)
library(bslib)

# Stub dependencies that the module uses but aren't needed for unit tests
log_action <- function(...) invisible(NULL)

source(here::here("R", "utils_processing_helpers.R")) # disable_when_processing
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "module_input_analysis_name.R"))


test_that("analysis_name_server: default is empty string", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      analysis_name <- analysis_name_server(
        id = "analysis_name",
        processing = processing,
        lang = lang
      )

      list(analysis_name = analysis_name)
    },
    {
      expect_equal(analysis_name(), "")
    }
  )
})

test_that("analysis_name_server: updates from input", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      analysis_name <- analysis_name_server(
        id = "analysis_name",
        processing = processing,
        lang = lang
      )

      list(analysis_name = analysis_name)
    },
    {
      session$setInputs(
        `analysis_name-analysis_name` = "My Test Analysis"
      )
      session$flushReact()

      expect_equal(analysis_name(), "My Test Analysis")
    }
  )
})

test_that("analysis_name_server: trims whitespace", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      analysis_name <- analysis_name_server(
        id = "analysis_name",
        processing = processing,
        lang = lang
      )

      list(analysis_name = analysis_name)
    },
    {
      session$setInputs(
        `analysis_name-analysis_name` = "  Padded Name  "
      )
      session$flushReact()

      expect_equal(analysis_name(), "Padded Name")
    }
  )
})

test_that("analysis_name_server: works with English translations", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      analysis_name <- analysis_name_server(
        id = "analysis_name",
        processing = processing,
        lang = lang
      )

      list(analysis_name = analysis_name)
    },
    {
      session$setInputs(
        `analysis_name-analysis_name` = "English Name"
      )
      session$flushReact()

      expect_equal(analysis_name(), "English Name")
    }
  )
})
