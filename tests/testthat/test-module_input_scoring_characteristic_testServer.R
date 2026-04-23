library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

source(here::here("R", "module_core_processing.R")) # disable_when_processing
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "module_input_scoring_characteristic.R"))


test_that("score_server: default empty and updates from input", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Scoren")
      processing <- reactiveVal(FALSE)

      scoring <- score_server(
        id = "score",
        mode = mode,
        processing = processing,
        lang = lang
      )

      list(scoring = scoring, lang = lang)
    },
    {
      expect_equal(scoring(), "")

      session$setInputs(`score-scoring_characteristic` = "emotionele lading")
      session$flushReact()

      expect_equal(scoring(), "emotionele lading")

      lang(make_test_lang("en")())
      session$flushReact()

      expect_equal(scoring(), "emotionele lading")
      expect_match(
        output$`score-scoring`$html,
        "emotionele lading",
        fixed = TRUE
      )
    }
  )
})


test_that("score_server: mode other than Scoren keeps reactive stable", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")
      scoring <- score_server(
        id = "score",
        mode = mode,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(scoring = scoring)
    },
    {
      expect_equal(scoring(), "")

      session$setInputs(`score-scoring_characteristic` = "x")
      session$flushReact()

      # server still observes input changes even if UI not shown
      expect_equal(scoring(), "x")
    }
  )
})
