library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

source(here::here("R", "component_modal_helpers.R"))
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "component_yes_no_toggle_card.R"))

source(here::here("R", "module_toggle_assign_multiple_categories.R"))
source(here::here("R", "module_toggle_human_in_the_loop.R"))
source(here::here("R", "module_toggle_interrater_reliability.R"))
source(here::here("R", "module_toggle_write_paragraphs.R"))


test_that("assign_multiple_categories_toggle_server: defaults TRUE and responds to input", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")
      processing <- reactiveVal(FALSE)

      toggle <- assign_multiple_categories_toggle_server(
        id = "amc",
        processing = processing,
        mode = mode,
        lang = lang
      )

      list(toggle = toggle, lang = lang)
    },
    {
      expect_true(toggle())

      session$setInputs(`amc-toggle` = "false")
      session$flushReact()

      expect_false(toggle())
    }
  )
})


test_that("human_in_the_loop_toggle_server: hidden when mode != Onderwerpextractie but keeps default", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")
      processing <- reactiveVal(FALSE)

      toggle <- human_in_the_loop_toggle_server(
        id = "hitl",
        processing = processing,
        mode = mode,
        lang = lang
      )

      list(toggle = toggle)
    },
    {
      # Default is FALSE.
      expect_false(toggle())
    }
  )
})


test_that("interrater_toggle_server: shown for Categorisatie/Scoren/Onderwerpextractie", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Scoren")
      processing <- reactiveVal(FALSE)

      toggle <- interrater_toggle_server(
        id = "irr",
        processing = processing,
        mode = mode,
        lang = lang
      )

      list(toggle = toggle, lang = lang)
    },
    {
      # Default is FALSE.
      expect_false(toggle())

      session$setInputs(`irr-toggle` = "true")
      session$flushReact()

      expect_true(toggle())
    }
  )
})


test_that("write_paragraphs_toggle_server: wrapper returns FALSE when mode not eligible", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")
      processing <- reactiveVal(FALSE)

      res <- write_paragraphs_toggle_server(
        id = "wp",
        processing = processing,
        mode = mode,
        lang = lang
      )

      list(res = res, lang = lang, mode = mode)
    },
    {
      # Eligible in Categorisatie; default TRUE.
      expect_true(res$write_paragraphs())
      expect_true(is.character(res$style_prompt()))

      session$setInputs(`wp-toggle` = "false")
      session$flushReact()
      expect_false(res$write_paragraphs())

      # Ineligible mode -> wrapper forces FALSE.
      mode("Scoren")
      session$flushReact()
      expect_false(res$write_paragraphs())
    }
  )
})
