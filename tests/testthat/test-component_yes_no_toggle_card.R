library(testthat)
library(shiny)

source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "component_icon_button.R"))
source(here::here("R", "component_yes_no_toggle_card.R"))


test_that("yes_no_toggle_card_server: default value and toggle mapping", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      result <- yes_no_toggle_card_server(
        id = "yn",
        title = "Title",
        tooltip_text = "Tooltip",
        question_text = "Question",
        default_value = FALSE,
        show_when = reactive(TRUE),
        processing = processing,
        lang = lang
      )

      list(result = result, lang = lang)
    },
    {
      expect_false(result())

      session$setInputs(`yn-toggle` = lang()$t("Ja"))
      session$flushReact()
      expect_true(result())

      session$setInputs(`yn-toggle` = lang()$t("Nee"))
      session$flushReact()
      expect_false(result())
    }
  )
})


test_that("yes_no_toggle_card_server: show_when FALSE does not error", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      result <- yes_no_toggle_card_server(
        id = "yn",
        title = "Title",
        tooltip_text = "Tooltip",
        question_text = "Question",
        default_value = TRUE,
        show_when = reactive(FALSE),
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(result = result)
    },
    {
      expect_true(result())
    }
  )
})


test_that("yes_no_toggle_card_server: modal_config saves and resets modal_value", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      res <- yes_no_toggle_card_server(
        id = "yn",
        title = "Title",
        tooltip_text = "Tooltip",
        question_text = "Question",
        default_value = FALSE,
        show_when = reactive(TRUE),
        modal_config = list(
          icon = "palette",
          tooltip = "Edit",
          title = "Modal",
          body_text1 = "Body",
          input_label = "Label",
          input_placeholder = "Placeholder"
        ),
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(res = res, lang = lang)
    },
    {
      # Save
      session$setInputs(`yn-show_modal` = 1)
      session$flushReact()

      session$setInputs(`yn-modal_input` = "abc")
      session$flushReact()

      session$setInputs(`yn-modal_save` = 1)
      session$flushReact()

      expect_equal(res$modal_value(), "abc")

      # Reset
      session$setInputs(`yn-show_modal` = 2)
      session$flushReact()

      session$setInputs(`yn-modal_reset` = 1)
      session$flushReact()

      expect_equal(res$modal_value(), "")
    }
  )
})
