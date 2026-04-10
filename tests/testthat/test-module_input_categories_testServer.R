library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

source(here::here("R", "component_editable_field_list.R"))
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "module_input_categories.R"))


test_that("categories_server: save/edit cycle and exclusive selection", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")
      processing <- reactiveVal(FALSE)
      assign_multiple <- reactiveVal(TRUE)

      categories <- categories_server(
        id = "categories",
        mode = mode,
        processing = processing,
        assign_multiple_categories = assign_multiple,
        lang = lang
      )

      list(categories = categories)
    },
    {
      expect_true(categories$editing())
      expect_equal(categories$unique_non_empty_count(), 0)

      session$setInputs(
        `categories-fields-field1` = "  A  ",
        `categories-fields-field2` = "B",
        `categories-fields-field3` = "B", # duplicate
        `categories-fields-exclusive1` = TRUE,
        `categories-fields-exclusive2` = FALSE,
        `categories-fields-exclusive3` = FALSE
      )
      session$flushReact()

      # Save
      session$setInputs(`categories-fields-toggleEdit` = 1)
      session$flushReact()

      expect_false(categories$editing())

      # texts() are trimmed+unique by editable_field_list
      expect_equal(sort(categories$texts()), sort(c("A", "B")))
      expect_equal(categories$unique_non_empty_count(), 2)

      # exclusive_texts is now trimmed+unique, matching texts()
      expect_equal(categories$exclusive_texts(), "A")

      # Back to edit
      session$setInputs(`categories-fields-toggleEdit` = 2)
      session$flushReact()
      expect_true(categories$editing())
    }
  )
})


test_that("categories_server: mode other than Categorisatie does not error", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Scoren")
      categories <- categories_server(
        id = "categories",
        mode = mode,
        processing = reactiveVal(FALSE),
        assign_multiple_categories = reactiveVal(FALSE),
        lang = lang
      )

      list(categories = categories)
    },
    {
      expect_true(is.logical(categories$editing()))
      expect_true(is.character(categories$texts()))
    }
  )
})
