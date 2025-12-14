library(testthat)
library(shiny)

source(here::here("R", "component_editable_field_list.R"))


test_that("editable_field_list_server: default state and save/edit behavior", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      fields <- editable_field_list_server(
        id = "fields",
        field_label = "Categorie",
        initial_count = 2,
        show_exclusive = reactiveVal(TRUE),
        processing = processing,
        lang = lang
      )

      list(fields = fields)
    },
    {
      expect_true(fields$editing())
      expect_equal(fields$unique_non_empty_count(), 0)

      # Enter values
      session$setInputs(
        `fields-field1` = "  A  ",
        `fields-field2` = "B",
        `fields-exclusive1` = TRUE,
        `fields-exclusive2` = FALSE
      )
      session$flushReact()

      # Save
      session$setInputs(`fields-toggleEdit` = 1)
      session$flushReact()

      expect_false(fields$editing())
      expect_equal(sort(fields$texts()), sort(c("A", "B")))
      expect_equal(fields$unique_non_empty_count(), 2)

      # Exclusive texts should include only the exclusive one
      expect_equal(fields$exclusive_texts(), "  A  ")

      # Back to edit mode
      session$setInputs(`fields-toggleEdit` = 2)
      session$flushReact()
      expect_true(fields$editing())
    }
  )
})


test_that("editable_field_list_server: set_values updates fields", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      fields <- editable_field_list_server(
        id = "fields",
        field_label = "Code",
        initial_count = 1,
        show_exclusive = reactiveVal(FALSE),
        processing = reactiveVal(FALSE),
        lang = lang
      )

      # Set values programmatically
      fields$set_values(c("X", "Y"))

      list(fields = fields)
    },
    {
      session$flushReact()
      expect_true(fields$editing())
      expect_equal(sort(fields$texts()), sort(c("X", "Y")))
      expect_equal(fields$unique_non_empty_count(), 2)
    }
  )
})
