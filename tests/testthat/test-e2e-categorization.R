library(shinytest2)

test_that("{shinytest2} recording: standard process - categorization", {
  app <- AppDriver$new(
    name = "standard process - categorization",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )

  # Upload texts
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  # Enter categories
  app$set_inputs(`research_background-research_background` = "no clue!")
  app$set_inputs(`categories-fields-field1` = "a")
  app$set_inputs(`categories-fields-field2` = "b")
  app$click("categories-fields-toggleEdit")

  # Set model
  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)
  app$click("llm_provider-get_models")
  app$wait_for_value(
    export = "llm_provider-available_models_openai",
  )
  models <- app$get_value(export = "llm_provider-available_models_openai")
  expect_true("gpt-4.1-nano-2025-04-14" %in% models)
  app$set_inputs(`model-main_model` = "gpt-4.1-nano-2025-04-14")

  # Set writing paragraphs toggle
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

  # Start processing
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
  )

  # Confirm results
  app$expect_values(
    export = c(
      # Text upload & processing works
      "text_management-anonymization_mode",
      "text_management-texts__raw",
      "text_management-texts__preprocessed",
      "text_management-texts__df",

      # Categories works
      "categories-fields-n_fields",
      "categories-fields-txt_in_fields",
      "categories-fields-isEditing",

      # Processing was successful
      "processing-processing",
      "processing-success"
    )
  )

  # Read results
  results <- app$get_value(export = "processing-results_table")

  # Expect that all texts are present in column 'text'
  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))
  expect_true(all.equal(
    table(texts),
    table(results$text),
    check.attributes = FALSE
  ))

  # Expect that all categories are present as columns in results
  expect_true(all(c("a", "b") %in% colnames(results)))
  # Expect that all category columns are logical
  expect_true(all(sapply(results[c("a", "b")], is.logical)))
  # Expect that all texts are categorized in at least one category
  expect_true(all(rowSums(results[c("a", "b")]) > 0))

  app$stop()
})
