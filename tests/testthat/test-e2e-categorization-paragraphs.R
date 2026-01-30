library(shinytest2)

test_that("{shinytest2} recording: categorization with paragraph writing", {
  # This test verifies that categorization with write_paragraphs=Yes works correctly.

  # Previously, errors in paragraph writing were not properly caught by tests
  # because the categorization test used write_paragraphs=No.
  app <- AppDriver$new(
    name = "categorization with paragraphs",
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
  app$set_inputs(
    `research_background-research_background` = "Testing paragraphs"
  )
  app$set_inputs(`categories-fields-field1` = "Positive feedback")
  app$set_inputs(`categories-fields-field2` = "Negative feedback")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

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

  # IMPORTANT: Enable paragraph writing - this is what this test specifically covers
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")
  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")

  # Start processing
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 60000 # Longer timeout for paragraph writing
  )

  # Confirm results
  app$expect_values(
    export = c(
      "processing-processing",
      "processing-success"
    )
  )

  # Read results
  results <- app$get_value(export = "processing-final_results_df")

  # Expect that all texts are present in column 'text'
  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))

  # Expect that all categories are present as columns in results
  expect_true(all(
    c("Positive feedback", "Negative feedback") %in% colnames(results)
  ))

  # Expect that results have 'paragraphs' attribute (key test for paragraph writing)
  expect_true("paragraphs" %in% names(attributes(results)))
  paragraphs <- attr(results, "paragraphs")
  # Expect at least one paragraph was written
  expect_true(length(paragraphs) > 0)
  # Expect correct paragraph structure
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.vector(paragraphs[[1]]$texts))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)

  app$stop()
})
