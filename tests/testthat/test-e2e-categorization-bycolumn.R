library(shinytest2)

test_that("{shinytest2} recording: categorization with by_column grouping variable", {
  # Create a temporary CSV file with grouping variable
  # Use vroom::vroom_write to ensure delimiter detection works correctly
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    text = c(
      "I love this product!",
      "This is terrible",
      "Great experience",
      "Not satisfied at all",
      "Excellent quality",
      "Amazing service",
      "Poor quality control",
      "Would recommend to others"
    ),
    group = c(
      "Positive",
      "Negative",
      "Positive",
      "Negative",
      "Positive",
      "Positive",
      "Negative",
      "Positive"
    ),
    stringsAsFactors = FALSE
  )
  vroom::vroom_write(test_data, temp_csv, delim = ",")

  app <- AppDriver$new(
    name = "categorization with by_column",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )

  # Upload CSV file
  app$upload_file(`text_upload-text_file` = temp_csv)
  Sys.sleep(2)

  # Select text column and wait for texts to be loaded
  app$set_inputs(`text_upload-column` = "text")

  # Wait for texts to be populated after column selection
  app$wait_for_value(
    export = "text_management-texts__raw",
    timeout = 10000,
    ignore = c(NULL)
  )

  # Wait for the by_column selector UI to render
  Sys.sleep(1)

  # Select by_column (grouping variable)
  app$set_inputs(`text_upload-by_column` = "group")
  Sys.sleep(1)

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "Testing by_column feature"
  )

  # Set categories (mode defaults to Categorization)
  app$set_inputs(`categories-fields-field1` = "Positive")
  app$set_inputs(`categories-fields-field2` = "Negative")
  Sys.sleep(1)
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

  # Set analysis options (no paragraphs/IRR for faster test)
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-toggle` = "No")

  # Start processing
  app$click("processing-process")

  # Wait for processing to start
  app$wait_for_value(
    export = "processing-processing",
    timeout = 10000,
    ignore = c(NULL, FALSE)
  )

  app$wait_for_value(
    export = "processing-success",
    timeout = 60000 # Longer timeout for by_column processing
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

  # Expect that all texts are present
  expect_true(all(test_data$text %in% results$text))

  # Expect category columns are present (categorization creates boolean columns)
  expect_true(all(c("Positive", "Negative") %in% colnames(results)))

  # Expect that category columns are logical
  expect_true(all(sapply(results[c("Positive", "Negative")], is.logical)))

  # Expect that all texts are categorized in at least one category
  expect_true(all(rowSums(results[c("Positive", "Negative")]) > 0))

  # Clean up
  unlink(temp_csv)
  app$stop()
})
