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
    options = list(
      kwallm.test_async = TRUE,
      kwallm.test_fake_llm = TRUE
    )
  )
  on.exit(app$stop(), add = TRUE)
  on.exit(unlink(temp_csv), add = TRUE)

  # Upload CSV file
  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)

  # Select text column and wait for texts to be loaded
  wait_for_select_option(app, "text_upload-column", "text")
  app$set_inputs(`text_upload-column` = "text")

  # Wait for texts to be populated after column selection
  wait_for_export(
    app,
    export = "text_management-texts__document_text",
    predicate = function(x) identical(length(x), 8L),
    timeout = 10000,
    description = "loaded CSV texts"
  )

  # Wait for the by_column selector UI to render
  wait_for_bound_input(app, "text_upload-by_column")

  # Select by_column (grouping variable)
  app$set_inputs(`text_upload-by_column` = "group")

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "Testing by_column feature"
  )

  # Set categories (mode defaults to Categorization)
  app$set_inputs(`categories-fields-field1` = "Positive")
  app$set_inputs(`categories-fields-field2` = "Negative")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  set_fake_models(app)

  # Set analysis options (no paragraphs/IRR for faster test)
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-toggle` = "No")

  # Start processing
  wait_for_enabled_element(app, "processing-process")
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

  # Read results
  results <- app$get_value(export = "processing-results_table")

  # Expect that all texts are present
  expect_true(all(test_data$text %in% results$text))

  # Expect category columns are present (categorization creates boolean columns)
  expect_true(all(c("Positive", "Negative") %in% colnames(results)))

  # Expect that category columns are logical
  expect_true(all(sapply(results[c("Positive", "Negative")], is.logical)))

  results_by_text <- results[
    match(test_data$text, results$text),
    ,
    drop = FALSE
  ]
  expect_identical(results_by_text$Positive, test_data$group == "Positive")
  expect_identical(results_by_text$Negative, test_data$group == "Negative")
  expect_true(all(rowSums(results[c("Positive", "Negative")]) == 1))
})
