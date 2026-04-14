library(shinytest2)

test_that("{shinytest2} recording: categorization with paragraph writing", {
  skip_if_bundle_validation_unavailable()

  # This test verifies that categorization with write_paragraphs=Yes works correctly.

  # Previously, errors in paragraph writing were not properly caught by tests
  # because the categorization test used write_paragraphs=No.
  app <- kwallm_app_driver(
    name = "categorization with paragraphs",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_fake_llm = TRUE
    )
  )
  on.exit(app$stop(), add = TRUE)

  # Upload texts
  wait_for_text_upload_input(app)
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
  wait_for_bound_input(app, "categories-fields-toggleEdit")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  # Set deterministic fake model
  set_fake_models(app)

  # IMPORTANT: Enable paragraph writing - this is what this test specifically covers
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")
  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")

  # Start processing
  wait_for_bound_input(app, "processing-process")
  wait_for_enabled_element(app, "processing-process")
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

  app$wait_for_value(
    export = "processing-paragraph_entries",
    timeout = 10000
  )

  # Read results
  results <- app$get_value(export = "processing-results_table")
  paragraphs <- app$get_value(export = "processing-paragraph_entries")

  # Expect that all texts are present in column 'text'
  texts <- app$get_value(
    export = "text_management-texts__preprocessed"
  )
  expect_true(all(texts %in% results$text))

  # Expect that all categories are present as columns in results
  expect_true(all(
    c("Positive feedback", "Negative feedback") %in% colnames(results)
  ))

  # At least one row must have been assigned to multiple categories (both TRUE).
  # Without this assertion, a parser bug that silently drops all but the first
  # category would pass the test suite.
  multi_assigned <- results[["Positive feedback"]] &
    results[["Negative feedback"]]
  expect_true(
    any(multi_assigned, na.rm = TRUE),
    label = "at least one text assigned to both categories"
  )

  # Expect at least one paragraph was written
  expect_true(length(paragraphs) > 0)
  # Expect correct paragraph structure
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.vector(paragraphs[[1]]$texts))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)
  expect_true(is.numeric(paragraphs[[1]]$analysis_unit_ids))
  expect_true(length(paragraphs[[1]]$analysis_unit_ids) > 0)
  expect_identical(
    length(paragraphs[[1]]$analysis_unit_ids),
    length(paragraphs[[1]]$texts)
  )

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "categorization",
    expected_sheet_names = c(
      "metadata",
      "results",
      "labels",
      "assignments",
      "categorization_response_status",
      "paragraphs",
      "paragraph_sources"
    ),
    expected_results_columns = c(
      "text",
      "Positive feedback",
      "Negative feedback"
    ),
    expected_result_rows = nrow(results),
    expected_texts = texts,
    expected_text_count = length(texts)
  )

  expect_true(isTRUE(bundle$metadata$mode_config$write_paragraphs))

  bundle_paragraphs <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "paragraphs"
  )
  bundle_paragraph_sources <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "paragraph_sources"
  )
  expect_gt(nrow(bundle_paragraphs), 0)
  expect_gt(nrow(bundle_paragraph_sources), 0)
})
