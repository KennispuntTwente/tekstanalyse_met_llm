library(shinytest2)

test_that("{shinytest2} recording: marking with by_column grouping variable", {
  skip_if_bundle_validation_unavailable()

  # Create a temporary CSV file with grouping variable
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    text = c(
      "I love this product, the quality is amazing!",
      "This is terrible, very disappointed",
      "Great experience overall, would buy again",
      "Not satisfied at all with the delivery",
      "Excellent quality and fast shipping",
      "Amazing service from the support team"
    ),
    group = c(
      "Positive",
      "Negative",
      "Positive",
      "Negative",
      "Positive",
      "Positive"
    ),
    stringsAsFactors = FALSE
  )
  vroom::vroom_write(test_data, temp_csv, delim = ",")

  app <- kwallm_app_driver(
    name = "marking with by_column",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
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
    predicate = function(x) identical(length(x), nrow(test_data)),
    timeout = 10000,
    description = "loaded CSV texts"
  )

  # Wait for the by_column selector UI to render
  wait_for_bound_input(app, "text_upload-by_column")

  # Select by_column (grouping variable)
  app$set_inputs(`text_upload-by_column` = "group")

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "Testing grouped marking"
  )

  # Set mode to Mark
  app$set_inputs(`mode-mode` = "Mark")

  set_fake_models(app)

  # Set a known marking code
  wait_for_enabled_element(app, "marking_codes-fields-toggleEdit")
  app$set_inputs(`marking_codes-fields-field1` = "Product feedback")
  app$click("marking_codes-fields-toggleEdit")
  app$wait_for_value(
    export = "marking_codes-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  # Disable paragraphs for faster test
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

  # Start processing
  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")

  app$wait_for_value(
    export = "processing-success",
    timeout = 60000
  )

  # Read results
  results <- app$get_value(export = "processing-results_table")

  # Core marking columns present
  expect_true(all(
    c("text", "chunk_text", "code", "marked_text") %in% colnames(results)
  ))

  # All texts present
  expect_true(all(test_data$text %in% results$text))

  # Marked text (when not NA) is a substring of chunk text
  expect_true(all(
    is.na(results$marked_text) |
      mapply(
        grepl,
        pattern = results$marked_text,
        x = results$chunk_text,
        fixed = TRUE
      )
  ))

  # Download bundle includes document_groups sheet for grouped marking
  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "marking",
    expected_sheet_names = c(
      "metadata",
      "results",
      "codes",
      "chunks",
      "marking_responses",
      "markings",
      "document_groups"
    ),
    expected_results_columns = c("text", "chunk_text", "code", "marked_text"),
    expected_texts = test_data$text,
    expected_text_count = nrow(test_data)
  )

  # Grouping metadata recorded
  expect_identical(bundle$metadata$input$grouping_column, "group")
  expect_setequal(
    vapply(
      bundle$metadata$text_lineage$document_groups,
      function(g) g$group_value,
      character(1)
    ),
    unique(test_data$group)
  )

  # document_groups sheet populated
  bundle_document_groups <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "document_groups"
  )
  expect_gt(nrow(bundle_document_groups), 0)
  expect_setequal(bundle_document_groups$group_value, unique(test_data$group))

  # Marking-specific sheets populated
  bundle_chunks <- readxl::read_xlsx(bundle$results_path, sheet = "chunks")
  bundle_markings <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "markings"
  )
  bundle_responses <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "marking_responses"
  )
  expect_gt(nrow(bundle_chunks), 0)
  expect_gt(nrow(bundle_responses), 0)
  expect_true("response_status" %in% names(bundle_responses))

  # Response statuses are valid
  valid_statuses <- .kwallm_marking_response_statuses()
  expect_true(all(bundle_responses$response_status %in% valid_statuses))
})
