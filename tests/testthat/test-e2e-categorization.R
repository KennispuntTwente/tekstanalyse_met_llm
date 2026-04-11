library(shinytest2)

test_that("{shinytest2} recording: standard process - categorization", {
  skip_if_bundle_validation_unavailable()

  app <- kwallm_app_driver(
    name = "standard process - categorization",
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
  app$set_inputs(`research_background-research_background` = "no clue!")
  app$set_inputs(`categories-fields-field1` = "Positive")
  app$set_inputs(`categories-fields-field2` = "Negative")
  wait_for_bound_input(app, "categories-fields-toggleEdit")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  set_fake_models(app)

  # Set writing paragraphs toggle
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

  # Start processing
  wait_for_bound_input(app, "processing-process")
  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
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
  expect_false(any(grepl(
    "kennispunttwente.nl",
    app$get_value(export = "text_management-texts__preprocessed"),
    fixed = TRUE
  )))

  # Expect that all categories are present as columns in results
  expect_true(all(c("Positive", "Negative") %in% colnames(results)))
  # Expect that all category columns are logical
  expect_true(all(sapply(results[c("Positive", "Negative")], is.logical)))

  results_by_text <- results[match(texts, results$text), , drop = FALSE]
  expect_identical(
    results_by_text$Negative,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_true(all(rowSums(results[c("Positive", "Negative")]) > 0))

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "categorization",
    expected_sheet_names = c(
      "metadata",
      "results",
      "labels",
      "assignments",
      "categorization_response_status"
    ),
    expected_results_columns = c("text", "Positive", "Negative"),
    expected_result_rows = nrow(results),
    expected_texts = texts,
    expected_text_count = length(texts)
  )

  expect_identical(bundle$metadata$research_background, "no clue!")
  expect_length(bundle$metadata$results$labels, 2L)
  expect_setequal(
    vapply(
      bundle$metadata$results$labels,
      function(label) label$label_text,
      character(1)
    ),
    c("Positive", "Negative")
  )

  bundle_labels <- readxl::read_xlsx(bundle$results_path, sheet = "labels")
  bundle_assignments <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "assignments"
  )
  bundle_results_by_text <- bundle$results_sheet[
    match(texts, bundle$results_sheet$text),
    ,
    drop = FALSE
  ]

  expect_gt(nrow(bundle_assignments), 0)
  expect_true(all(bundle_assignments$label_id %in% bundle_labels$label_id))
  expect_equal(bundle_results_by_text$Positive, results_by_text$Positive)
  expect_equal(bundle_results_by_text$Negative, results_by_text$Negative)
})
