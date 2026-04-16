library(shinytest2)

test_that("{shinytest2} recording: categorization with split texts and by_column", {
  skip_if_bundle_validation_unavailable()

  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    text = c(
      paste(rep("lovely product and amazing service.", 40), collapse = " "),
      paste(
        rep("bad refund and confusing delivery. not satisfied.", 40),
        collapse = " "
      )
    ),
    group = c("Positive feedback", "Negative feedback"),
    stringsAsFactors = FALSE
  )
  vroom::vroom_write(test_data, temp_csv, delim = ",")

  app <- kwallm_app_driver(
    name = "categorization with split texts and by_column",
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

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)

  wait_for_select_option(app, "text_upload-column", "text")
  app$set_inputs(`text_upload-column` = "text")

  wait_for_export(
    app,
    export = "text_management-texts__document_text",
    predicate = function(x) identical(length(x), 2L),
    timeout = 10000,
    description = "loaded CSV texts"
  )

  wait_for_bound_input(app, "text_upload-by_column")
  app$set_inputs(`text_upload-by_column` = "group")

  wait_for_bound_input(app, "text_split-toggle")
  app$set_inputs(`text_split-toggle` = "true")
  wait_for_bound_input(app, "text_split-max_tokens")
  app$set_inputs(
    `text_split-max_tokens` = 16,
    `text_split-overlap` = 0
  )
  wait_for_enabled_element(app, "text_split-split_texts", timeout = 60000)
  app$click("text_split-split_texts")

  split_rows <- wait_for_export(
    app,
    export = "text_split-split_rows",
    predicate = function(x) is.data.frame(x) && nrow(x) > nrow(test_data),
    timeout = 120000,
    description = "split rows"
  )
  expect_true(nrow(split_rows) > nrow(test_data))

  app$set_inputs(
    `research_background-research_background` = "Testing split texts and grouped uploads"
  )
  app$set_inputs(`categories-fields-field1` = "Positive feedback")
  app$set_inputs(`categories-fields-field2` = "Negative feedback")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  set_fake_models(app)

  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "false")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "false")
  app$set_inputs(`interrater_toggle-toggle` = "false")

  wait_for_enabled_element(app, "processing-process", timeout = 30000)
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 120000
  )

  results <- app$get_value(export = "processing-results_table")

  expect_true(nrow(results) > nrow(test_data))
  expect_true(all(
    c(
      "source_document_id",
      "source_document_text",
      "text",
      "result"
    ) %in%
      names(results)
  ))
  expect_identical(length(unique(results$source_document_id)), 2L)
  expect_true(all(results$result %in% test_data$group))

  expect_identical(
    unique(as.character(results$result[results$source_document_id == 1L])),
    "Positive feedback"
  )
  expect_identical(
    unique(as.character(results$result[results$source_document_id == 2L])),
    "Negative feedback"
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
      "document_groups"
    ),
    expected_results_columns = c("text", "result"),
    expected_result_rows = nrow(results),
    timeout = 120000
  )

  expect_identical(bundle$metadata$input$grouping_column, "group")
  expect_true(isTRUE(bundle$metadata$input$split_enabled))
  expect_identical(as.integer(bundle$metadata$text_counts$source_documents), 2L)
  expect_gt(
    as.integer(bundle$metadata$text_counts$documents),
    as.integer(bundle$metadata$text_counts$source_documents)
  )
  expect_gt(
    as.integer(bundle$metadata$text_counts$analysis_units),
    as.integer(bundle$metadata$text_counts$source_documents)
  )

  bundle_document_groups <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "document_groups"
  )
  expect_identical(nrow(bundle_document_groups), nrow(test_data))
  expect_setequal(bundle_document_groups$group_value, unique(test_data$group))
})
