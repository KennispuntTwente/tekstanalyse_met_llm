library(shinytest2)


.paragraph_overflow_texts <- function() {
  vapply(
    seq_len(12L),
    function(i) {
      paste(
        sprintf("Great support case %02d.", i),
        paste(
          rep("The helpful reliable service resolved my question.", 30L),
          collapse = " "
        )
      )
    },
    character(1)
  )
}


.run_paragraph_overflow_e2e <- function(strategy) {
  skip_if_bundle_validation_unavailable()

  input_path <- withr::local_tempfile(fileext = ".txt")
  input_texts <- .paragraph_overflow_texts()
  writeLines(input_texts, input_path, useBytes = TRUE)

  app <- kwallm_app_driver(
    name = paste("categorization paragraph overflow", strategy),
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_fake_llm = TRUE,
      paragraph_summary_strategy = strategy
    )
  )
  on.exit(app$stop(), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = input_path)

  app$set_inputs(
    `research_background-research_background` = "Overflow e2e test"
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

  set_fake_models(app, main = "kwallm-fake-main-1024")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "true")

  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")
  wait_for_processing_success(app, timeout = 90000)

  results <- app$get_value(export = "processing-results_table")
  paragraphs <- wait_for_nonempty_export(
    app,
    export = "processing-paragraph_entries",
    timeout = 10000
  )

  expect_identical(nrow(results), length(input_texts))
  expect_true(all(results[["Positive feedback"]]))
  expect_length(paragraphs, 1L)
  expect_true(isTRUE(paragraphs[[1]]$prompt_fits))
  expect_true(nzchar(paragraphs[[1]]$paragraph))
  expect_identical(
    paragraphs[[1]]$source_coverage,
    if (strategy == "sample") {
      "sampled"
    } else {
      "complete"
    }
  )

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "categorization",
    expected_sheet_names = c(
      "metadata",
      "results",
      "stage_executions",
      "paragraphs",
      "paragraph_sources"
    ),
    expected_results_columns = c(
      "text",
      "Positive feedback",
      "Negative feedback"
    ),
    expected_result_rows = length(input_texts),
    expected_texts = input_texts,
    expected_text_count = length(input_texts),
    timeout = 90000
  )

  bundle_paragraphs <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "paragraphs"
  )
  bundle_sources <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "paragraph_sources"
  )
  executions <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "stage_executions"
  )
  paragraph_executions <- executions[
    executions$stage_id == "paragraph_generation",
    ,
    drop = FALSE
  ]

  expect_identical(
    bundle$metadata$mode_config$paragraph_summary_strategy,
    strategy
  )
  expect_identical(bundle_paragraphs$prompt_fits, TRUE)
  expect_identical(
    bundle_paragraphs$source_coverage,
    paragraphs[[1]]$source_coverage
  )
  expect_gt(nrow(paragraph_executions), 0L)

  if (strategy == "sample") {
    expect_identical(nrow(paragraph_executions), 1L)
    expect_gt(nrow(bundle_sources), 0L)
    expect_lt(nrow(bundle_sources), length(input_texts))
  } else {
    # Multiple paragraph-generation calls prove that the initial prompt did not
    # fit and the recursive batch path ran. The exported sources prove that it
    # still covered every original document.
    expect_gt(nrow(paragraph_executions), 1L)
    expect_identical(nrow(bundle_sources), length(input_texts))
    expect_setequal(bundle_sources$excerpt_text, input_texts)
  }

  invisible(NULL)
}


test_that("paragraph context overflow is summarized end to end in sample mode", {
  .run_paragraph_overflow_e2e("sample")
})


test_that("paragraph context overflow is summarized end to end in batch mode", {
  .run_paragraph_overflow_e2e("batch")
})
