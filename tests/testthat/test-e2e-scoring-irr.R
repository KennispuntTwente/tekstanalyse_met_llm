library(shinytest2)

test_that("{shinytest2} recording: scoring with inter-rater reliability", {
  skip_if_bundle_validation_unavailable()

  temp_txt <- tempfile(fileext = ".txt")
  texts <- c("Routine note one.", "Routine note two.")
  writeLines(texts, temp_txt, useBytes = TRUE)

  app <- kwallm_app_driver(
    name = "scoring with inter-rater reliability",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_fake_llm = TRUE
    )
  )
  on.exit(app$stop(), add = TRUE)
  on.exit(unlink(temp_txt), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_txt)

  app$set_inputs(
    `text_management-select_none` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  wait_for_export(
    app,
    export = "text_management-anonymization_mode",
    predicate = function(x) identical(x, "none"),
    timeout = 10000,
    description = "anonymization mode none"
  )

  app$set_inputs(`mode-mode` = "Scoren")
  app$set_inputs(`scoring-scoring_characteristic` = "Positive sentiment")

  set_fake_models(app)

  wait_for_bound_input(app, "interrater_toggle-toggle")
  app$set_inputs(`interrater_toggle-toggle` = "true")

  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")

  wait_for_modal(app, modal_id = "interrater_modal", timeout = 30000)
  wait_for_bound_input(app, "processing-rater_modal-sample_type")
  app$set_inputs(`processing-rater_modal-sample_type` = "abs")
  app$set_inputs(`processing-rater_modal-sample_abs` = 2)
  wait_for_enabled_element(app, "processing-rater_modal-confirm_sample_start")
  app$click("processing-rater_modal-confirm_sample_start")

  wait_for_bound_input(app, "processing-rater_modal-current_rating")
  app$set_inputs(`processing-rater_modal-current_rating` = 50)
  wait_for_enabled_element(app, "processing-rater_modal-submit_next")
  app$click("processing-rater_modal-submit_next")

  wait_for_bound_input(app, "processing-rater_modal-current_rating")
  app$set_inputs(`processing-rater_modal-current_rating` = 50)
  wait_for_enabled_element(app, "processing-rater_modal-submit_next")
  app$click("processing-rater_modal-submit_next")

  app$wait_for_value(
    export = "processing-success",
    timeout = 60000
  )

  results <- app$get_value(export = "processing-results_table")

  expect_identical(results$text, texts)
  expect_true("result" %in% names(results))
  expect_true(is.numeric(results$result))
  expect_identical(results$result, c(50, 50))
  expect_false(isTRUE(app$get_js(
    "!!document.querySelector(\"[data-kwallm-modal-id='interrater_modal']\")"
  )))

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "scoring",
    expected_sheet_names = c("metadata", "results", "scores", "reliability"),
    expected_results_columns = c("text", "result"),
    expected_result_rows = nrow(results),
    expected_texts = texts,
    expected_text_count = length(texts)
  )

  expect_identical(
    bundle$metadata$mode_config$scoring_characteristic,
    "Positive sentiment"
  )
  expect_false(is.null(bundle$metadata$reliability))
  expect_equal(
    vapply(
      bundle$metadata$results$scores,
      function(score_row) as.numeric(score_row$score),
      numeric(1)
    ),
    c(50, 50)
  )

  bundle_reliability <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "reliability"
  )
  expect_gt(nrow(bundle_reliability), 0)
  expect_true(
    "subjects" %in% names(bundle_reliability),
    label = "reliability sheet must contain subjects column"
  )
  expect_true(
    !anyNA(bundle_reliability$subjects),
    label = "subjects must not be NA"
  )
})
