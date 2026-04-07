library(shinytest2)

test_that("{shinytest2} recording: topic modelling auto-confirms in single-label mode", {
  skip_if_bundle_validation_unavailable()

  temp_txt <- tempfile(fileext = ".txt")
  texts <- c(
    "Invoice refund issue remained unresolved.",
    "Support reply was late and generic.",
    "Product quality felt broken and unreliable.",
    "App navigation was confusing for new users."
  )
  writeLines(texts, temp_txt, useBytes = TRUE)

  app <- kwallm_app_driver(
    name = "topic modelling auto-confirm single-label",
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
    `research_background-research_background` = "Topic auto-confirm test"
  )
  app$set_inputs(`mode-mode` = "Topic extraction")

  set_fake_models(
    app,
    main = "kwallm-fake-main-1024",
    large = "kwallm-fake-reducer-320"
  )

  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "No")
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-toggle` = "No")

  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 60000
  )

  results <- app$get_value(export = "processing-results_table")
  topic_columns <- names(results)[vapply(results, is.logical, logical(1))]

  expect_true("analysis_unit_id" %in% names(results))
  expect_true("result" %in% names(results))
  expect_true(is.character(results$result))
  expect_true(all(nzchar(results$result)))
  expect_identical(length(topic_columns), 0L)
  expect_null(app$get_value(export = "processing-paragraph_entries"))
  expect_false(isTRUE(app$get_js(
    "!!document.querySelector(\"[data-kwallm-modal-id='edit_topics_modal']\")"
  )))

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "topic_extraction",
    expected_sheet_names = c(
      "metadata",
      "results",
      "labels",
      "assignments",
      "topic_generation_settings"
    ),
    expected_results_columns = c("text", "result"),
    expected_result_rows = nrow(results),
    expected_texts = texts,
    expected_text_count = length(texts)
  )

  expect_false(isTRUE(bundle$metadata$mode_config$human_in_the_loop))
  expect_false(isTRUE(bundle$metadata$results$multi_label))
  expect_true(length(bundle$metadata$results$labels) > 0)
})
