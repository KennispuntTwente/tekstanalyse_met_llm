library(shinytest2)

test_that("{shinytest2} recording: standard process - scoring", {
  app <- kwallm_app_driver(
    name = "standard process - scoring",
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

  # Turn anonymization off
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

  # Set scoring characteristic
  app$set_inputs(`mode-mode` = "Scoring")
  app$set_inputs(`scoring-scoring_characteristic` = "Positive sentiment")

  set_fake_models(app)

  # Start processing
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

  # Expect that column 'result' is present & numeric
  expect_true("result" %in% colnames(results))
  expect_true(is.numeric(results$result))

  results_by_text <- results[match(texts, results$text), , drop = FALSE]
  expect_identical(results_by_text$result, c(68, 32, 50, 50, 50))

  expect_identical(
    app$get_value(export = "text_management-texts__preprocessed"),
    texts
  )
})
