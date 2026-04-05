library(shinytest2)

test_that("{shinytest2} recording: categorization with paragraphs under regex anonymization", {
  app <- AppDriver$new(
    name = "categorization with paragraphs anonymized",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_async = TRUE,
      kwallm.test_fake_llm = TRUE
    )
  )

  wait_for_text_upload_input(app)
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  app$wait_for_value(
    export = "text_management-texts__preprocessed",
    timeout = 15000
  )

  app$set_inputs(
    `research_background-research_background` = "Testing anonymized paragraphs"
  )
  app$set_inputs(`categories-fields-field1` = "Positive feedback")
  app$set_inputs(`categories-fields-field2` = "Negative feedback")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  app$wait_for_js(
    "!!document.getElementById('model-main_model')",
    timeout = 30000
  )
  app$set_inputs(`model-main_model` = "kwallm-fake-main-1024")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")

  document_texts <- app$get_value(
    export = "text_management-texts__document_text"
  )
  preprocessed_texts <- app$get_value(
    export = "text_management-texts__preprocessed"
  )

  expect_true(any(grepl("kennispunttwente.nl", document_texts, fixed = TRUE)))
  expect_false(any(grepl(
    "kennispunttwente.nl",
    preprocessed_texts,
    fixed = TRUE
  )))

  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 60000
  )

  expect_true(isTRUE(app$get_value(export = "processing-processing")))
  expect_true(isTRUE(app$get_value(export = "processing-success")))

  app$wait_for_value(
    export = "processing-paragraph_entries",
    timeout = 10000
  )

  results <- app$get_value(export = "processing-results_table")
  paragraphs <- app$get_value(export = "processing-paragraph_entries")

  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))
  expect_true(length(paragraphs) > 0)
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)
  expect_true(is.numeric(paragraphs[[1]]$analysis_unit_ids))
  expect_true(length(paragraphs[[1]]$analysis_unit_ids) > 0)
  expect_identical(
    length(paragraphs[[1]]$analysis_unit_ids),
    length(paragraphs[[1]]$texts)
  )

  app$stop()
})
