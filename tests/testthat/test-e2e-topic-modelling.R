library(shinytest2)

test_that("{shinytest2} recording: standard process - topic modelling", {
  app <- AppDriver$new(
    name = "standard process - topic modelling",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_async = TRUE,
      kwallm.test_fake_llm = TRUE
    )
  )

  # Upload texts
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "My research background"
  )

  # Set mode
  app$set_inputs(`mode-mode` = "Topic extraction")

  # Set deterministic fake models
  app$wait_for_js(
    "!!document.getElementById('model-main_model') && !!document.getElementById('model-large_model')",
    timeout = 30000
  )
  app$set_inputs(`model-main_model` = "kwallm-fake-main-1024")
  app$set_inputs(`model-large_model` = "kwallm-fake-reducer-320")

  # Set analysis options
  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "Yes")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-interrater_reliability` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")

  # Start processing
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-edit_topics-started",
    timeout = 60000
  )
  Sys.sleep(3)
  app$click("processing-edit_topics-confirm_topics")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
  )

  # Confirm results
  app$expect_values(
    export = c(
      # Text upload & processing works
      "text_management-anonymization_mode",
      "text_management-texts__raw",
      "text_management-texts__preprocessed",
      "text_management-texts__df",

      # Processing was successful
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
  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))
  expect_true(all.equal(
    table(texts),
    table(results$text),
    check.attributes = FALSE
  ))

  # Expect that at least 1 other column is present (topic column)
  expect_true(ncol(results) > 1)
  # Expect that all columns besides 'text' are logical
  expect_true(all(sapply(results[-1], is.logical)))
  # Expect that all texts are categorized in at least one topic
  expect_true(all(rowSums(results[-1]) > 0))

  # Expect correct paragraph structure
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.vector(paragraphs[[1]]$texts))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)

  app$stop()
})
