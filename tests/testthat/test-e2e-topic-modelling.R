library(shinytest2)

test_that("{shinytest2} recording: standard process - topic modelling", {
  app <- AppDriver$new(
    name = "standard process - topic modelling",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
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

  # Set models
  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(3)
  app$click("llm_provider-get_models")
  app$wait_for_value(
    export = "llm_provider-available_models_openai",
  )
  models <- app$get_value(export = "llm_provider-available_models_openai")
  expect_true("gpt-4.1-nano-2025-04-14" %in% models)
  app$set_inputs(`model-main_model` = "gpt-4.1-nano-2025-04-14")
  app$set_inputs(`model-large_model` = "gpt-4.1-nano-2025-04-14")

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

  # Read results
  results <- app$get_value(export = "processing-final_results_df")

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

  # Expect that results have 'paragraphs' attribute
  expect_true("paragraphs" %in% names(attributes(results)))
  paragraphs <- attr(results, "paragraphs")
  # Expect correct paragraph structure
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.vector(paragraphs[[1]]$texts))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)

  app$stop()
})
