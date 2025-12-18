library(shinytest2)

test_that("{shinytest2} recording: standard process - marking", {
  app <- AppDriver$new(
    name = "standard process - marking",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
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
  app$set_inputs(`mode-mode` = "Mark")

  # Set model
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

  # Generate codes & save them
  app$set_inputs(`marking_codes-fields-field1` = "Product feedback")
  # app$click("marking_codes-generateCodes")
  # app$wait_for_value(
  #   export = "marking_codes-generated_codes",
  #   timeout = 15000
  # )
  Sys.sleep(3)
  app$click("marking_codes-fields-toggleEdit")
  app$wait_for_value(
    export = "marking_codes-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  # Start processing
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
  )

  # Confirm results
  app$expect_values(
    export = c(
      # Processing was successful
      "processing-processing",
      "processing-success"
    )
  )

  # Read results
  results <- app$get_value(export = "processing-final_results_df")

  # Expect that columns 'text', 'sub_text', 'code', & 'marked_text' are present
  expect_true(all(
    c("text", "sub_text", "code", "marked_text") %in% colnames(results)
  ))
  # Expect that all columns are character
  expect_true(all(sapply(results, is.character)))

  # Expect that all texts are present in column 'text'
  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))

  # Expect that when marked_text is not NA, it is part of the sub_text
  expect_true(all(
    is.na(results$marked_text) |
      mapply(grepl, pattern = results$marked_text, x = results$sub_text)
  ))

  # Expect that all unique values in results$code are present in
  #   txt_in_fields of marking_codes
  codes <- c(app$get_value(export = "marking_codes-txt_in_fields"), NA)
  expect_true(all(unique(results$code) %in% codes))

  app$stop()
})

test_that("{shinytest2} recording: standard process - topic modelling", {
  app <- AppDriver$new(
    name = "standard process - topic modelling",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
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
    timeout = 15000
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

test_that("{shinytest2} recording: standard process - scoring", {
  app <- AppDriver$new(
    name = "standard process - scoring",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Upload texts
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
  Sys.sleep(3)

  # Set scoring characteristic
  app$set_inputs(`mode-mode` = "Scoring")
  app$set_inputs(`scoring-scoring_characteristic` = "Positive sentiment")

  # Set model
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

  # Start processing
  app$click("processing-process")
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

  # Expect that column 'result' is present & numeric
  expect_true("result" %in% colnames(results))
  expect_true(is.numeric(results$result))
  # Expect that all results are between 0 and 100
  expect_true(all(results$result >= 0 & results$result <= 100))

  app$stop()
})

test_that("{shinytest2} recording: standard process - categorization", {
  app <- AppDriver$new(
    name = "standard process - categorization",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Upload texts
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  # Enter categories
  app$set_inputs(`research_background-research_background` = "no clue!")
  app$set_inputs(`categories-fields-field1` = "a")
  app$set_inputs(`categories-fields-field2` = "b")
  app$click("categories-fields-toggleEdit")

  # Set model
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

  # Set writing paragraphs toggle
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

  # Start processing
  app$click("processing-process")
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

      # Categories works
      "categories-fields-n_fields",
      "categories-fields-txt_in_fields",
      "categories-fields-isEditing",

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

  # Expect that all categories are present as columns in results
  expect_true(all(c("a", "b") %in% colnames(results)))
  # Expect that all category columns are logical
  expect_true(all(sapply(results[c("a", "b")], is.logical)))
  # Expect that all texts are categorized in at least one category
  expect_true(all(rowSums(results[c("a", "b")]) > 0))

  app$stop()
})

test_that("{shinytest2} recording: topic extraction - horizontal mode", {
  app <- AppDriver$new(
    name = "topic extraction - horizontal mode",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  # Switch to horizontal/sections mode
  app$set_inputs(kwallm_layout_view = "sections")
  Sys.sleep(1)

  # ---- Section 1: Texts ----
  # Verify we start on section 1
  expect_equal(app$get_value(input = "kwallm_sections_step"), "1")

  # Upload texts
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  # Navigate to section 2 using next button
  app$click("kwallm_sections_next")
  Sys.sleep(0.5)

  # ---- Section 2: Research & Mode ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "2")

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "My research background"
  )

  # Set mode
  app$set_inputs(`mode-mode` = "Topic extraction")

  # Navigate to section 3 using step button directly
  app$set_inputs(kwallm_sections_step = "3")
  Sys.sleep(0.5)

  # ---- Section 3: Analysis ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "3")

  # Set analysis options
  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")

  # Navigate to section 4 using next button
  app$click("kwallm_sections_next")
  Sys.sleep(0.5)

  # ---- Section 4: LLM & Context ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "4")

  # Set model
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

  # Test back navigation - go back to section 3 and then forward again
  app$click("kwallm_sections_prev")
  Sys.sleep(0.5)
  expect_equal(app$get_value(input = "kwallm_sections_step"), "3")

  # Navigate directly to section 5 using step button
  app$set_inputs(kwallm_sections_step = "5")
  Sys.sleep(0.5)

  # ---- Section 5: Run ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "5")

  # Set remaining options
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "Yes")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-interrater_reliability` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")

  # Start processing
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-edit_topics-started",
    timeout = 15000
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
