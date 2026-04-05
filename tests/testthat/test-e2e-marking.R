library(shinytest2)

test_that("{shinytest2} recording: standard process - marking", {
  app <- AppDriver$new(
    name = "standard process - marking",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )

  # Upload texts
  wait_for_text_upload_input(app)
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
  results <- app$get_value(export = "processing-results_table")

  # Expect that columns 'text', 'chunk_text', 'code', & 'marked_text' are present
  expect_true(all(
    c("text", "chunk_text", "code", "marked_text") %in% colnames(results)
  ))
  # Expect that all columns are character
  expect_true(all(sapply(results, is.character)))

  # Expect that all texts are present in column 'text'
  texts <- readLines(
    here::here("tests", "testthat", "test_texts.txt")
  )
  expect_true(all(texts %in% results$text))

  # Expect that when marked_text is not NA, it is part of the chunk_text
  expect_true(all(
    is.na(results$marked_text) |
      mapply(grepl, pattern = results$marked_text, x = results$chunk_text)
  ))

  # Expect that all unique values in results$code are present in
  #   txt_in_fields of marking_codes
  codes <- c(app$get_value(export = "marking_codes-txt_in_fields"), NA)
  expect_true(all(unique(results$code) %in% codes))

  app$stop()
})
