library(shinytest2)

test_that("{shinytest2} recording: topic modelling - horizontal mode", {
  app <- kwallm_app_driver(
    name = "topic modelling - horizontal mode",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_fake_llm = TRUE
    )
  )
  on.exit(app$stop(), add = TRUE)

  # Switch to horizontal/sections mode
  app$set_inputs(kwallm_layout_view = "sections")
  wait_for_radio_value(app, "kwallm_sections_step", "1")

  # ---- Section 1: Texts ----
  # Verify we start on section 1
  expect_equal(app$get_value(input = "kwallm_sections_step"), "1")

  # Upload texts
  wait_for_text_upload_input(app)
  app$upload_file(
    `text_upload-text_file` = here::here(
      "tests",
      "testthat",
      "test_texts.txt"
    )
  )

  # Navigate to section 2 using next button
  app$click("kwallm_sections_next")
  wait_for_radio_value(app, "kwallm_sections_step", "2")

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
  wait_for_radio_value(app, "kwallm_sections_step", "3")

  # ---- Section 3: Analysis ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "3")

  # Set analysis options
  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")

  # Navigate to section 4 using next button
  app$click("kwallm_sections_next")
  wait_for_radio_value(app, "kwallm_sections_step", "4")

  # ---- Section 4: LLM & Context ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "4")

  # Set deterministic fake models
  set_fake_models(
    app,
    main = "kwallm-fake-main-1024",
    large = "kwallm-fake-reducer-320"
  )

  # Test back navigation - go back to section 3 and then forward again
  app$click("kwallm_sections_prev")
  wait_for_radio_value(app, "kwallm_sections_step", "3")
  expect_equal(app$get_value(input = "kwallm_sections_step"), "3")

  # Navigate directly to section 5 using step button
  app$set_inputs(kwallm_sections_step = "5")
  wait_for_radio_value(app, "kwallm_sections_step", "5")

  # ---- Section 5: Run ----
  expect_equal(app$get_value(input = "kwallm_sections_step"), "5")

  # Set remaining options
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "Yes")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-interrater_reliability` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "Yes")

  # Start processing
  app$click("processing-process")
  wait_for_topic_edit_modal_ready(app)
  app$click("processing-edit_topics-confirm_topics")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
  )

  expect_true(isTRUE(app$get_value(export = "processing-processing")))
  expect_true(isTRUE(app$get_value(export = "processing-success")))

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

  expect_true("analysis_unit_id" %in% names(results))
  topic_columns <- names(results)[vapply(results, is.logical, logical(1))]

  expect_true(length(topic_columns) > 0)
  # Expect that all texts are categorized in at least one topic
  expect_true(all(rowSums(results[topic_columns]) > 0))

  # Expect correct paragraph structure
  expect_true(is.character(paragraphs[[1]]$paragraph))
  expect_true(is.logical(paragraphs[[1]]$prompt_fits))
  expect_true(is.vector(paragraphs[[1]]$texts))
  expect_true(is.character(paragraphs[[1]]$texts))
  expect_true(length(paragraphs[[1]]$texts) > 0)
  expect_true(is.numeric(paragraphs[[1]]$analysis_unit_ids))
  expect_identical(
    length(paragraphs[[1]]$analysis_unit_ids),
    length(paragraphs[[1]]$texts)
  )
})
