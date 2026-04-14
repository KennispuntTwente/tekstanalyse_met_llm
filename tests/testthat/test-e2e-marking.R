library(shinytest2)

test_that("{shinytest2} recording: standard process - marking", {
  skip_if_bundle_validation_unavailable()

  app <- kwallm_app_driver(
    name = "standard process - marking",
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

  # Enter background
  app$set_inputs(
    `research_background-research_background` = "My research background"
  )

  # Set mode
  app$set_inputs(`mode-mode` = "Mark")

  set_fake_models(app)

  # Generate codes so the workflow exercises code generation as well.
  wait_for_enabled_element(app, "marking_codes-generateCodes")
  app$click("marking_codes-generateCodes")
  generated_codes <- wait_for_nonempty_export(
    app,
    export = "marking_codes-generated_codes",
    timeout = 30000
  )
  expect_true(length(generated_codes) > 0)
  wait_for_export(
    app,
    export = "marking_codes-txt_in_fields",
    predicate = function(x) is.character(x) && any(nzchar(x)),
    timeout = 30000,
    description = "generated marking code fields"
  )

  # Force one known matching code so result assertions stay meaningful.
  app$set_inputs(`marking_codes-fields-field1` = "Product feedback")
  app$click("marking_codes-fields-toggleEdit")
  app$wait_for_value(
    export = "marking_codes-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  # Start processing
  wait_for_enabled_element(app, "processing-process")
  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 30000
  )

  # Read results
  results <- app$get_value(export = "processing-results_table")

  # Expect that columns 'text', 'chunk_text', 'code', & 'marked_text' are present
  expect_true(all(
    c("text", "chunk_text", "code", "marked_text") %in% colnames(results)
  ))
  # Marking results now include typed diagnostic/id columns; only the text-like
  # fields this test exercises must remain character vectors.
  expect_true(all(sapply(
    results[c("text", "chunk_text", "code", "marked_text")],
    is.character
  )))

  # Expect that all texts are present in column 'text'
  texts <- app$get_value(
    export = "text_management-texts__preprocessed"
  )
  expect_true(all(texts %in% results$text))

  # Expect that when marked_text is not NA, it is part of the chunk_text
  expect_true(all(
    is.na(results$marked_text) |
      mapply(grepl, pattern = results$marked_text, x = results$chunk_text)
  ))
  expect_true(!all(is.na(results$marked_text)))

  # Expect that all unique values in results$code are present in
  #   txt_in_fields of marking_codes
  codes <- c(app$get_value(export = "marking_codes-txt_in_fields"), NA)
  expect_true(all(unique(results$code) %in% codes))

  bundle <- expect_download_bundle(
    app,
    expected_mode_id = "marking",
    expected_sheet_names = c(
      "metadata",
      "results",
      "codes",
      "chunks",
      "marking_responses",
      "markings"
    ),
    expected_results_columns = c("text", "chunk_text", "code", "marked_text"),
    expected_texts = texts,
    expected_text_count = length(texts)
  )

  expect_true(length(bundle$metadata$results$codes) > 0)
  expect_true(length(bundle$metadata$results$chunks) > 0)
  expect_true(length(bundle$metadata$results$markings) > 0)

  bundle_chunks <- readxl::read_xlsx(bundle$results_path, sheet = "chunks")
  bundle_markings <- readxl::read_xlsx(
    bundle$results_path,
    sheet = "markings"
  )
  expect_gt(nrow(bundle_chunks), 0)
  expect_gt(nrow(bundle_markings), 0)
  expect_true("marked_text" %in% names(bundle_markings))
  expect_true(any(
    !is.na(bundle_markings$marked_text) & nzchar(bundle_markings$marked_text)
  ))
})
