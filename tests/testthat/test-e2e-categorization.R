library(shinytest2)

test_that("{shinytest2} recording: standard process - categorization", {
  app <- AppDriver$new(
    name = "standard process - categorization",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(
      kwallm.test_async = TRUE,
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

  # Enter categories
  app$set_inputs(`research_background-research_background` = "no clue!")
  app$set_inputs(`categories-fields-field1` = "Positive")
  app$set_inputs(`categories-fields-field2` = "Negative")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  set_fake_models(app)

  # Set writing paragraphs toggle
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")

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
  expect_false(any(grepl(
    "kennispunttwente.nl",
    app$get_value(export = "text_management-texts__preprocessed"),
    fixed = TRUE
  )))

  # Expect that all categories are present as columns in results
  expect_true(all(c("Positive", "Negative") %in% colnames(results)))
  # Expect that all category columns are logical
  expect_true(all(sapply(results[c("Positive", "Negative")], is.logical)))

  results_by_text <- results[match(texts, results$text), , drop = FALSE]
  expect_identical(
    results_by_text$Negative,
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_true(all(rowSums(results[c("Positive", "Negative")]) > 0))
})
