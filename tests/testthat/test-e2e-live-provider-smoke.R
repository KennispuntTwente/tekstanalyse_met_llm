library(shinytest2)

test_that("{shinytest2} live provider smoke: categorization workflow", {
  skip_if_no_live_openai()

  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("lovely product!", "bad product!"), temp_txt, useBytes = TRUE)
  on.exit(unlink(temp_txt), add = TRUE)

  app <- AppDriver$new(
    name = "live provider smoke - categorization",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )
  on.exit(app$stop(), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_txt)

  app$set_inputs(`research_background-research_background` = "Live smoke test")
  app$set_inputs(`categories-fields-field1` = "Positive")
  app$set_inputs(`categories-fields-field2` = "Negative")
  app$click("categories-fields-toggleEdit")
  app$wait_for_value(
    export = "categories-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  chosen_model <- configure_live_openai_model(app, timeout = 60000)
  expect_true(is.character(chosen_model) && nzchar(chosen_model))

  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  wait_for_enabled_element(app, "processing-process", timeout = 30000)
  app$click("processing-process")
  app$wait_for_value(export = "processing-success", timeout = 60000)

  results <- app$get_value(export = "processing-results_table")
  expect_identical(sort(results$text), c("bad product!", "lovely product!"))
  expect_true(all(c("Positive", "Negative") %in% names(results)))
  expect_true(all(rowSums(results[c("Positive", "Negative")]) > 0))
})


test_that("{shinytest2} live provider smoke: marking workflow", {
  skip_if_no_live_openai()

  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("lovely product!", "bad product!"), temp_txt, useBytes = TRUE)
  on.exit(unlink(temp_txt), add = TRUE)

  app <- AppDriver$new(
    name = "live provider smoke - marking",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )
  on.exit(app$stop(), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_txt)

  app$set_inputs(`research_background-research_background` = "Live smoke test")
  app$set_inputs(`mode-mode` = "Mark")
  chosen_model <- configure_live_openai_model(app, timeout = 60000)
  expect_true(is.character(chosen_model) && nzchar(chosen_model))

  app$set_inputs(`marking_codes-fields-field1` = "Product feedback")
  app$click("marking_codes-fields-toggleEdit")
  app$wait_for_value(
    export = "marking_codes-fields-isEditing",
    timeout = 5000,
    ignore = c(NULL, TRUE)
  )

  wait_for_enabled_element(app, "processing-process", timeout = 30000)
  app$click("processing-process")
  app$wait_for_value(export = "processing-success", timeout = 90000)

  results <- app$get_value(export = "processing-results_table")
  expect_true(all(
    c("text", "chunk_text", "code", "marked_text") %in% colnames(results)
  ))
  expect_true(is.character(results$marked_text))
  expect_true("Product feedback" %in% results$code)
  expect_true(nrow(results) >= 2)
})
