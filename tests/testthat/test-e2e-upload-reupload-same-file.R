library(shinytest2)

test_that("{shinytest2} reuploading the same file refreshes upload state", {
  temp_csv <- tempfile(pattern = "same-file-upload-", fileext = ".csv")
  first_data <- data.frame(
    text = c("Alpha", "Beta"),
    group = c("G1", "G2"),
    stringsAsFactors = FALSE
  )
  second_data <- data.frame(
    text = c("Delta", "Epsilon", "Zeta"),
    group = c("H1", "H1", "H2"),
    stringsAsFactors = FALSE
  )

  vroom::vroom_write(first_data, temp_csv, delim = ",")
  on.exit(unlink(temp_csv), add = TRUE)

  app <- AppDriver$new(
    name = "same file reupload refreshes upload state",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)
  app$set_inputs(`text_upload-column` = "text")
  app$wait_for_value(
    export = "text_management-texts__document_text",
    timeout = 10000,
    ignore = c(NULL)
  )
  app$set_inputs(`text_upload-by_column` = "group")

  expect_equal(
    sort(app$get_value(export = "text_management-texts__document_text")),
    sort(first_data$text)
  )
  expect_true(app$get_js("!!document.getElementById('text_upload-by_column')"))
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")

  vroom::vroom_write(second_data, temp_csv, delim = ",")
  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)

  refreshed <- FALSE
  for (i in seq_len(20)) {
    Sys.sleep(0.25)
    current_texts <- sort(app$get_value(
      export = "text_management-texts__document_text"
    ))
    if (identical(current_texts, sort(second_data$text))) {
      refreshed <- TRUE
      break
    }
  }

  expect_true(refreshed)
  expect_equal(
    sort(app$get_value(export = "text_management-texts__document_text")),
    sort(second_data$text)
  )
  expect_equal(
    app$get_value(export = "text_upload-uploaded_file_name"),
    basename(temp_csv)
  )
  expect_true(app$get_js("!!document.getElementById('text_upload-column')"))
  expect_true(app$get_js("!!document.getElementById('text_upload-by_column')"))
  expect_equal(app$get_value(input = "text_upload-column"), "text")
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")

  app$stop()
})
