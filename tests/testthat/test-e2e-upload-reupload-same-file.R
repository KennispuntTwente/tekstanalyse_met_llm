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

  app <- kwallm_app_driver(
    name = "same file reupload refreshes upload state",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )
  on.exit(app$stop(), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)
  wait_for_select_option(app, "text_upload-column", "text")
  app$set_inputs(`text_upload-column` = "text")
  wait_for_bound_input(app, "text_upload-by_column")
  app$set_inputs(`text_upload-by_column` = "group")

  vroom::vroom_write(second_data, temp_csv, delim = ",")
  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_csv)

  wait_for_export(
    app,
    export = "text_management-texts__document_text",
    predicate = function(x) identical(sort(x), sort(second_data$text)),
    timeout = 5000,
    description = "refreshed uploaded texts"
  )
  expect_equal(
    sort(app$get_value(export = "text_management-texts__document_text")),
    sort(second_data$text)
  )
  expect_equal(
    app$get_value(export = "text_upload-uploaded_file_name"),
    basename(temp_csv)
  )
  expect_equal(app$get_value(input = "text_upload-column"), "text")
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")
})
