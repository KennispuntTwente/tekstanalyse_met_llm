library(shinytest2)

test_that("{shinytest2} upload card keeps consistent state across navigation and language changes", {
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(
    text = c("Alpha", "Beta", "Gamma"),
    group = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )
  vroom::vroom_write(test_data, temp_csv, delim = ",")
  on.exit(unlink(temp_csv), add = TRUE)

  app <- AppDriver$new(
    name = "upload card preserves state",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123,
    options = list(kwallm.test_async = TRUE)
  )

  app$upload_file(`text_upload-text_file` = temp_csv)
  app$set_inputs(`text_upload-column` = "text")
  app$wait_for_value(
    export = "text_management-texts__raw",
    timeout = 10000,
    ignore = c(NULL)
  )
  app$set_inputs(`text_upload-by_column` = "group")

  expect_equal(
    app$get_value(export = "text_upload-uploaded_file_name"),
    basename(temp_csv)
  )
  expect_equal(app$get_value(input = "text_upload-column"), "text")
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")
  expect_true(app$get_js("document.body.innerText.includes('group')"))
  expect_true(app$get_js(sprintf(
    "document.body.innerText.includes('%s')",
    basename(temp_csv)
  )))

  app$set_inputs(kwallm_layout_view = "sections")
  app$set_inputs(kwallm_sections_step = "2")
  app$set_inputs(kwallm_sections_step = "1")

  expect_equal(
    app$get_value(export = "text_upload-uploaded_file_name"),
    basename(temp_csv)
  )
  expect_equal(app$get_value(input = "text_upload-column"), "text")
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")

  app$set_inputs(`language-toggle` = "nl")
  app$wait_for_js(
    sprintf(
      "!!document.getElementById('text_upload-column') && document.body.innerText.includes('%s')",
      basename(temp_csv)
    )
  )

  expect_equal(
    app$get_value(export = "text_upload-uploaded_file_name"),
    basename(temp_csv)
  )
  expect_equal(app$get_value(input = "text_upload-column"), "text")
  expect_equal(app$get_value(input = "text_upload-by_column"), "group")
  expect_true(app$get_js(sprintf(
    "document.body.innerText.includes('%s')",
    basename(temp_csv)
  )))

  app$upload_file(`text_upload-text_file` = temp_csv)
  app$wait_for_value(
    export = "text_upload-uploaded_file_name",
    timeout = 10000
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
