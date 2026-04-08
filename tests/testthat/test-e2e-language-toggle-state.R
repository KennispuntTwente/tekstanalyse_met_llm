library(shinytest2)

test_that("{shinytest2} language toggle preserves visible input state", {
  app <- kwallm_app_driver(
    name = "language toggle preserves state",
    height = 1400,
    width = 2400,
    load_timeout = 30000,
    seed = 123
  )

  is_disabled <- function(id) {
    app$get_js(sprintf("document.getElementById('%s').disabled", id))
  }

  app$set_inputs(`language-toggle` = "en")
  wait_for_label_text(app, "categories-fields-field1", "Category")

  app$set_inputs(`categories-fields-field1` = "alpha")
  app$set_inputs(`categories-fields-field2` = "beta")
  app$click("categories-fields-toggleEdit")

  app$wait_for_value(export = "categories-fields-isEditing")
  expect_false(app$get_value(export = "categories-fields-isEditing"))
  expect_true(is_disabled("categories-fields-field1"))
  expect_true(is_disabled("categories-fields-addField"))
  expect_true(is_disabled("categories-fields-removeField"))

  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "No")

  app$set_inputs(`language-toggle` = "nl")
  wait_for_label_text(app, "categories-fields-field1", "Categorie")

  expect_false(app$get_value(export = "categories-fields-isEditing"))
  expect_true(is_disabled("categories-fields-field1"))
  expect_true(is_disabled("categories-fields-addField"))
  expect_true(is_disabled("categories-fields-removeField"))
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "Nee")

  app$set_inputs(`language-toggle` = "en")
  wait_for_label_text(app, "categories-fields-field1", "Category")

  expect_false(app$get_value(export = "categories-fields-isEditing"))
  expect_true(is_disabled("categories-fields-field1"))
  expect_true(is_disabled("categories-fields-addField"))
  expect_true(is_disabled("categories-fields-removeField"))
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "No")

  app$stop()
})
