library(shinytest2)

test_that("{shinytest2} language toggle preserves visible input state", {
  app <- kwallm_app_driver(
    name = "language toggle preserves state",
    height = 1400,
    width = 2400,
    load_timeout = 120000,
    seed = 123
  )
  on.exit(app$stop(), add = TRUE)

  is_disabled <- function(id) {
    app$get_js(sprintf("document.getElementById('%s').disabled", id))
  }

  app$set_inputs(`language-toggle` = "en")
  wait_for_label_text(app, "categories-fields-field1", "Category")

  app$set_inputs(
    `research_background-research_background` = "Survey context",
    `analysis_name-analysis_name` = "Run A"
  )

  app$set_inputs(`text_split-toggle` = "true")
  wait_for_bound_input(app, "text_split-max_tokens")
  app$set_inputs(
    `text_split-max_tokens` = 64,
    `text_split-overlap` = 4
  )

  app$set_inputs(`categories-fields-field1` = "alpha")
  app$set_inputs(`categories-fields-field2` = "beta")
  app$click("categories-fields-toggleEdit")

  app$wait_for_value(export = "categories-fields-isEditing")
  expect_false(app$get_value(export = "categories-fields-isEditing"))
  expect_true(is_disabled("categories-fields-field1"))
  expect_true(is_disabled("categories-fields-addField"))
  expect_true(is_disabled("categories-fields-removeField"))

  app$set_inputs(`write_paragraphs_toggle-toggle` = "false")
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "false")

  app$set_inputs(`language-toggle` = "nl")
  wait_for_label_text(app, "categories-fields-field1", "Categorie")
  wait_for_bound_input(app, "text_split-max_tokens")

  expect_false(app$get_value(export = "categories-fields-isEditing"))
  expect_true(is_disabled("categories-fields-field1"))
  expect_true(is_disabled("categories-fields-addField"))
  expect_true(is_disabled("categories-fields-removeField"))
  expect_equal(
    app$get_value(input = "research_background-research_background"),
    "Survey context"
  )
  expect_equal(app$get_value(input = "analysis_name-analysis_name"), "Run A")
  expect_true(app$get_value(export = "text_split-split_toggle"))
  expect_equal(app$get_value(input = "text_split-max_tokens"), 64)
  expect_equal(app$get_value(input = "text_split-overlap"), 4)
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "false")

  app$set_inputs(`mode-mode` = "Scoren")
  wait_for_bound_input(app, "scoring-scoring_characteristic")
  app$set_inputs(`scoring-scoring_characteristic` = "clarity")

  expect_equal(app$get_value(export = "mode-mode"), "Scoren")
  expect_equal(
    app$get_value(input = "scoring-scoring_characteristic"),
    "clarity"
  )

  app$set_inputs(`language-toggle` = "en")
  wait_for_bound_input(app, "scoring-scoring_characteristic")

  expect_equal(app$get_value(export = "mode-mode"), "Scoren")
  expect_equal(
    app$get_value(input = "scoring-scoring_characteristic"),
    "clarity"
  )
  expect_equal(
    app$get_value(input = "research_background-research_background"),
    "Survey context"
  )
  expect_equal(app$get_value(input = "analysis_name-analysis_name"), "Run A")
  expect_true(app$get_value(export = "text_split-split_toggle"))
  expect_equal(app$get_value(input = "text_split-max_tokens"), 64)
  expect_equal(app$get_value(input = "text_split-overlap"), 4)
  expect_equal(app$get_value(input = "write_paragraphs_toggle-toggle"), "false")
})
