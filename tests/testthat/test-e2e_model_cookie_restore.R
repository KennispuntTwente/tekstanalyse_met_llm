library(shinytest2)
library(testthat)

# This test verifies the browser cookie persistence used to remember the last
# selected model per provider (openai/ollama/preconfigured).
#
# Note: In shinytest2, the app runs in Shiny test mode which we normally treat
# as a signal to skip cookie IO to keep tests deterministic. For this one test,
# we explicitly enable cookie IO via env var inherited by the app process.

test_that("{shinytest2} remembers last selected model (per provider)", {
  Sys.setenv(KWALLM_ENABLE_COOKIE_IO_IN_TESTMODE = "1")
  on.exit(Sys.unsetenv("KWALLM_ENABLE_COOKIE_IO_IN_TESTMODE"), add = TRUE)

  app <- AppDriver$new(
    name = "model cookie restore",
    height = 1200,
    width = 2000,
    load_timeout = 30000,
    seed = 123
  )
  on.exit(app$stop(), add = TRUE)

  # Switch to OpenAI mode
  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(2)

  # Ping available models so the model selector has choices.
  app$click("llm_provider-get_models")
  app$wait_for_value(export = "llm_provider-available_models_openai")
  models <- app$get_value(export = "llm_provider-available_models_openai")

  # Keep this aligned with other e2e tests in this repo.
  model_a <- "gpt-4.1-nano-2025-04-14"
  expect_true(model_a %in% models)

  # 1) Restore behavior:
  # Set the cookie directly (simulates a previous session), then toggle away/back
  # so the model module requests the cookie again and restores it.
  app$run_js(
    sprintf(
      "kwallmSetCookie('kwallm_last_model__openai__main', '%s', 365);",
      model_a
    )
  )

  # Toggle away/back to force a cookie read + restore attempt.
  app$set_inputs(
    `llm_provider-select_preconfigured` = 0.456,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(1)
  app$set_inputs(
    `llm_provider-select_openai` = 0.789,
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(2)

  # Ensure model choices are present after toggling (and trigger cookie restore).
  app$click("llm_provider-get_models")
  app$wait_for_value(export = "llm_provider-available_models_openai")

  # Expect that the main model dropdown got auto-selected.
  app$wait_for_js(
    sprintf("$('#model-main_model').val() === '%s'", model_a),
    timeout = 10000
  )
  expect_equal(app$get_value(input = "model-main_model"), model_a)

  # 2) Save behavior:
  # If there is a second model, selecting it should update the cookie.
  if (length(models) >= 2) {
    model_b <- models[[1]]
    if (identical(model_b, model_a)) {
      model_b <- models[[2]]
    }

    app$set_inputs(`model-main_model` = model_b)
    app$wait_for_js(
      sprintf(
        "kwallmGetCookie('kwallm_last_model__openai__main') === '%s'",
        model_b
      ),
      timeout = 8000
    )
    cookie_val <- app$get_js(
      "kwallmGetCookie('kwallm_last_model__openai__main');"
    )
    expect_equal(cookie_val, model_b)
  } else {
    testthat::skip("Only one model available; skip cookie update assertion")
  }
})
