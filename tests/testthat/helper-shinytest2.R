js_string <- function(value) {
  jsonlite::toJSON(as.character(value), auto_unbox = TRUE)
}


kwallm_app_driver <- function(..., options = list()) {
  if (is.null(options)) {
    options <- list()
  }

  shinytest2::AppDriver$new(
    ...,
    options = utils::modifyList(
      list(kwallm.test_async = TRUE),
      options
    )
  )
}


wait_until <- function(
  check_fn,
  timeout = 30000,
  interval = 100,
  description = "condition"
) {
  deadline <- Sys.time() + (timeout / 1000)

  repeat {
    ready <- tryCatch(check_fn(), error = function(e) FALSE)
    if (isTRUE(ready)) {
      return(invisible(TRUE))
    }

    if (Sys.time() >= deadline) {
      testthat::fail(sprintf("Timed out waiting for %s", description))
      return(invisible(FALSE))
    }

    Sys.sleep(interval / 1000)
  }
}


wait_for_element <- function(app, selector, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      "!!document.querySelector(%s);",
      js_string(selector)
    ),
    timeout = timeout
  )
}


wait_for_bound_input <- function(app, id, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      paste(
        "var el = document.getElementById(%s);",
        "!!el && el.classList.contains('shiny-bound-input');"
      ),
      js_string(id)
    ),
    timeout = timeout
  )
}


wait_for_enabled_element <- function(app, id, timeout = 30000) {
  app$wait_for_js(
    sprintf(
      paste(
        "var el = document.getElementById(%s);",
        "!!el && !el.disabled;"
      ),
      js_string(id)
    ),
    timeout = timeout
  )
}


wait_for_select_option <- function(app, id, value, timeout = 30000) {
  wait_for_bound_input(app, id, timeout = timeout)
  app$wait_for_js(
    sprintf(
      paste(
        "var el = document.getElementById(%s);",
        "!!el && Array.from(el.options || []).some(function(option) {",
        "  return option.value === %s;",
        "});"
      ),
      js_string(id),
      js_string(value)
    ),
    timeout = timeout
  )
}


wait_for_radio_value <- function(app, name, value, timeout = 30000) {
  selector <- sprintf("input[name='%s']:checked", name)

  app$wait_for_js(
    sprintf(
      paste(
        "var el = document.querySelector(%s);",
        "!!el && el.value === %s;"
      ),
      js_string(selector),
      js_string(value)
    ),
    timeout = timeout
  )
}


wait_for_modal <- function(
  app,
  modal_id = "edit_topics_modal",
  timeout = 30000
) {
  wait_for_element(
    app,
    sprintf("[data-kwallm-modal-id='%s']", modal_id),
    timeout = timeout
  )
}


wait_for_export <- function(
  app,
  export,
  predicate = function(x) !is.null(x),
  timeout = 30000,
  interval = 100,
  description = export
) {
  value <- NULL

  wait_until(
    function() {
      value <<- app$get_value(export = export)
      isTRUE(predicate(value))
    },
    timeout = timeout,
    interval = interval,
    description = description
  )

  value
}


wait_for_nonempty_export <- function(app, export, timeout = 30000) {
  wait_for_export(
    app,
    export = export,
    predicate = function(x) !is.null(x) && length(x) > 0,
    timeout = timeout,
    description = sprintf("non-empty export '%s'", export)
  )
}


wait_for_processing_started <- function(app, timeout = 30000) {
  wait_for_export(
    app,
    export = "processing-processing",
    predicate = isTRUE,
    timeout = timeout,
    description = "processing to start"
  )
}


wait_for_topic_edit_modal_ready <- function(app, timeout = 90000) {
  wait_for_processing_started(app, timeout = timeout)
  wait_for_modal(app, timeout = timeout)
  wait_for_enabled_element(
    app,
    "processing-edit_topics-confirm_topics",
    timeout = timeout
  )
}


pick_live_openai_model <- function(
  models,
  preferred = c(
    "gpt-4.1-nano-2025-04-14",
    "gpt-4.1-nano",
    "gpt-4.1-mini",
    "gpt-5-mini"
  )
) {
  stopifnot(length(models) > 0)

  preferred_match <- preferred[preferred %in% models]
  if (length(preferred_match) > 0) {
    return(preferred_match[[1]])
  }

  models[[1]]
}


configure_live_openai_model <- function(app, timeout = 30000) {
  app$set_inputs(
    `llm_provider-select_openai` = 0.123,
    allow_no_input_binding_ = TRUE
  )
  wait_for_enabled_element(app, "llm_provider-get_models", timeout = timeout)
  app$click("llm_provider-get_models")

  models <- wait_for_nonempty_export(
    app,
    export = "llm_provider-available_models_openai",
    timeout = timeout
  )
  chosen_model <- pick_live_openai_model(models)

  wait_for_bound_input(app, "model-main_model", timeout = timeout)
  app$set_inputs(`model-main_model` = chosen_model)

  chosen_model
}


set_fake_models <- function(
  app,
  main = "kwallm-fake-main-1024",
  large = NULL,
  timeout = 30000
) {
  wait_for_select_option(app, "model-main_model", main, timeout = timeout)
  if (is.null(large)) {
    app$set_inputs(`model-main_model` = main)
    return(invisible(main))
  }

  wait_for_select_option(app, "model-large_model", large, timeout = timeout)
  app$set_inputs(
    `model-main_model` = main,
    `model-large_model` = large
  )

  invisible(c(main = main, large = large))
}


skip_if_no_live_openai <- function() {
  testthat::skip_if(
    !nzchar(Sys.getenv("OPENAI_API_KEY", "")),
    "live-provider smoke requires OPENAI_API_KEY"
  )
}


wait_for_text_upload_input <- function(app, timeout = 30000) {
  wait_for_bound_input(app, "text_upload-text_file", timeout = timeout)
}
