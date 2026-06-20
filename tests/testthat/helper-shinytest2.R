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
  deadline <- proc.time()[["elapsed"]] + (timeout / 1000)

  repeat {
    ready <- tryCatch(check_fn(), error = function(e) FALSE)
    if (isTRUE(ready)) {
      return(invisible(TRUE))
    }

    if (proc.time()[["elapsed"]] >= deadline) {
      testthat::fail(sprintf("Timed out waiting for %s", description))
      return(invisible(FALSE))
    }

    Sys.sleep(interval / 1000)
  }
}


wait_for_processing_success <- function(app, timeout = 60000) {
  app$wait_for_value(
    export = "processing-success",
    timeout = timeout,
    ignore = c(NULL, FALSE)
  )

  wait_for_export(
    app,
    export = "processing-results_table",
    predicate = function(x) is.data.frame(x) && nrow(x) > 0,
    timeout = timeout,
    description = "processing results table"
  )

  wait_for_download_bundle(app, timeout = timeout)
  invisible(TRUE)
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


wait_for_label_text <- function(app, for_id, expected_text, timeout = 30000) {
  selector <- sprintf("label[for=\"%s\"]", for_id)

  app$wait_for_js(
    sprintf(
      paste(
        "var el = document.querySelector(%s);",
        "!!el && (el.textContent || '').includes(%s);"
      ),
      js_string(selector),
      js_string(expected_text)
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


wait_for_input_value <- function(
  app,
  input,
  expected,
  timeout = 30000,
  interval = 100,
  description = input
) {
  wait_until(
    function() {
      identical(app$get_value(input = input), expected)
    },
    timeout = timeout,
    interval = interval,
    description = description
  )
}


skip_if_bundle_validation_unavailable <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("zip")
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not(
    isTRUE(rmarkdown::pandoc_available()),
    "download bundle validation requires pandoc"
  )
}


wait_for_download_bundle <- function(app, timeout = 60000) {
  wait_for_export(
    app,
    export = "processing-zip_file",
    predicate = function(x) {
      is.character(x) &&
        length(x) == 1L &&
        nzchar(x) &&
        file.exists(x)
    },
    timeout = timeout,
    description = "download bundle to become available"
  )
}


read_download_bundle <- function(zip_path) {
  bundle_dir <- tempfile("kwallm_bundle_")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)

  utils::unzip(zipfile = zip_path, exdir = bundle_dir)

  metadata_path <- file.path(bundle_dir, "metadata.json")
  results_path <- file.path(bundle_dir, "results.xlsx")
  report_path <- file.path(bundle_dir, "report.html")
  metadata_json <- paste(
    readLines(metadata_path, warn = FALSE),
    collapse = "\n"
  )
  metadata <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)
  sheet_names <- readxl::excel_sheets(results_path)
  metadata_sheet <- readxl::read_xlsx(results_path, sheet = "metadata")
  results_sheet <- readxl::read_xlsx(results_path, sheet = "results")
  report_html <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
  metadata_values <- stats::setNames(
    as.character(metadata_sheet$value),
    metadata_sheet$field
  )

  list(
    zip_path = zip_path,
    files = zip::zip_list(zip_path)$filename,
    bundle_dir = bundle_dir,
    metadata_path = metadata_path,
    results_path = results_path,
    report_path = report_path,
    metadata = metadata,
    sheet_names = sheet_names,
    metadata_sheet = metadata_sheet,
    metadata_values = metadata_values,
    results_sheet = results_sheet,
    report_html = report_html,
    report_size = unname(file.info(report_path)$size)
  )
}


expect_download_bundle <- function(
  app,
  expected_mode_id,
  expected_sheet_names,
  expected_results_columns,
  expected_result_rows = NULL,
  expected_texts = NULL,
  expected_text_count = NULL,
  timeout = 60000
) {
  skip_if_bundle_validation_unavailable()

  bundle <- read_download_bundle(wait_for_download_bundle(
    app,
    timeout = timeout
  ))

  testthat::expect_true(file.exists(bundle$zip_path))
  testthat::expect_setequal(
    bundle$files,
    c("metadata.json", "results.xlsx", "report.html")
  )

  testthat::expect_identical(bundle$metadata$mode_id, expected_mode_id)
  testthat::expect_length(bundle$metadata$schema_version, 1L)
  testthat::expect_true(
    is.character(bundle$metadata$run_id) &&
      length(bundle$metadata$run_id) == 1L &&
      nzchar(bundle$metadata$run_id)
  )
  testthat::expect_true(
    is.character(bundle$metadata$timestamp) &&
      length(bundle$metadata$timestamp) == 1L &&
      nzchar(bundle$metadata$timestamp)
  )

  if (!is.null(expected_text_count)) {
    testthat::expect_identical(
      as.integer(bundle$metadata$text_counts$source_documents),
      as.integer(expected_text_count)
    )
    testthat::expect_identical(
      as.integer(bundle$metadata$text_counts$documents),
      as.integer(expected_text_count)
    )
    testthat::expect_identical(
      as.integer(bundle$metadata$text_counts$analysis_units),
      as.integer(expected_text_count)
    )
  }

  testthat::expect_true(all(expected_sheet_names %in% bundle$sheet_names))
  testthat::expect_identical(
    bundle$metadata_values[["mode_id"]],
    expected_mode_id
  )

  if (!is.null(expected_text_count)) {
    testthat::expect_identical(
      bundle$metadata_values[["source_documents"]],
      as.character(expected_text_count)
    )
    testthat::expect_identical(
      bundle$metadata_values[["documents"]],
      as.character(expected_text_count)
    )
    testthat::expect_identical(
      bundle$metadata_values[["analysis_units"]],
      as.character(expected_text_count)
    )
  }

  testthat::expect_true(
    all(expected_results_columns %in% names(bundle$results_sheet))
  )

  if (!is.null(expected_result_rows)) {
    testthat::expect_identical(nrow(bundle$results_sheet), expected_result_rows)
  }

  if (!is.null(expected_texts) && "text" %in% names(bundle$results_sheet)) {
    testthat::expect_true(all(expected_texts %in% bundle$results_sheet$text))
  }

  testthat::expect_gt(bundle$report_size, 1024)
  testthat::expect_match(bundle$report_html, "<html", ignore.case = TRUE)

  bundle
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

  wait_until(
    function() {
      export_started <- tryCatch(
        isTRUE(app$get_value(export = "processing-edit_topics-started")),
        error = function(e) FALSE
      )

      modal_present <- tryCatch(
        isTRUE(app$get_js(
          "!!document.querySelector(\"[data-kwallm-modal-id='edit_topics_modal']\")"
        )),
        error = function(e) FALSE
      )

      confirm_present <- tryCatch(
        isTRUE(app$get_js(
          "!!document.getElementById(\"processing-edit_topics-confirm_topics\")"
        )),
        error = function(e) FALSE
      )

      export_started || modal_present || confirm_present
    },
    timeout = timeout,
    description = "topic edit modal to become available"
  )

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
    !identical(Sys.getenv("KWALLM_RUN_LIVE_PROVIDER_SMOKE", ""), "true"),
    paste0(
      "live-provider smoke tests are disabled unless ",
      "KWALLM_RUN_LIVE_PROVIDER_SMOKE=true"
    )
  )
  testthat::skip_if(
    !nzchar(Sys.getenv("OPENAI_API_KEY", "")),
    "live-provider smoke requires OPENAI_API_KEY"
  )
}


wait_for_text_upload_input <- function(app, timeout = 30000) {
  wait_for_bound_input(app, "text_upload-text_file", timeout = timeout)
}
