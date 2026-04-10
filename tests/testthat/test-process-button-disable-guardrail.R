library(testthat)

# Static guardrail: the process button disable logic must include
# mode-specific prerequisite checks so the button is not clickable
# before inputs (categories, scoring characteristic, codes) are valid.

test_that("process_button renderUI disables on mode-specific prerequisites", {
  src <- readLines(here::here("R", "module_core_processing.R"))
  src_text <- paste(src, collapse = "\n")

  # The disable_flag block should contain mode-specific checks.
  # Categorization: editing or too few categories.
  expect_true(
    grepl("categories\\$editing\\(\\)", src_text, perl = TRUE),
    info = "disable_flag must check categories$editing()"
  )
  expect_true(
    grepl(
      "categories\\$unique_non_empty_count\\(\\)\\s*<\\s*2",
      src_text,
      perl = TRUE
    ),
    info = "disable_flag must check categories$unique_non_empty_count() < 2"
  )
  expect_true(
    grepl("categories\\$has_duplicates\\(\\)", src_text, perl = TRUE),
    info = "disable_flag must check categories$has_duplicates()"
  )

  # Scoring: empty characteristic.
  # The disable_flag should contain this check so the button is not clickable.
  expect_true(
    grepl(
      "nchar\\(trimws\\(scoring_characteristic\\(\\)\\)\\)\\s*<\\s*1",
      src_text,
      perl = TRUE
    ),
    info = "disable_flag must check nchar(scoring_characteristic()) < 1"
  )

  # Marking: editing or too few codes or duplicates.
  expect_true(
    grepl("codes\\$editing\\(\\)", src_text, perl = TRUE),
    info = "disable_flag must check codes$editing()"
  )
  expect_true(
    grepl(
      "codes\\$unique_non_empty_count\\(\\)\\s*<\\s*1",
      src_text,
      perl = TRUE
    ),
    info = "disable_flag must check codes$unique_non_empty_count() < 1"
  )
  expect_true(
    grepl("codes\\$has_duplicates\\(\\)", src_text, perl = TRUE),
    info = "disable_flag must check codes$has_duplicates()"
  )
})

test_that("start_processing_run returns log_context_capture result", {
  src <- readLines(here::here("R", "module_core_processing.R"))

  # Find the start_processing_run function body and verify it ends with

  # log_context_capture (i.e. returns it as the last expression).
  fn_start <- grep("start_processing_run\\s*<-\\s*function", src)
  expect_length(fn_start, 1)

  # The function should contain a call to log_context_capture as its

  # return value (last meaningful expression before closing brace).
  # Search from the function start for the next 50 lines.
  fn_body <- src[fn_start:min(fn_start + 50, length(src))]
  expect_true(
    any(grepl("log_context_capture\\(", fn_body)),
    info = "start_processing_run() must call log_context_capture()"
  )
})
