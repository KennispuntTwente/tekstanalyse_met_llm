library(testthat)

# Static guardrail: the process button disable logic must include
# mode-specific prerequisite checks so the button is not clickable
# before inputs (categories, scoring characteristic, codes) are valid.
# The checks now live in processing_active_blockers() and each blocker
# must have a corresponding translated user-facing message.

test_that("processing_active_blockers covers all required blocker keys", {
  src <- readLines(here::here("R", "utils_processing_helpers.R"))
  src_text <- paste(src, collapse = "\n")

  # The function must exist.
  expect_true(
    grepl(
      "processing_active_blockers\\s*<-\\s*function",
      src_text,
      perl = TRUE
    ),
    info = "processing_active_blockers() must be defined in utils_processing_helpers.R"
  )

  # Every expected blocker key must appear as a string literal in the function.
  expected_keys <- c(
    "no_texts",
    "models_missing",
    "context_overflow",
    "too_many_batches",
    "gliner_pending",
    "split_in_progress",
    "categories_editing",
    "categories_too_few",
    "categories_duplicates",
    "scoring_empty",
    "codes_editing",
    "codes_too_few",
    "codes_duplicates"
  )

  for (key in expected_keys) {
    expect_true(
      grepl(paste0('"', key, '"'), src_text, fixed = TRUE),
      info = paste0('processing_active_blockers must include key "', key, '"')
    )
  }

  # The predicates themselves must still appear in the function body.
  expect_true(
    grepl("categories\\$editing\\(\\)", src_text, perl = TRUE),
    info = "processing_active_blockers must check categories$editing()"
  )
  expect_true(
    grepl(
      "categories\\$unique_non_empty_count\\S*\\s*<\\s*2",
      src_text,
      perl = TRUE
    ),
    info = "processing_active_blockers must check categories count < 2"
  )
  expect_true(
    grepl("categories\\$has_duplicates\\(\\)", src_text, perl = TRUE),
    info = "processing_active_blockers must check categories$has_duplicates()"
  )
  expect_true(
    grepl("nchar\\(trimws\\(sc\\)\\)\\s*<\\s*1", src_text, perl = TRUE),
    info = "processing_active_blockers must check nchar(scoring) < 1"
  )
  expect_true(
    grepl("codes\\$editing\\(\\)", src_text, perl = TRUE),
    info = "processing_active_blockers must check codes$editing()"
  )
  expect_true(
    grepl(
      "codes\\$unique_non_empty_count\\S*\\s*<\\s*1",
      src_text,
      perl = TRUE
    ),
    info = "processing_active_blockers must check codes count < 1"
  )
  expect_true(
    grepl("codes\\$has_duplicates\\(\\)", src_text, perl = TRUE),
    info = "processing_active_blockers must check codes$has_duplicates()"
  )
})

test_that("process_button renderUI uses processing_active_blockers", {
  src <- readLines(here::here("R", "module_core_processing.R"))
  src_text <- paste(src, collapse = "\n")

  expect_true(
    grepl("processing_active_blockers\\(", src_text, perl = TRUE),
    info = "process_button renderUI must call processing_active_blockers()"
  )
})

test_that("all blocker keys have translation entries", {
  lang_data <- jsonlite::fromJSON(
    here::here("language", "language.json"),
    simplifyDataFrame = FALSE
  )
  translations <- lang_data$translation

  nl_values <- vapply(translations, function(t) t$nl %||% "", character(1))

  # These are the NL translation strings used as keys in processing_active_blockers.
  expected_nl <- c(
    "Geen teksten ge\u00fcpload",
    "Geen model geselecteerd",
    "Sommige teksten overschrijden het context-window",
    "Te veel batches voor onderwerpextractie",
    "GLiNER-anonimisering nog niet voltooid",
    "Teksten worden gesplitst",
    "Sla de categorie\u00ebn eerst op",
    "Minimaal 2 categorie\u00ebn vereist",
    "Verwijder dubbele categorie\u00ebn",
    "Vul een scoringskenmerk in",
    "Sla de codes eerst op",
    "Minimaal 1 code vereist",
    "Verwijder dubbele codes"
  )

  for (nl_key in expected_nl) {
    expect_true(
      nl_key %in% nl_values,
      info = paste0(
        "Translation missing for blocker message: \"",
        nl_key,
        "\""
      )
    )
  }
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
