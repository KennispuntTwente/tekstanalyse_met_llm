library(testthat)
library(shiny)
library(shinyjs)

source(here::here("R", "module_input_text_management.R"))

# Stub gliner_server so text_management_server can be tested without Python/GLiNER.
# The module only uses $done and $anonymized_texts in the compute step,
# and optionally $start in a modal trigger.
gliner_server <- function(id, pii_texts, lang, gliner_model) {
  reactiveValues(
    done = FALSE,
    anonymized_texts = NULL,
    pii_label_counts = NULL,
    start = function() invisible(NULL)
  )
}


test_that("text_management_server: errors if all anonymization methods disabled", {
  withr::local_options(list(
    anonymization__none = FALSE,
    anonymization__regex = FALSE,
    anonymization__gliner_model = FALSE
  ))

  expect_error(
    text_management_server(
      id = "tm",
      document_texts = reactiveVal(c("x")),
      gliner_model = NULL,
      processing = reactiveVal(FALSE),
      lang = make_test_lang("nl")
    ),
    "At least one anonymization method"
  )
})


test_that("text_management_server: regex default produces anonymized preprocessed texts", {
  withr::local_options(list(
    anonymization__default = "regex",
    anonymization__none = TRUE,
    anonymization__regex = TRUE,
    anonymization__gliner_model = FALSE
  ))

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      document_texts <- reactiveVal(c(
        " Mail me at bob@example.com ",
        "Call +31 6 1234 5678",
        "Postcode 1234 AB"
      ))

      texts <- text_management_server(
        id = "tm",
        document_texts = reactive(document_texts()),
        gliner_model = NULL,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(document_texts = document_texts, texts = texts, lang = lang)
    },
    {
      session$flushReact()

      expect_true(is.character(texts$preprocessed))

      joined <- paste(texts$preprocessed, collapse = "\n")
      expect_true(grepl(
        lang()$t("<< e-mailadres verwijderd >>"),
        joined,
        fixed = TRUE
      ))
      expect_true(grepl(
        lang()$t("<< (telefoon)nummer verwijderd >>"),
        joined,
        fixed = TRUE
      ))
      expect_true(grepl(
        lang()$t("<< postcode verwijderd >>"),
        joined,
        fixed = TRUE
      ))
    }
  )
})


test_that("text_management_server: fallback to 'none' when regex disabled and default is regex", {
  withr::local_options(list(
    anonymization__default = "regex",
    anonymization__none = TRUE,
    anonymization__regex = FALSE,
    anonymization__gliner_model = FALSE
  ))

  # The warning fires before moduleServer() which then errors on missing session.
  expect_warning(
    tryCatch(
      text_management_server(
        id = "tm",
        document_texts = reactiveVal(c("x")),
        gliner_model = NULL,
        processing = reactiveVal(FALSE),
        lang = make_test_lang("nl")
      ),
      error = function(e) NULL
    ),
    "Default anonymization method 'regex'"
  )
})


test_that("pre_process_texts: replaces email/phone/postcode markers", {
  lang <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  lang$set_translation_language("nl")
  out <- pre_process_texts(
    c(
      "a@b.com",
      "+31 6 1234 5678",
      "1234 AB"
    ),
    lang = lang
  )

  expect_true(any(grepl(
    lang$t("<< e-mailadres verwijderd >>"),
    out,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    lang$t("<< (telefoon)nummer verwijderd >>"),
    out,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    lang$t("<< postcode verwijderd >>"),
    out,
    fixed = TRUE
  )))
})


test_that("pre_process_texts leaves whitespace normalization to a separate helper", {
  lang <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  lang$set_translation_language("nl")

  out <- pre_process_texts(
    c("  a@b.com  "),
    lang = lang
  )

  expect_true(startsWith(out[[1]], "  "))
  expect_true(endsWith(out[[1]], "  "))
})


test_that("normalize_preprocessed_texts preserves whitespace independently", {
  out <- normalize_preprocessed_texts(c("  first\n\nsecond   third  "))

  expect_identical(out, "  first\n\nsecond   third  ")
})


test_that("text_management_server keeps structurally different texts distinct", {
  withr::local_options(list(
    anonymization__default = "none",
    anonymization__none = TRUE,
    anonymization__regex = TRUE,
    anonymization__gliner_model = FALSE
  ))

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      document_texts <- reactiveVal(c(" same\n\ntext ", "same text"))

      texts <- text_management_server(
        id = "tm",
        document_texts = reactive(document_texts()),
        gliner_model = NULL,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(texts = texts)
    },
    {
      session$flushReact()

      expect_identical(texts$document_text, c(" same\n\ntext ", "same text"))
      expect_identical(texts$preprocessed, c(" same\n\ntext ", "same text"))
      expect_identical(
        texts$analysis_units$preprocessed,
        c(" same\n\ntext ", "same text")
      )
      expect_identical(texts$df$analysis_unit_id, c(1L, 2L))
    }
  )
})


test_that("text_management_server maps duplicate rows to shared analysis units", {
  withr::local_options(list(
    anonymization__default = "none",
    anonymization__none = TRUE,
    anonymization__regex = TRUE,
    anonymization__gliner_model = FALSE
  ))

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      document_texts <- reactiveVal(c("same", "same", "other"))

      texts <- text_management_server(
        id = "tm",
        document_texts = reactive(document_texts()),
        gliner_model = NULL,
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(texts = texts)
    },
    {
      session$flushReact()

      expect_identical(texts$preprocessed, c("same", "other"))
      expect_identical(texts$analysis_units$analysis_unit_id, c(1L, 2L))
      expect_identical(texts$analysis_units$preprocessed, c("same", "other"))
      expect_identical(texts$df$document_text, c("same", "same", "other"))
      expect_identical(texts$df$analysis_unit_id, c(1L, 1L, 2L))
    }
  )
})
