library(testthat)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)

shinyQueue <- function() {
  structure(
    list(
      consumer = list(
        start = function(millis = 50) invisible(millis),
        stop = function() invisible(NULL)
      ),
      producer = list(
        fireAssignReactive = function(...) invisible(NULL)
      )
    ),
    class = "Queue"
  )
}

AsyncInterruptor <- list(
  new = function() {
    structure(
      list(
        interrupt = function(...) invisible(NULL),
        execInterrupts = function() invisible(NULL),
        destroy = function() invisible(NULL)
      ),
      class = "AsyncInterruptor"
    )
  }
)

source(here::here("R", "component_editable_field_list.R"))
source(here::here("R", "component_card_header_with_tooltip.R"))
source(here::here("R", "utils_prompt_sanitization.R"))
source(here::here("R", "analysis_code_generation.R"))
source(here::here("R", "module_input_marking_codes.R"))


test_that(".kwallm_marking_code_generation_chunk_settings uses current context window values", {
  settings <- .kwallm_marking_code_generation_chunk_settings(list(
    max_tokens = 512,
    overlap = 24
  ))
  defaults <- .kwallm_marking_code_generation_chunk_settings(list())

  expect_identical(settings$text_size_tokens, 512)
  expect_identical(settings$overlap_size_tokens, 24)
  expect_identical(defaults$text_size_tokens, 256)
  expect_identical(defaults$overlap_size_tokens, 0)
})


test_that("marking_codes_server: save/edit cycle returns trimmed unique codes", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Markeren")
      processing <- reactiveVal(FALSE)

      # Minimal texts/context_window objects required by the module.
      texts <- reactiveValues(
        preprocessed = c("t1"),
        document_text = character()
      )
      research_background <- reactiveVal("background")
      context_window <- reactiveValues(any_fit_problem = FALSE)

      models <- reactiveValues(
        main = list(parameters = list(model = "unit-test-model")),
        large = NULL
      )

      codes <- marking_codes_server(
        id = "codes",
        mode = mode,
        processing = processing,
        texts = texts,
        research_background = research_background,
        context_window = context_window,
        llm_provider_rv = NULL,
        models = models,
        lang = lang
      )

      list(codes = codes, lang = lang)
    },
    {
      expect_true(codes$editing())
      expect_equal(codes$unique_non_empty_count(), 0)

      # marking_codes starts with a single field; add two more.
      session$setInputs(`codes-fields-addField` = 1)
      session$flushReact()
      session$setInputs(`codes-fields-addField` = 2)
      session$flushReact()

      session$setInputs(
        `codes-fields-field1` = "  Code A  ",
        `codes-fields-field2` = "Code B",
        `codes-fields-field3` = "Code B" # duplicate
      )
      session$flushReact()

      # Save
      session$setInputs(`codes-fields-toggleEdit` = 1)
      session$flushReact()

      expect_false(codes$editing())

      # texts() are trimmed+unique by editable_field_list
      expect_equal(sort(codes$texts()), sort(c("Code A", "Code B")))
      expect_equal(codes$unique_non_empty_count(), 2)

      # Back to edit
      session$setInputs(`codes-fields-toggleEdit` = 2)
      session$flushReact()
      expect_true(codes$editing())
    }
  )
})


test_that("marking_codes_server: mode other than Markeren does not error", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      mode <- reactiveVal("Categorisatie")

      codes <- marking_codes_server(
        id = "codes",
        mode = mode,
        processing = reactiveVal(FALSE),
        texts = reactiveValues(
          preprocessed = character(),
          document_text = character()
        ),
        research_background = reactiveVal(""),
        context_window = reactiveValues(any_fit_problem = FALSE),
        llm_provider_rv = NULL,
        models = reactiveValues(main = NULL, large = NULL),
        lang = lang
      )

      list(codes = codes)
    },
    {
      expect_true(is.logical(codes$editing()))
      expect_true(is.character(codes$texts()))
    }
  )
})
