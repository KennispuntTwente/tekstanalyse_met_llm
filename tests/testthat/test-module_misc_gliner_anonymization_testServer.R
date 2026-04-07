library(testthat)
library(shiny)
suppressWarnings(library(promises))

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

AsyncProgress <- list(
  new = function(message = NULL, detail = NULL) {
    force(message)
    force(detail)

    list(
      inc = function(...) invisible(NULL),
      close = function() invisible(NULL)
    )
  }
)

showModal <- function(...) invisible(NULL)
log_debug <- function(...) invisible(NULL)
log_action <- function(...) invisible(NULL)
log_info <- function(...) invisible(NULL)
log_error <- function(...) invisible(NULL)
get_session_id <- function() "test-session"
log_context_capture <- function(...) list()
log_context_apply <- function(...) invisible(NULL)
log_async_globals <- function(log_context) {
  list(
    log_context = log_context,
    log_context_apply = log_context_apply
  )
}
async_message_printer <- function(...) {
  function(...) invisible(NULL)
}
initialize_python_environment <- function(...) invisible(NULL)
gliner_load_model <- function(queue = NULL) {
  force(queue)

  list(
    predict_entities = function(text, labels) {
      force(text)
      force(labels)
      list()
    }
  )
}

source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
source(here::here("R", "module_misc_gliner_anonymization.R"), local = TRUE)


test_that("gliner_server passes worker setup globals for async model loading", {
  testthat::skip_if_not_installed("mirai")

  mirai_ns <- asNamespace("mirai")

  old_mirai <- get("mirai", envir = mirai_ns)
  captured <- new.env(parent = emptyenv())

  withr::defer({
    unlockBinding("mirai", mirai_ns)
    assign("mirai", old_mirai, envir = mirai_ns)
    lockBinding("mirai", mirai_ns)
  })

  unlockBinding("mirai", mirai_ns)
  assign(
    "mirai",
    function(
      .expr,
      ...,
      .args = list(),
      .timeout = NULL,
      .compute = NULL
    ) {
      force(.timeout)
      force(.compute)

      captured$args <- c(list(...), .args)

      promises::promise(function(resolve, reject) {
        captured$resolve <- resolve
        captured$reject <- reject
      })
    },
    envir = mirai_ns
  )
  lockBinding("mirai", mirai_ns)

  withr::local_options(list(anonymization__gliner_model = TRUE))

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      pii_texts <- reactiveVal(c("Alice works at Example Corp.", "Call Bob."))

      gliner <- gliner_server(
        id = "gliner",
        pii_texts = pii_texts,
        lang = lang,
        gliner_model = NULL
      )

      list(gliner = gliner, lang = lang)
    },
    {
      gliner$start()
      session$flushReact()

      session$setInputs(`gliner-pii_labels` = "name,email")
      session$flushReact()

      session$setInputs(`gliner-start_anonymization` = 1)
      session$flushReact()

      expect_true(all(
        c(
          "gliner_load_model",
          "prepare_async_analysis_worker",
          "initialize_python_environment",
          "async_message_printer"
        ) %in%
          names(captured$args)
      ))
    }
  )
})
