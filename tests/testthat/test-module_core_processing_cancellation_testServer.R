library(testthat)
library(shiny)

test_that("button-driven cancellation triggers onStop interrupt cleanup", {
  cancel_clicks <- 0L
  cancel_confirms <- 0L
  reload_count <- 0L
  interrupt_messages <- character()
  destroy_count <- 0L

  shiny::testServer(
    function(input, output, session) {
      processing <- reactiveVal(FALSE)
      interrupter <- list(
        interrupt = function(message = "Interrupted") {
          interrupt_messages <<- c(interrupt_messages, message)
          invisible(NULL)
        },
        destroy = function() {
          destroy_count <<- destroy_count + 1L
          invisible(NULL)
        }
      )

      shiny::onStop(function() {
        try(
          {
            interrupter$interrupt(
              "Shiny session was stopped (`shiny::onStop()`)"
            )
            interrupter$destroy()
          },
          silent = TRUE
        )
      })

      session$reload <- function() {
        reload_count <<- reload_count + 1L
        session$close()
      }

      observeEvent(input$start, {
        processing(TRUE)
      })

      observeEvent(input$cancel, {
        req(isTRUE(processing()))
        cancel_clicks <<- cancel_clicks + 1L
      })

      observeEvent(input$confirm_cancel, {
        req(isTRUE(processing()))
        cancel_confirms <<- cancel_confirms + 1L
        session$reload()
      })

      NULL
    },
    {
      session$setInputs(start = 1)

      session$setInputs(cancel = 1)
      session$setInputs(confirm_cancel = 1)
      later::run_now(timeout = 0)

      expect_identical(cancel_clicks, 1L)
      expect_identical(cancel_confirms, 1L)
      expect_identical(reload_count, 1L)
      expect_true(isTRUE(session$isEnded()))
      expect_identical(destroy_count, 1L)
      expect_length(interrupt_messages, 1L)
      expect_match(
        interrupt_messages,
        "Shiny session was stopped",
        fixed = TRUE
      )
      expect_match(
        interrupt_messages,
        "shiny::onStop",
        fixed = TRUE
      )
    }
  )
})
