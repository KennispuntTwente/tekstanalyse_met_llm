library(testthat)
library(shiny)
library(shinyjs)

# Avoid UI side effects in unit tests.
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(...) invisible(NULL)

# Deterministic sampling: always take the first n.
sample <- function(x, size, replace = FALSE, ...) {
  x[seq_len(size)]
}

source(here::here("R", "module_interrater_reliability.R"), local = TRUE)


test_that("interrater_server (Categorisatie, multi-category): computes kappa and sets done", {
  testthat::skip_if_not_installed("irr")

  rating_data <- data.frame(
    text = c("t1", "t2", "t3", "t4"),
    A = c(TRUE, TRUE, FALSE, FALSE),
    B = c(FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      irr <- interrater_server(
        id = "irr",
        rating_data = rating_data,
        text_col = "text",
        mode = "Categorisatie",
        all_categories = c("A", "B"),
        assign_multiple_categories = TRUE,
        rater1_col = "result",
        lang = lang
      )

      list(irr = irr, lang = lang)
    },
    {
      expect_false(irr$done)
      expect_null(irr$result)

      # Configure sample: take 2 items (deterministic: first two rows of long format).
      session$setInputs(`irr-sample_type` = "abs")
      session$flushReact()
      session$setInputs(`irr-sample_abs` = 2)
      session$flushReact()
      session$setInputs(`irr-confirm_sample_start` = 1)
      session$flushReact()

      # Rate 2 items.
      # For multi-category mode, Shiny radioButtons typically returns "TRUE"/"FALSE" strings.
      session$setInputs(`irr-current_rating` = "TRUE")
      session$flushReact()
      session$setInputs(`irr-submit_next` = 1)
      session$flushReact()

      session$setInputs(`irr-current_rating` = "FALSE")
      session$flushReact()
      session$setInputs(`irr-submit_next` = 2)
      session$flushReact()

      expect_true(irr$done)
      expect_true(!is.null(irr$result))

      # Basic shape of irr::kappa2 output (was coerced to list).
      expect_true(all(c("method", "value", "p.value") %in% names(irr$result)))
      expect_true(is.numeric(irr$result$value))
    }
  )
})


test_that("interrater_server (Scoren): runs paired t-test and returns summary stats", {
  testthat::skip_if_not_installed("broom")
  testthat::skip_if_not_installed("pwr")

  rating_data <- data.frame(
    text = c("t1", "t2", "t3", "t4", "t5"),
    result = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      irr <- interrater_server(
        id = "irr",
        rating_data = rating_data,
        text_col = "text",
        mode = "Scoren",
        all_categories = c("A", "B"),
        assign_multiple_categories = FALSE,
        rater1_col = "result",
        lang = lang
      )

      list(irr = irr, lang = lang)
    },
    {
      # Configure sample: take 3 items.
      session$setInputs(`irr-sample_type` = "abs")
      session$flushReact()
      session$setInputs(`irr-sample_abs` = 3)
      session$flushReact()
      session$setInputs(`irr-confirm_sample_start` = 1)
      session$flushReact()

      # Provide ratings identical to original for first 3.
      session$setInputs(`irr-current_rating` = 10)
      session$flushReact()
      session$setInputs(`irr-submit_next` = 1)
      session$flushReact()

      session$setInputs(`irr-current_rating` = 20)
      session$flushReact()
      session$setInputs(`irr-submit_next` = 2)
      session$flushReact()

      session$setInputs(`irr-current_rating` = 30)
      session$flushReact()
      session$setInputs(`irr-submit_next` = 3)
      session$flushReact()

      expect_true(irr$done)
      expect_true(!is.null(irr$result))

      # Expected fields from broom::tidy(t.test(...)) plus summary stats.
      expect_true(all(
        c("p.value", "statistic", "estimate") %in% names(irr$result)
      ))
      expect_true(all(
        c("user_mean", "llm_mean", "user_sd", "llm_sd") %in% names(irr$result)
      ))

      expect_equal(irr$result$user_mean, mean(c(10, 20, 30)))
      expect_equal(irr$result$llm_mean, mean(c(10, 20, 30)))

      # When all paired differences are zero, t.test() returns NaN.
      # The module should guard against this and produce clean numeric values.
      expect_false(is.nan(irr$result$statistic))
      expect_false(is.nan(irr$result$p.value))
      expect_equal(irr$result$statistic, 0)
      expect_equal(irr$result$p.value, 1)
    }
  )
})
