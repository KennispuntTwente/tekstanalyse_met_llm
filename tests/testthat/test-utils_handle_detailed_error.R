library(testthat)

source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)


test_that("kwallm_error_message omits condition calls but preserves causes", {
  provider_error <- structure(
    list(
      message = "PROVIDER_ERROR_SENTINEL",
      call = quote(onFulfilled(...))
    ),
    class = c("simpleError", "error", "condition")
  )

  expect_identical(
    kwallm_error_message(provider_error),
    "PROVIDER_ERROR_SENTINEL"
  )
  expect_identical(kwallm_error_message("plain error"), "plain error")
})


test_that("handle_detailed_error: wraps message with context", {
  h <- handle_detailed_error("Topic reduction")

  expect_true(is.function(h))

  expect_error(
    h(simpleError("nope")),
    "Topic reduction failed:\nMessage: nope",
    fixed = TRUE
  )
})
