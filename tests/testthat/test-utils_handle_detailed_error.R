library(testthat)

source(here::here("R", "utils_handle_detailed_error.R"), local = TRUE)


test_that("handle_detailed_error: wraps message with context", {
  h <- handle_detailed_error("Topic reduction")

  expect_true(is.function(h))

  expect_error(
    h(simpleError("nope")),
    "Topic reduction failed:\nMessage: nope",
    fixed = TRUE
  )
})
