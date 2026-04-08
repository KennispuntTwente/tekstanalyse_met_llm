library(testthat)

source(here::here("R", "utils_context_window.R"), local = TRUE)


test_that("get_context_window_size_in_tokens: known models map to expected sizes", {
  expect_equal(get_context_window_size_in_tokens("kwallm-fake-main-1024"), 1024)
  expect_equal(
    get_context_window_size_in_tokens("kwallm-fake-reducer-320"),
    320
  )
  expect_equal(get_context_window_size_in_tokens("gpt-4o-mini"), 128000)
  expect_equal(get_context_window_size_in_tokens("gpt-5-nano"), 400000)
  expect_equal(get_context_window_size_in_tokens("o3-mini"), 200000)
  expect_equal(get_context_window_size_in_tokens("gpt-3.5-turbo-0125"), 4096)
  expect_equal(get_context_window_size_in_tokens("gpt-4.1"), 1047576)
})


test_that("get_context_window_size_in_tokens: unknown models return NULL", {
  expect_null(get_context_window_size_in_tokens("some-unknown-model"))
})


test_that("context_window_known logic: TRUE for known models, FALSE for unknown", {
  # This mirrors the logic in module_misc_context_window.R:

  #   context_window_known <- !is.null(size)
  known_size <- get_context_window_size_in_tokens("gpt-4o-mini")
  expect_true(!is.null(known_size)) # context_window_known should be TRUE

  unknown_size <- get_context_window_size_in_tokens("some-unknown-model")
  expect_false(!is.null(unknown_size)) # context_window_known should be FALSE
})
