# Additional tests for analysis_marking.R
# Focused on best_literal_substring() helper and edge cases

library(testthat)

source(here::here("R", "analysis_marking.R"))

# Tests for best_literal_substring helper
test_that("best_literal_substring handles exact match", {
  result <- best_literal_substring(
    needle = "hello world",
    haystack = "The greeting was hello world today"
  )

  expect_equal(result$match, "hello world")
  expect_equal(result$distance, 0L)
  expect_true(result$start > 0)
  expect_true(result$end >= result$start)
})

test_that("best_literal_substring handles case differences via normalization", {
  result <- best_literal_substring(
    needle = "HELLO WORLD",
    haystack = "The greeting was hello world today"
  )

  expect_equal(result$match, "hello world")
  expect_equal(result$distance, 0L)
})

test_that("best_literal_substring handles curly quotes", {
  result <- best_literal_substring(
    needle = '"hello"',
    haystack = 'She said "hello" to everyone'
  )

  expect_false(is.na(result$match))
  expect_true(grepl("hello", result$match))
})

test_that("best_literal_substring handles em-dash normalization", {
  result <- best_literal_substring(
    needle = "one-two",
    haystack = "The combination one—two was used"
  )

  expect_false(is.na(result$match))
  expect_true(grepl("one", result$match))
})

test_that("best_literal_substring returns NA for no match", {
  result <- best_literal_substring(
    needle = "xyz completely different",
    haystack = "The quick brown fox jumps"
  )

  expect_true(is.na(result$match))
  expect_true(is.na(result$distance))
  expect_true(is.na(result$start))
  expect_true(is.na(result$end))
})

test_that("best_literal_substring handles NA needle", {
  result <- best_literal_substring(
    needle = NA_character_,
    haystack = "some text"
  )

  expect_true(is.na(result$match))
})

test_that("best_literal_substring handles empty needle", {
  result <- best_literal_substring(
    needle = "",
    haystack = "some text"
  )

  expect_true(is.na(result$match))
})

test_that("best_literal_substring handles empty haystack", {
  result <- best_literal_substring(
    needle = "test",
    haystack = ""
  )

  expect_true(is.na(result$match))
})

test_that("best_literal_substring handles whitespace normalization", {
  result <- best_literal_substring(
    needle = "hello world",
    haystack = "greeting: hello   world today"
  )

  expect_false(is.na(result$match))
  # The match should come from the original haystack with multiple spaces
  expect_true(grepl("hello\\s+world", result$match))
})

test_that("best_literal_substring finds fuzzy match within threshold", {
  # With default rel=0.12, for a 10-char needle, threshold = max(2, ceil(1.2)) = 2
  # So 1-2 character differences should match
  result <- best_literal_substring(
    needle = "abcdefghij",
    haystack = "prefix abcxefghij suffix" # 1 substitution: d->x
  )

  expect_false(is.na(result$match))
  expect_true(result$distance <= 2)
})

test_that("best_literal_substring rejects fuzzy match beyond threshold", {
  # For a 10-char needle, threshold = 2
  # 3+ substitutions should fail
  result <- best_literal_substring(
    needle = "abcdefghij",
    haystack = "prefix xxxdefghij suffix" # 3 substitutions
  )

  expect_true(is.na(result$match))
})

# Test for multiple potential matches - should prefer exact
test_that("best_literal_substring prefers exact match over fuzzy", {
  result <- best_literal_substring(
    needle = "hello",
    haystack = "hallo there, hello world, hullo again"
  )

  expect_equal(result$match, "hello")
  expect_equal(result$distance, 0L)
})

# Test start/end positions are correct
test_that("best_literal_substring returns correct positions", {
  haystack <- "prefix hello suffix"
  needle <- "hello"

  result <- best_literal_substring(needle, haystack)

  expect_equal(result$start, 8L) # "hello" starts at position 8

  expect_equal(result$end, 12L) # "hello" ends at position 12
  expect_equal(substr(haystack, result$start, result$end), "hello")
})
