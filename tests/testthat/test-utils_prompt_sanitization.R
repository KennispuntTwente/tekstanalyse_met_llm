library(testthat)

source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

# 1. Basic escaping of a single tag ------------------------------------------

test_that("escape_prompt_delimiters escapes a plain closing tag", {
  result <- escape_prompt_delimiters("Hello </text> world", "text")
  expect_equal(result, "Hello <\\/text> world")
})

test_that("escape_prompt_delimiters escapes multiple occurrences", {
  result <- escape_prompt_delimiters(
    "a </text> b </text> c",
    "text"
  )
  expect_equal(result, "a <\\/text> b <\\/text> c")
})

# 2. Multiple tag names -------------------------------------------------------

test_that("escape_prompt_delimiters handles multiple tag names", {
  result <- escape_prompt_delimiters(
    "x </text> y </code> z </research_background>",
    c("text", "code", "research_background")
  )
  expect_equal(
    result,
    "x <\\/text> y <\\/code> z <\\/research_background>"
  )
})

# 3. Numbered variants --------------------------------------------------------

test_that("escape_prompt_delimiters escapes numbered variants like </text 1>", {
  result <- escape_prompt_delimiters(
    "a </text 1> b </text 2> c </text>",
    "text"
  )
  expect_equal(result, "a <\\/text 1> b <\\/text 2> c <\\/text>")
})

test_that("escape_prompt_delimiters escapes numbered variant with extra spaces", {
  result <- escape_prompt_delimiters("</text  3 >", "text")
  expect_equal(result, "<\\/text  3>")
})

# 4. No-op when content has no closing tags -----------------------------------

test_that("escape_prompt_delimiters is a no-op when no closing tags present", {
  input <- "Just plain text with <text> open tags"
  result <- escape_prompt_delimiters(input, c("text", "code"))
  expect_equal(result, input)
})

test_that("escape_prompt_delimiters is a no-op for empty string", {
  result <- escape_prompt_delimiters("", "text")
  expect_equal(result, "")
})

# 5. Partial matches are not escaped ------------------------------------------

test_that("escape_prompt_delimiters does not escape partial tag names", {
  result <- escape_prompt_delimiters("</textarea> </texting>", "text")
  # These are different tag names, should NOT be escaped
  expect_equal(result, "</textarea> </texting>")
})

# 6. Input validation ---------------------------------------------------------

test_that("escape_prompt_delimiters validates inputs", {
  expect_error(escape_prompt_delimiters(123, "text"))
  expect_error(escape_prompt_delimiters(c("a", "b"), "text"))
  expect_error(escape_prompt_delimiters("text", character(0)))
})

# 7. Already-escaped content is double-escaped --------------------------------

test_that("escape_prompt_delimiters double-escapes already-escaped tags", {
  result <- escape_prompt_delimiters("<\\/text>", "text")
  # The backslash is literal in the content, not a regex escape
  # <\/text> does not match </text> so it should pass through unchanged
  expect_equal(result, "<\\/text>")
})
