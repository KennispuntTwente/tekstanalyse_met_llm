library(testthat)

source(here::here("R", "utils_sanitize.R"), local = TRUE)

# 1 Basic sanitization ---------------------------------------------------------

test_that("sanitize_filename returns clean name for normal input", {
  expect_equal(sanitize_filename("My Analysis"), "My_Analysis")
})

test_that("sanitize_filename strips forbidden characters", {
  expect_equal(
    sanitize_filename('Test / Analysis <"2026">'),
    "Test_Analysis_2026"
  )
  expect_equal(sanitize_filename("file:name*with|bad"), "filenamewithbad")
  expect_equal(sanitize_filename("back\\slash"), "backslash")
  expect_equal(sanitize_filename("pipe|char"), "pipechar")
  expect_equal(sanitize_filename("question?mark"), "questionmark")
})

test_that("sanitize_filename collapses whitespace and underscores", {
  expect_equal(sanitize_filename("  spaces  and  tabs  "), "spaces_and_tabs")
  expect_equal(sanitize_filename("a__b___c"), "a_b_c")
  expect_equal(sanitize_filename("a _ b"), "a_b")
})

test_that("sanitize_filename removes leading/trailing dots and underscores", {
  expect_equal(sanitize_filename("...hidden"), "hidden")
  expect_equal(sanitize_filename("_leading_"), "leading")
  expect_equal(sanitize_filename("._mixed._"), "mixed")
})

# 2 Empty / NULL inputs --------------------------------------------------------

test_that("sanitize_filename returns empty string for NULL", {
  expect_equal(sanitize_filename(NULL), "")
})

test_that("sanitize_filename returns empty string for empty string", {
  expect_equal(sanitize_filename(""), "")
})

test_that("sanitize_filename returns empty string for whitespace-only input", {
  expect_equal(sanitize_filename("   "), "")
})

test_that("sanitize_filename returns empty string when all chars are forbidden", {
  expect_equal(sanitize_filename('/:*?"<>|'), "")
})

# 3 Truncation -----------------------------------------------------------------

test_that("sanitize_filename truncates to max_length", {
  long_name <- paste(rep("A", 100), collapse = "")
  result <- sanitize_filename(long_name)
  expect_equal(nchar(result), 80)
  expect_equal(result, paste(rep("A", 80), collapse = ""))
})

test_that("sanitize_filename respects custom max_length", {
  result <- sanitize_filename("Hello World", max_length = 5)
  expect_equal(result, "Hello")
})

test_that("sanitize_filename trims trailing underscore after truncation", {
  # Create a string that will have an underscore at position max_length
  result <- sanitize_filename("AAAA BBBBB", max_length = 5)
  # "AAAA_BBBBB" truncated to 5 = "AAAA_", then trailing _ stripped = "AAAA"
  expect_equal(result, "AAAA")
})

# 4 Edge cases -----------------------------------------------------------------

test_that("sanitize_filename handles non-ASCII characters", {
  # Non-ASCII letters should be preserved (they're valid in filenames)
  expect_equal(sanitize_filename("Üntersuchung"), "Üntersuchung")
  expect_equal(sanitize_filename("café résumé"), "café_résumé")
})

test_that("sanitize_filename handles numeric input", {
  expect_equal(sanitize_filename(123), "123")
})

test_that("sanitize_filename handles single character", {
  expect_equal(sanitize_filename("A"), "A")
})

test_that("sanitize_filename handles name at exactly max_length", {
  name_80 <- paste(rep("X", 80), collapse = "")
  expect_equal(sanitize_filename(name_80), name_80)
  expect_equal(nchar(sanitize_filename(name_80)), 80)
})
