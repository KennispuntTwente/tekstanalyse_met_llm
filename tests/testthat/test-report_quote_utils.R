test_that("extract_quotes_matrix finds straight and curly quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)
  txt <- 'He said "hello" and then “bye”. Also: \'cya\''
  m <- extract_quotes_matrix(txt)
  expect_true(is.matrix(m))
  expect_equal(colnames(m), c("Full Match", "Content"))
  expect_equal(nrow(m), 3)
  expect_setequal(m[, "Content"], c("hello", "bye", "cya"))
})

test_that("verify_and_decorate_quotes decorates found/missing with icons (en)", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)
  paragraph <- 'Contains "alpha" and "beta".'
  supporting <- 'The document mentions Alpha clearly.'
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )
  # alpha present (case-insensitive)
  expect_match(out, "check-circle-fill")
  # beta missing
  expect_match(out, "exclamation-triangle-fill")
  # icons wrapped in <sup>
  expect_match(out, "<sup>.*check-circle-fill.*</sup>")
})

test_that("verify_and_decorate_quotes escapes remainder when requested", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)
  paragraph <- 'Text with <b>tag</b> and "safe".'
  supporting <- 'safe'
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = TRUE
  )
  # remainder should be escaped
  expect_match(out, "&lt;b&gt;tag&lt;/b&gt;")
  # placeholder replacement should still insert raw HTML for icon
  expect_match(out, "<sup>.*check-circle-fill.*</sup>")
})
