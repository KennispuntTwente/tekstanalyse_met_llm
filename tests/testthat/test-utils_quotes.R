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

test_that("sub_fixed replaces first occurrence only", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  # Basic replacement
  expect_equal(sub_fixed("world", "universe", "hello world"), "hello universe")

  # Only replaces first occurrence
  expect_equal(
    sub_fixed("a", "X", "a banana a day"),
    "X banana a day"
  )

  # No match returns original

  expect_equal(sub_fixed("xyz", "abc", "hello world"), "hello world")

  # Empty pattern edge case
  expect_equal(sub_fixed("", "X", "hello"), "Xhello")
})

test_that("extract_quotes_matrix handles guillemets", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  # Guillemets (French style)
  txt_guillemets <- 'Il a dit «bonjour» et «au revoir».'
  m <- extract_quotes_matrix(txt_guillemets)
  expect_equal(nrow(m), 2)
  expect_setequal(m[, "Content"], c("bonjour", "au revoir"))
})

test_that("extract_quotes_matrix handles empty input and no quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  # Empty string
  m_empty <- extract_quotes_matrix("")
  expect_true(is.matrix(m_empty))
  expect_equal(nrow(m_empty), 0)

  # No quotes present
  m_none <- extract_quotes_matrix("This text has no quotes at all.")
  expect_true(is.matrix(m_none))
  expect_equal(nrow(m_none), 0)

  # Invalid input (not single string) - should warn
  expect_warning(
    m_invalid <- extract_quotes_matrix(c("a", "b")),
    "single character string"
  )
  expect_equal(nrow(m_invalid), 0)
})

test_that("verify_and_decorate_quotes handles multiple quotes correctly", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'The study found "alpha", "beta", and "gamma" results.'
  supporting <- 'The alpha result was significant. Gamma was also noted.'
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )
  # alpha and gamma should have check-circle, beta should have warning
  # Count occurrences
  n_success <- lengths(regmatches(
    out,
    gregexpr("check-circle-fill", out, fixed = TRUE)
  ))
  n_warning <- lengths(regmatches(
    out,
    gregexpr("exclamation-triangle-fill", out, fixed = TRUE)
  ))
  expect_equal(n_success, 2) # alpha and gamma
  expect_equal(n_warning, 1) # beta
})

test_that("verify_and_decorate_quotes uses Dutch tooltips when lang='nl'", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Het rapport meldt "resultaat".'
  supporting <- 'resultaat was goed'
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "nl",
    escape_html = FALSE
  )
  # Dutch tooltip should be present
  expect_match(out, "Quote geverifieerd")
})

test_that("verify_and_decorate_quotes handles NA supporting_texts (no error)", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "alpha".'
  expect_warning(
    expect_error(
      out <- verify_and_decorate_quotes(
        paragraph,
        supporting_texts = NA_character_,
        lang = "en",
        escape_html = FALSE
      ),
      NA
    ),
    NA
  )

  # With no supporting texts, quote should be marked missing
  expect_match(out, "exclamation-triangle-fill")
})

test_that("verify_and_decorate_quotes does not call str_detect with empty patterns", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  # After cleaning trailing punctuation/symbols, this becomes an empty query.
  paragraph <- 'He said "!!!".'
  supporting <- "some text"
  expect_warning(
    expect_error(
      out <- verify_and_decorate_quotes(
        paragraph,
        supporting_texts = supporting,
        lang = "en",
        escape_html = FALSE
      ),
      NA
    ),
    NA
  )
  expect_match(out, "exclamation-triangle-fill")
})

test_that("verify_and_decorate_quotes drops NA within supporting_texts vectors", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'He said "hello".'
  supporting <- c(NA_character_, "Hello world")
  expect_error(
    out <- verify_and_decorate_quotes(
      paragraph,
      supporting_texts = supporting,
      lang = "en",
      escape_html = FALSE
    ),
    NA
  )
  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes does not verify across supporting text boundaries", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "alpha beta".'
  supporting <- c("alpha", "beta")
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_no_match(out, "check-circle-fill")
  expect_match(out, "exclamation-triangle-fill")
})

test_that("verify_and_decorate_quotes preserves internal whitespace during verification", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "alpha   beta".'
  supporting <- "alpha beta"
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_no_match(out, "check-circle-fill")
  expect_match(out, "exclamation-triangle-fill")
})
