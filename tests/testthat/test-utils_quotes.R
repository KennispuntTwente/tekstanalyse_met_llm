test_that("extract_quotes_matrix finds straight and curly quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)
  txt <- 'He said "hello" and then “bye”. Also: \'cya\''
  m <- extract_quotes_matrix(txt)
  expect_true(is.matrix(m))
  expect_equal(colnames(m), c("Full Match", "Content"))
  expect_equal(nrow(m), 3)
  expect_setequal(m[, "Content"], c("hello", "bye", "cya"))
})

test_that("extract_quotes_matrix keeps apostrophes inside single-quoted text", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "'You just can't break this product'"
  m <- extract_quotes_matrix(txt)

  expect_equal(nrow(m), 1)
  expect_equal(unname(m[, "Full Match"]), txt)
  expect_equal(unname(m[, "Content"]), "You just can't break this product")
})

test_that("extract_quotes_matrix keeps curly apostrophes inside curly single quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "‘You just can’t break this product’"
  m <- extract_quotes_matrix(txt)

  expect_equal(nrow(m), 1)
  expect_equal(unname(m[, "Full Match"]), txt)
  expect_equal(unname(m[, "Content"]), "You just can’t break this product")
})

test_that("extract_quotes_matrix ignores apostrophes in plain words", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "You just can't break this product."
  m <- extract_quotes_matrix(txt)

  expect_equal(nrow(m), 0)
})

test_that("extract_quotes_matrix finds later real quote after apostrophe in prior word", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "He said it's 'pretty good' overall."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "pretty good")
})

test_that("extract_quotes_matrix handles quoted contractions and possessives", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "The slogan was 'can't fail' and the owner's copy said 'won't stop'."
  m <- extract_quotes_matrix(txt)

  expect_equal(
    unname(m[, "Content"]),
    c("can't fail", "won't stop")
  )
})

test_that("extract_quotes_matrix keeps valid single-quote markers around short tokens", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "It's rock 'n' roll and grade 'A' branding."
  m <- extract_quotes_matrix(txt)

  expect_equal(
    unname(m[, "Content"]),
    c("n", "A")
  )
})

test_that("extract_quotes_matrix handles quoted text inside punctuation wrappers", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "They called it ('can't miss')."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "can't miss")
})

test_that("extract_quotes_matrix handles quotes with internal unicode boundary spaces", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "Franse stijl: «\u00A0bonjour\u00A0» en ‹\u202Fsalut\u202F›."
  m <- extract_quotes_matrix(txt)

  expect_equal(
    unname(m[, "Content"]),
    c("\u00A0bonjour\u00A0", "\u202Fsalut\u202F")
  )
})

test_that("extract_quotes_matrix ignores measurement markers and possessive apostrophes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "The laptop has a 13\" screen, a 6' cable, and users' feedback notes."
  m <- extract_quotes_matrix(txt)

  expect_equal(nrow(m), 0)
})

test_that("extract_quotes_matrix rejects false single-quote openers before later real quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "The '90s were loud, but 'can't fail' still landed."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "can't fail")
})

test_that("extract_quotes_matrix rejects elision-style apostrophes before later real quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "'cause it worked before, but 'can't fail' was the actual quote."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "can't fail")
})

test_that("extract_quotes_matrix handles single guillemets and german low-high single quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "Use ‹alpha› here and call it ‚beta‘ there."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), c("alpha", "beta"))
})

test_that("extract_quotes_matrix handles CJK quote markers in continuous text", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "他说「你好」然后又说『再见』。"
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), c("你好", "再见"))
})

test_that("extract_quotes_matrix handles curly quotes in continuous text", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "他说“不能坏”然后走了。"
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "不能坏")
})

test_that("extract_quotes_matrix handles halfwidth and fullwidth quote markers", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "｢alpha｣ and ＂beta＂ and ＇gamma＇"
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), c("alpha", "beta", "gamma"))
})

test_that("extract_quotes_matrix skips unmatched obscure openers and still finds later valid quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- "He started «unfinished and later wrote 「done」 clearly."
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "done")
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

test_that("verify_and_decorate_quotes verifies single-quoted text with apostrophes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- "'You just can't break this product' was a recurring theme."
  supporting <- "One respondent said: You just can't break this product."
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
  expect_no_match(out, "exclamation-triangle-fill")
})

test_that("verify_and_decorate_quotes normalizes curly apostrophes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- "'You just can't break this product' was repeated."
  supporting <- "One respondent wrote: You just can’t break this product."
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes trims unicode boundary spaces inside quotes", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- "Franse stijl: «\u00A0bonjour\u00A0»."
  supporting <- "bonjour"
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes normalizes quote and dash typography", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "hello-world".'
  supporting <- "The text says hello—world with an em dash."
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes handles CJK quotes in continuous text", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- "他说「不能坏」然后走了。"
  supporting <- "有人回复说不能坏，这点很好。"
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes matches canonical-equivalent accents", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "café".'
  supporting <- "The text says cafe\u0301 very clearly."
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
})

test_that("verify_and_decorate_quotes only decorates later real quote after false opener", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- "The '90s were loud, but 'can't fail' still landed."
  supporting <- "Multiple respondents said can't fail."
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  n_success <- lengths(regmatches(
    out,
    gregexpr("check-circle-fill", out, fixed = TRUE)
  ))
  expect_equal(n_success, 1)
  expect_match(out, "can't fail")
})

test_that("extract_quotes_matrix handles latin1 encoded text", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  txt <- iconv("Il a dit «café».", from = "UTF-8", to = "latin1")
  expect_false(is.na(txt))
  m <- extract_quotes_matrix(txt)

  expect_equal(unname(m[, "Content"]), "café")
})

test_that("verify_and_decorate_quotes handles latin1 encoded supporting texts", {
  source(here::here("R", "utils_quotes.R"), local = TRUE)

  paragraph <- 'Contains "café".'
  supporting <- iconv(
    "Le texte mentionne café clairement.",
    from = "UTF-8",
    to = "latin1"
  )
  expect_false(is.na(supporting))
  out <- verify_and_decorate_quotes(
    paragraph,
    supporting_texts = supporting,
    lang = "en",
    escape_html = FALSE
  )

  expect_match(out, "check-circle-fill")
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
