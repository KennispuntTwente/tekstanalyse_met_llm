# This script tests the fuzzy matching which is performed in R/analys_marking.R

library(testthat)
source(here::here("R", "analysis_marking.R"))

# Deterministic k-substitution mutator used in “property-like” tests
mutate_k_subs <- function(s, k) {
  if (k <= 0) {
    return(s)
  }
  ch <- unlist(strsplit(s, "", fixed = TRUE))
  pos <- seq(1, length(ch), length.out = min(k, length(ch)))
  pos <- unique(pmin(round(pos), length(ch)))
  for (i in pos) {
    ch[i] <- if (ch[i] != "x") "x" else "y"
  }
  paste0(ch, collapse = "")
}

test_that("fuzzy_threshold math matches spec", {
  expect_equal(fuzzy_threshold(1), 2) # max(2, ceil(0.12))
  expect_equal(fuzzy_threshold(10), 2) # max(2, ceil(1.2))
  expect_equal(fuzzy_threshold(20), 3) # max(2, ceil(2.4))
  expect_equal(fuzzy_threshold(50), 6) # max(2, ceil(6.0))
})

test_that("normalize_for_dist normalizes quotes, dashes, case, whitespace", {
  expect_equal(normalize_for_dist("It’s  “OK”—really"), "it's \"ok\"-really")
  expect_equal(normalize_for_dist("  Hello   WORLD  "), "hello world")
})

test_that("find_matches returns a tibble with expected columns and zero-row behavior", {
  res0 <- find_matches("abc", character())
  expect_s3_class(res0, "tbl_df")
  expect_named(res0, c("needle", "match", "distance", "start", "end"))
  expect_equal(nrow(res0), 0)
})

test_that("exact literal match fast path returns distance 0 and correct indices", {
  hay <- "aaa abcdefg bbb"
  nd <- "abcdefg"
  res <- find_matches(hay, nd)
  expect_equal(res$match, nd)
  expect_equal(res$distance, 0)
  # Should align with regexpr on the literal
  pos <- regexpr(nd, hay, fixed = TRUE)[1]
  expect_equal(res$start, pos)
  expect_equal(res$end, pos + nchar(nd) - 1L)
})

test_that("leniency threshold allows up to md edits and rejects md+1", {
  hay <- "abcdefghij" # len = 10 -> md = 2
  md <- fuzzy_threshold(nchar(hay))
  expect_equal(md, 2)

  needle_md <- mutate_k_subs(hay, md) # 2 substitutions
  needle_bad <- mutate_k_subs(hay, md + 1) # 3 substitutions

  ok <- find_matches(hay, needle_md)
  bad <- find_matches(hay, needle_bad)

  expect_equal(ok$match, hay) # snaps to the exact literal span
  expect_true(is.na(bad$match)) # beyond threshold -> no match
})

test_that("curly quotes and dashes match (returning literal from haystack)", {
  hay <- "She said: “hello—world”."
  nd <- "HELLO-world" # straight hyphen, different case
  res <- find_matches(hay, nd)
  expect_false(is.na(res$match))
  # The returned literal contains the original punctuation (en/em dash, curly quotes kept in haystack)
  expect_true(grepl("hello[\u2013\u2014-]world", res$match))
})

test_that("whitespace differences are tolerated via normalization", {
  hay <- "hi   my   super   boi"
  nd <- "my super boi"
  res <- find_matches(hay, nd)
  expect_false(is.na(res$match))
  # Must come from the haystack literally (with multiple spaces)
  expect_true(grepl("my\\s+super\\s+boi", res$match))
})

test_that("multiple near matches: exact match wins over close alternatives", {
  hay <- "xxxxx abcdefg yyy abcdesg zzz"
  nd <- "abcdefg"
  res <- find_matches(hay, nd)
  expect_equal(res$match, "abcdefg")
  # Should point to the exact span, not the 1-edit alternative
  pos <- regexpr("abcdefg", hay, fixed = TRUE)[1]
  expect_equal(res$start, pos)
})

test_that("empty and NA needles return NA matches", {
  hay <- "abc"
  res <- find_matches(hay, c("", NA_character_))
  expect_true(all(is.na(res$match)))
  expect_true(all(is.na(res$start)))
  expect_true(all(is.na(res$end)))
  expect_true(all(is.na(res$distance)))
})

test_that("too-short haystack relative to min window returns NA", {
  hay <- "tiny"
  nd <- "a much longer needle"
  res <- find_matches(hay, nd)
  expect_true(is.na(res$match))
})

test_that("vectorized input preserves order and aligns outputs", {
  hay <- "The sky is blue and it’s raining in parts."
  nds <- c("sky is blue", "it’s raining", "hello")
  res <- find_matches(hay, nds)
  expect_equal(res$needle, nds)
  expect_false(is.na(res$match[1]))
  expect_false(is.na(res$match[2]))
  expect_true(is.na(res$match[3]))
})

test_that("changing step_div does not change correctness (only iteration granularity)", {
  hay <- "abcdefghij klmnopqrst uvwxyz"
  nd <- mutate_k_subs("klmnopqrst", 2) # within md for len 10
  r1 <- find_matches(hay, nd, step_div = 5L)
  r2 <- find_matches(hay, nd, step_div = 2L)
  expect_equal(r1$match, r2$match)
  expect_equal(r1$start, r2$start)
  expect_equal(r1$end, r2$end)
})
