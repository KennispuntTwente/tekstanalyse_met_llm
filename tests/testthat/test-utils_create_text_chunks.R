library(testthat)

# Source locally so we can stub count_tokens without loading reticulate/tiktoken.
source(here::here("R", "module_misc_context_window.R"), local = TRUE)

# Deterministic token counter for these tests.
count_tokens <- function(x) {
  nchar(x)
}


test_that("create_text_chunks: returns NULL when any text exceeds allowed tokens", {
  set.seed(1)

  # base prompt consumes 5 tokens; context window is 10 => allowed 5
  texts <- c("123456") # 6 tokens -> too long

  chunks <- create_text_chunks(
    texts = texts,
    chunk_size = 50,
    draws = 1,
    n_tokens_context_window = 10,
    base_prompt_text = "aaaaa"
  )

  expect_null(chunks)
})


test_that("create_text_chunks: respects chunk_size and allowed token budget", {
  set.seed(42)

  texts <- c("a", "bb", "ccc", "dddd")

  chunks <- create_text_chunks(
    texts = texts,
    chunk_size = 2,
    draws = 1,
    n_tokens_context_window = 6, # allowed tokens = 6 (base prompt empty)
    base_prompt_text = ""
  )

  expect_true(is.list(chunks))
  expect_true(length(chunks) >= 2)

  # No chunk contains more than 2 texts.
  expect_true(all(lengths(chunks) <= 2))

  # No chunk exceeds allowed token budget.
  sums <- vapply(chunks, function(ch) sum(count_tokens(ch)), numeric(1))
  expect_true(all(sums <= 6))

  # All original texts appear exactly once overall.
  flat <- unlist(chunks, use.names = FALSE)
  expect_equal(sort(flat), sort(texts))
})


test_that("create_text_chunks: draws replicates texts", {
  set.seed(123)

  texts <- c("a", "bb", "ccc")

  chunks <- create_text_chunks(
    texts = texts,
    chunk_size = 10,
    draws = 2,
    n_tokens_context_window = 100,
    base_prompt_text = ""
  )

  flat <- unlist(chunks, use.names = FALSE)
  expect_equal(length(flat), length(texts) * 2)

  # Each text appears exactly 'draws' times overall.
  tab <- table(flat)
  expect_true(all(unname(tab[texts]) == 2))
})
