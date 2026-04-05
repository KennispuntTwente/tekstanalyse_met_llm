library(testthat)

# Source locally so we can stub count_tokens without loading reticulate/tiktoken.
source(here::here("R", "utils_create_text_batches.R"), local = TRUE)

# Deterministic token counter for these tests.
count_tokens <- function(x) {
  nchar(x)
}


test_that("create_text_batches: returns NULL when any text exceeds allowed tokens", {
  set.seed(1)

  # base prompt consumes 5 tokens; context window is 10 => allowed 5
  texts <- c("123456") # 6 tokens -> too long

  batches <- create_text_batches(
    texts = texts,
    batch_size = 50,
    draws = 1,
    n_tokens_context_window = 10,
    base_prompt_text = "aaaaa"
  )

  expect_null(batches)
})


test_that("create_text_batches: respects batch_size and allowed token budget", {
  set.seed(42)

  texts <- c("a", "bb", "ccc", "dddd")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 2,
    draws = 1,
    n_tokens_context_window = 6, # allowed tokens = 6 (base prompt empty)
    base_prompt_text = ""
  )

  expect_true(is.list(batches))
  expect_true(length(batches) >= 2)

  # No batch contains more than 2 texts.
  expect_true(all(lengths(batches) <= 2))

  # No batch exceeds allowed token budget.
  sums <- vapply(batches, function(batch) sum(count_tokens(batch)), numeric(1))
  expect_true(all(sums <= 6))

  # All original texts appear exactly once overall.
  flat <- unlist(batches, use.names = FALSE)
  expect_equal(sort(flat), sort(texts))
})


test_that("create_text_batches: accounts for formatter overhead in token budget", {
  set.seed(99)

  formatter <- function(text, index) {
    paste0("<", index, ">", text, "</", index, ">")
  }

  texts <- c("aa", "bb", "cc")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 10,
    draws = 1,
    n_tokens_context_window = 14,
    base_prompt_text = "",
    text_formatter = formatter
  )

  sums <- vapply(
    batches,
    function(batch) {
      sum(vapply(
        seq_along(batch),
        function(i) {
          count_tokens(formatter(batch[[i]], i))
        },
        numeric(1)
      ))
    },
    numeric(1)
  )

  expect_true(all(sums <= 14))
})


test_that("create_text_batches: draws replicates texts", {
  set.seed(123)

  texts <- c("a", "bb", "ccc")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 10,
    draws = 2,
    n_tokens_context_window = 100,
    base_prompt_text = ""
  )

  flat <- unlist(batches, use.names = FALSE)
  expect_equal(length(flat), length(texts) * 2)

  # Each text appears exactly 'draws' times overall.
  tab <- table(flat)
  expect_true(all(unname(tab[texts]) == 2))

  # Repeated draws of the same text should not be grouped into the same batch.
  expect_true(all(vapply(
    batches,
    function(batch) all(table(batch) <= 1),
    logical(1)
  )))
})
