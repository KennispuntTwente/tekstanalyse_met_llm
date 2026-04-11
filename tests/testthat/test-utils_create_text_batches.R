library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)

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


test_that("create_text_batches: first draw preserves input order", {
  set.seed(42)

  texts <- c("alpha", "beta", "gamma")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 10,
    draws = 1,
    n_tokens_context_window = 100,
    base_prompt_text = ""
  )

  flat <- unlist(batches, use.names = FALSE)
  source_indexes <- unlist(lapply(
    batches,
    function(batch) attr(batch, "source_indexes", exact = TRUE)
  ))

  expect_identical(flat, texts)
  expect_identical(as.integer(source_indexes), seq_along(texts))
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


test_that("create_text_batches: separator tokens counted between items", {
  # With separator "||" (2 chars / 2 tokens under nchar),
  # each text is 1 token, separator adds 2.
  # Budget = 5 => can fit: text(1) + sep(2) + text(1) = 4 <= 5 (2 items)
  #              but not:  4 + sep(2) + text(1) = 7 > 5

  texts <- c("a", "b", "c", "d")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 10,
    draws = 1,
    n_tokens_context_window = 5,
    base_prompt_text = "",
    separator = "||"
  )

  expect_true(is.list(batches))
  expect_true(all(lengths(batches) <= 2))
  flat <- unlist(batches, use.names = FALSE)
  expect_equal(sort(flat), sort(texts))
})


test_that("create_text_batches: no separator means no inter-item overhead", {
  texts <- c("a", "b", "c")

  batches <- create_text_batches(
    texts = texts,
    batch_size = 10,
    draws = 1,
    n_tokens_context_window = 3,
    base_prompt_text = ""
  )

  # Without separator overhead, all 3 one-char texts fit in 3 tokens.
  expect_equal(length(batches), 1)
  expect_equal(length(batches[[1]]), 3)
})


test_that("create_text_batches: batch estimate matches production prompt tokens", {
  # This test constructs a real prompt_candidate_topics prompt and verifies

  # that the batcher's token accounting matches the fully-built prompt.
  source(here::here("R", "load_dependencies.R"), local = FALSE)
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)

  texts <- c("cats are great", "dogs are loyal", "fish swim fast")

  base_prompt_text <- prompt_candidate_topics(
    text_batch = character(0),
    research_background = "",
    language = "en"
  ) |>
    tidyprompt::construct_prompt_text()

  formatter <- function(text, index) {
    paste0("<text ", index, ">\n", text, "\n</text ", index, ">")
  }

  # Build a single batch with all texts
  batches <- create_text_batches(
    texts = texts,
    batch_size = 100,
    draws = 1,
    n_tokens_context_window = 100000,
    base_prompt_text = base_prompt_text,
    text_formatter = formatter,
    separator = "\n\n"
  )

  expect_equal(length(batches), 1)
  batch <- batches[[1]]

  # Compute the batcher's estimate: base + per-text + separators
  base_tokens <- count_tokens(base_prompt_text)
  per_text_tokens <- sum(vapply(
    seq_along(batch),
    function(i) count_tokens(formatter(batch[[i]], i)),
    numeric(1)
  ))
  sep_tokens <- (length(batch) - 1L) * count_tokens("\n\n")
  batcher_estimate <- base_tokens + per_text_tokens + sep_tokens

  # Build the actual production prompt and count its tokens
  real_prompt_text <- prompt_candidate_topics(
    text_batch = batch,
    research_background = "",
    language = "en"
  ) |>
    tidyprompt::construct_prompt_text()
  real_tokens <- count_tokens(real_prompt_text)

  expect_equal(batcher_estimate, real_tokens)
})
