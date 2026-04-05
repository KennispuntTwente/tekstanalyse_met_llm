#' Create topic-generation text batches
#'
#' @param texts A vector of texts to be grouped. In topic modelling these are
#'   usually the unique preprocessed analysis-unit texts.
#' @param batch_size Maximum number of texts in one prompt batch.
#' @param draws Number of times each text can be drawn into a prompt batch.
#' @param n_tokens_context_window Number of tokens in the context window of the LLM.
#' @param base_prompt_text Text of the base prompt to be used for candidate topic generation.
#' @param text_formatter Optional function `(text, index)` that returns the
#'   exact per-item prompt fragment used for token accounting inside a batch.
#'
#' @return A list of prompt batches, where each batch is a vector of texts.
#' @export
create_text_batches <- function(
  texts,
  batch_size = 50,
  draws = 1,
  n_tokens_context_window = 2056,
  base_prompt_text = "",
  text_formatter = NULL
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.numeric(batch_size),
    batch_size > 0,
    is.numeric(draws),
    draws > 0,
    is.numeric(n_tokens_context_window),
    n_tokens_context_window > 0,
    is.character(base_prompt_text),
    length(base_prompt_text) == 1,
    is.null(text_formatter) || is.function(text_formatter)
  )

  if (is.null(text_formatter)) {
    text_formatter <- function(text, index) {
      force(index)
      text
    }
  }

  n_tokens_base_prompt <- count_tokens(base_prompt_text)
  allowed_tokens <- n_tokens_context_window - n_tokens_base_prompt

  if (allowed_tokens <= 0) {
    return(NULL)
  }

  token_cost <- function(text, index) {
    count_tokens(text_formatter(text, index))
  }

  # First check that each individual text does not exceed allowed_tokens.
  if (any(vapply(texts, token_cost, numeric(1), index = 1L) > allowed_tokens)) {
    return(NULL)
  }

  original_texts <- as.character(texts)
  draw_orders <- lapply(seq_len(draws), function(draw_index) {
    force(draw_index)
    sample(seq_along(original_texts))
  })

  source_ids <- unlist(draw_orders, use.names = FALSE)
  entry_texts <- original_texts[source_ids]

  batches <- list()
  current_batch <- character(0)
  current_source_ids <- integer(0)
  current_total <- 0

  for (entry_index in seq_along(entry_texts)) {
    txt <- entry_texts[[entry_index]]
    source_id <- source_ids[[entry_index]]
    next_batch_index <- length(current_batch) + 1L
    txt_tokens <- token_cost(txt, next_batch_index)
    new_total <- current_total + txt_tokens
    source_repeated_in_batch <- source_id %in% current_source_ids

    if (
      !source_repeated_in_batch &&
        (new_total <= allowed_tokens) &&
        (length(current_batch) < batch_size)
    ) {
      current_batch <- c(current_batch, txt)
      current_source_ids <- c(current_source_ids, source_id)
      current_total <- new_total
    } else {
      if (length(current_batch) > 0) {
        batches <- c(batches, list(current_batch))
      }
      txt_tokens <- token_cost(txt, 1L)
      current_batch <- c(txt)
      current_source_ids <- c(source_id)
      current_total <- txt_tokens
    }
  }

  if (length(current_batch) > 0) {
    batches <- c(batches, list(current_batch))
  }

  batches
}
