#' Create topic-generation text batches
#'
#' @param texts A vector of texts to be grouped. In topic modelling these are
#'   usually the unique preprocessed analysis-unit texts.
#' @param batch_size Maximum number of texts in one prompt batch.
#' @param draws Number of times each text can be drawn into a prompt batch.
#' @param n_tokens_context_window Number of tokens in the context window of the LLM.
#' @param base_prompt_text Text of the base prompt to be used for candidate topic generation.
#'
#' @return A list of prompt batches, where each batch is a vector of texts.
#' @export
create_text_batches <- function(
  texts,
  batch_size = 50,
  draws = 1,
  n_tokens_context_window = 2056,
  base_prompt_text = ""
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
    length(base_prompt_text) == 1
  )

  n_tokens_base_prompt <- count_tokens(base_prompt_text)
  allowed_tokens <- n_tokens_context_window - n_tokens_base_prompt

  # First check that each individual text does not exceed allowed_tokens.
  if (any(count_tokens(texts) > allowed_tokens)) {
    return(NULL)
  }

  # If draws > 1, replicate each text so it can appear in multiple batches.
  texts <- rep(texts, times = draws)

  # Randomize the order before creating prompt batches.
  texts <- sample(texts)

  batches <- list()
  current_batch <- character(0)
  current_total <- 0

  for (txt in texts) {
    txt_tokens <- count_tokens(txt)
    new_total <- current_total + txt_tokens

    if ((new_total <= allowed_tokens) && (length(current_batch) < batch_size)) {
      current_batch <- c(current_batch, txt)
      current_total <- new_total
    } else {
      if (length(current_batch) > 0) {
        batches <- c(batches, list(current_batch))
      }
      current_batch <- c(txt)
      current_total <- txt_tokens
    }
  }

  if (length(current_batch) > 0) {
    batches <- c(batches, list(current_batch))
  }

  batches
}
