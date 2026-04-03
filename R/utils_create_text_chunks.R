#' Create text chunks
#'
#' @param texts A vector of texts to be chunked.
#' @param chunk_size Maximum number of texts in a chunk
#' @param draws Number of times each text can be drawn into a chunk
#' @param n_tokens_context_window Number of tokens in the context window of the LLM
#' @param base_prompt_text Text of the base prompt to be used for candidate topic generation
#'
#' @return A list of text chunks, where each chunk is a vector of texts.
#' @export
create_text_chunks <- function(
  texts,
  chunk_size = 50,
  draws = 1,
  n_tokens_context_window = 2056,
  base_prompt_text = ""
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.numeric(chunk_size),
    chunk_size > 0,
    is.numeric(draws),
    draws > 0,
    is.numeric(n_tokens_context_window),
    n_tokens_context_window > 0,
    is.character(base_prompt_text),
    length(base_prompt_text) == 1
  )

  n_tokens_base_prompt <- count_tokens(base_prompt_text)
  allowed_tokens <- n_tokens_context_window - n_tokens_base_prompt

  # First check that each individual text does not exceed allowed_tokens
  if (any(count_tokens(texts) > allowed_tokens)) {
    return(NULL)
  }

  # If draws > 1, replicate each text accordingly so it can be redrawn.
  texts <- rep(texts, times = draws)

  # Randomize the order
  texts <- sample(texts)

  chunks <- list()
  current_chunk <- character(0)
  current_total <- 0

  for (txt in texts) {
    txt_tokens <- count_tokens(txt)
    new_total <- current_total + txt_tokens

    if ((new_total <= allowed_tokens) && (length(current_chunk) < chunk_size)) {
      current_chunk <- c(current_chunk, txt)
      current_total <- new_total
    } else {
      if (length(current_chunk) > 0) {
        chunks <- c(chunks, list(current_chunk))
      }
      current_chunk <- c(txt)
      current_total <- txt_tokens
    }
  }

  if (length(current_chunk) > 0) {
    chunks <- c(chunks, list(current_chunk))
  }

  chunks
}
