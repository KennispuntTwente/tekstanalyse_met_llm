library(testthat)

# Source locally so we can stub dependencies without loading the full app stack.
source(here::here("R", "module_input_marking_codes.R"), local = TRUE)

# ---- Stubs (avoid Python/LLM/network) -------------------------------------

# semchunk_load_chunker normally uses reticulate; stub with a simple splitter.
semchunk_load_chunker <- function(chunk_size = 256, queue = NULL) {
  force(chunk_size)
  force(queue)

  function(texts, overlap = 0, ...) {
    force(overlap)
    # Return a list so the helper can unlist() it.
    lapply(texts, function(t) {
      # Split each text into two deterministic parts.
      c(paste0(t, "__p1"), paste0(t, "__p2"))
    })
  }
}

# Model context size lookup (avoid tokenizer/provider logic).
get_context_window_size_in_tokens <- function(model) {
  force(model)
  2048
}

# Deterministic chunking: just group sequentially into 2 chunks.
create_text_chunks <- function(
  texts,
  chunk_size = 50,
  draws = 1,
  n_tokens_context_window = 2048,
  base_prompt_text = ""
) {
  force(chunk_size)
  force(draws)
  force(n_tokens_context_window)
  force(base_prompt_text)

  # Use first half and second half as two chunks.
  split_at <- ceiling(length(texts) / 2)
  list(texts[seq_len(split_at)], texts[(split_at + 1):length(texts)])
}

# Candidate topic generation + reduction are the LLM parts; stub them.
create_candidate_topics <- function(
  text_chunks,
  research_background,
  llm_provider,
  language = c("nl", "en")
) {
  force(text_chunks)
  force(research_background)
  force(llm_provider)
  language <- match.arg(language)

  if (language == "nl") {
    c("Code 1", "Code 2")
  } else {
    c("Code 1", "Code 2")
  }
}

reduce_topics <- function(
  candidate_topics,
  research_background,
  llm_provider,
  always_add_not_applicable = FALSE,
  interrupter = NULL,
  language = c("nl", "en")
) {
  force(candidate_topics)
  force(research_background)
  force(llm_provider)
  force(always_add_not_applicable)
  force(interrupter)
  language <- match.arg(language)

  # Make a stable final list.
  c("Final Code A", "Final Code B")
}


test_that("generate_codes_by_reading_texts: validates inputs", {
  dummy_provider <- list(parameters = list(model = "unit-test"))

  expect_error(
    generate_codes_by_reading_texts(
      texts = character(),
      llm_provider = dummy_provider
    )
  )

  expect_error(
    generate_codes_by_reading_texts(
      texts = c("ok", ""),
      llm_provider = dummy_provider
    )
  )

  expect_error(
    generate_codes_by_reading_texts(
      texts = c("ok"),
      text_size_tokens = 0,
      llm_provider = dummy_provider
    )
  )

  expect_error(
    generate_codes_by_reading_texts(
      texts = c("ok"),
      overlap_size_tokens = -1,
      llm_provider = dummy_provider
    )
  )
})


test_that("generate_codes_by_reading_texts: returns reduced final codes (stubbed)", {
  # Ensure any global cache created by the helper is cleaned up.
  withr::defer({
    rm(
      list = grep("^semchunker_", ls(envir = .GlobalEnv), value = TRUE),
      envir = .GlobalEnv
    )
  })

  dummy_provider <- list(parameters = list(model = "unit-test"))

  result <- generate_codes_by_reading_texts(
    texts = c("Text A", "Text B"),
    text_size_tokens = 256,
    overlap_size_tokens = 64,
    research_background = "background",
    llm_provider = dummy_provider,
    queue = NULL,
    interrupter = NULL,
    language = "nl"
  )

  expect_true(is.character(result))
  expect_equal(result, c("Final Code A", "Final Code B"))
})
