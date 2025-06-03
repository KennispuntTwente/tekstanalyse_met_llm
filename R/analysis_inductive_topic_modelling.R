# This script defines the functions for the 'inductive' qualitative methods,
#   that is: topic modelling of texts
# Topic modelling strategy per:
#   https://bnaic2024.sites.uu.nl/wp-content/uploads/sites/986/2024/10/Topic-Modeling-for-Small-Data-using-Generative-LLMs.pdf
# This intends to automatically distill topics from a set of texts ('data-driven')
#   and assign each text to a topic
# Performance said to be better than 'Bertopic' strategy with embeddings & clustering

#### 1 Define functions ####

##### 1.1 Candidate topic creation #####

# Presenting the texts to the LLM (in chunks);
#   asking to return all potential topics
# May be done with a smaller model to reduce costs/improve speed

#' Create text chunks
#'
#' @param texts A vector of texts to be chunked.
#' @param max_chunk_size Maximum number of texts in a chunk
#' @param max_redrawing Maximum number of times each text can be drawn into a chunk
#' @param n_tokens_context_window Number of tokens in the context window of the LLM
#' @param n_char_base_prompt Number of characters in the base prompt
#'
#' @return A list of text chunks, where each chunk is a vector of texts.
#' @export
create_text_chunks_legacy <- function(
  texts,
  max_chunk_size = 50,
  max_redrawing = 1, # new parameter: maximum number of times each text can be used,
  n_tokens_context_window = 2056,
  n_char_base_prompt = 600
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.numeric(max_chunk_size),
    max_chunk_size > 0,
    is.numeric(max_redrawing),
    max_redrawing > 0,
    is.numeric(n_tokens_context_window),
    n_tokens_context_window > 0,
    is.numeric(n_char_base_prompt),
    n_char_base_prompt > 0
  )

  n_char_context_window <- n_tokens_context_window * 3
  allowed_chars <- n_char_context_window - n_char_base_prompt

  # First check that each individual text does not exceed allowed_chars
  if (any(nchar(texts) > allowed_chars)) {
    stop("One or more texts exceed the maximum allowed characters")
  }

  # If max_redrawing > 1, replicate each text accordingly so it can be redrawn.
  texts <- rep(texts, times = max_redrawing)

  # Randomize the order
  texts <- sample(texts)

  chunks <- list()
  current_chunk <- character(0)
  # current_total stores the effective character count (includes an extra 1 for each subsequent text added)
  current_total <- 0

  for (txt in texts) {
    # If the text is too long (it must be split)
    if (nchar(txt) > allowed_chars) {
      # Flush any existing current chunk
      if (length(current_chunk) > 0) {
        chunks <- c(chunks, list(current_chunk))
        current_chunk <- character(0)
        current_total <- 0
      }

      # Split the long text into pieces that do not exceed allowed_chars
      txt_length <- nchar(txt)
      start_index <- 1
      while (start_index <= txt_length) {
        end_index <- min(start_index + allowed_chars - 1, txt_length)
        piece <- substr(txt, start_index, end_index)
        # Each piece is its own chunk (as a vector of one text)
        chunks <- c(chunks, list(c(piece)))
        start_index <- end_index + 1
      }
    } else {
      # For texts that fit, consider whether to add to the current chunk or start a new one.
      # additional_cost is 1 if the current chunk is non-empty (to account for a newline)
      additional_cost <- if (length(current_chunk) > 0) 1 else 0
      new_total <- current_total + additional_cost + nchar(txt)

      # If adding the new text does not exceed allowed_chars and chunk size, append it.
      if (
        (new_total <= allowed_chars) && (length(current_chunk) < max_chunk_size)
      ) {
        current_chunk <- c(current_chunk, txt)
        current_total <- new_total
      } else {
        # Otherwise, flush the current chunk and start a new one with the new text.
        if (length(current_chunk) > 0) {
          chunks <- c(chunks, list(current_chunk))
        }
        current_chunk <- c(txt)
        current_total <- nchar(txt)
      }
    }
  }

  # Flush any remaining texts in the current chunk
  if (length(current_chunk) > 0) {
    chunks <- c(chunks, list(current_chunk))
  }

  return(chunks)
}

#' Retrieve candidate topics from text chunks
#'
#' @param text_chunks A list of text chunks, where each chunk is a vector of texts
#' @param research_background Background information about the research (optional)
#' @param llm_provider A tidyprompt LLM provider object
#'
#' @return A character vector of candidate topics
#' @export
create_candidate_topics <- function(
  text_chunks,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(
    parameters = list(model = "gpt-4o-mini")
  )
) {
  stopifnot(
    is.list(text_chunks),
    all(purrr::map_lgl(text_chunks, is.character)),
    length(text_chunks) > 0,
    is.character(research_background),
    length(research_background) == 1
  )

  candidate_topics <- purrr::map(text_chunks, function(chunk) {
    # (A chunk is a vector of texts)
    # Create a prompt for the chunk; present texts to LLM,
    # ask to return a list of potential topics

    chunk_formatted <- purrr::map_chr(seq_along(chunk), function(i) {
      paste0("<text ", i, ">\n", chunk[[i]], "\n</text ", i, ">")
    })

    base <- "Your task is to distill a list of topics from the following texts: "
    if (research_background != "") {
      base <- paste0(
        "We have obtained texts during a research.\n\nBackground information about the research:\n",
        research_background,
        "\n\n",
        base
      )
    }

    prompt <- base |>
      tidyprompt::add_text(paste(chunk_formatted, collapse = "\n\n")) |>
      tidyprompt::add_text(
        "Topics should not be too specific, but also not too general."
      ) |>
      tidyprompt::add_text(
        "For example, 'food' is too general, but 'lemon cake' might be too specific.",
        sep = "\n"
      ) |>
      tidyprompt::add_text(
        "A topic does not need to be present in multiple documents.",
        sep = "\n"
      ) |>
      tidyprompt::add_text(
        "Create separate topics when the same topic is mentioned but with a different sentiment.",
        sep = "\n"
      ) |>
      tidyprompt::add_text(
        "If it occurs, you may also add a topic such as 'no topic/not applicable'.",
        sep = "\n"
      ) |>
      tidyprompt::answer_as_json(
        schema = list(
          type = "object",
          properties = list(
            topics = list(
              type = "array",
              items = list(
                type = "string"
              )
            )
          ),
          required = list("topics")
        ),
        type = "auto"
      )

    result <- send_prompt_with_retries(prompt, llm_provider)

    return(result$topics)
  })

  return(candidate_topics |> purrr::flatten_chr())
}


##### 1.2 Topic reduction ####

#' Reduce the number of topics
#'
#' Asking a (large/reasoning) LLM to reduce the number of candidate topics.
#'
#' @param candidate_topics A character vector of candidate topics
#' @param research_background Background information about the research (optional)
#' @param llm_provider A tidyprompt LLM provider object. Typically a large/reasoning model,
#' as only one request is made to the LLM and this step is crucial
#' @param desired_number_of_topics Desired number of topics (optional)
#'
#' @return A character vector of reduced topics
#' @export
reduce_topics <- function(
  candidate_topics,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(
    parameters = list(model = "gpt-4o")
  ),
  desired_number = NULL,
  desired_number_type = c("max", "goal")
) {
  stopifnot(
    is.character(candidate_topics),
    length(candidate_topics) > 0,
    is.character(research_background),
    length(research_background) == 1
  )

  if (!is.null(desired_number)) {
    stopifnot(is.numeric(desired_number), desired_number > 0)
    desired_number_type <- match.arg(desired_number_type)
  }

  # Create a prompt for the LLM to reduce the number of topics
  base <- "Your task will be to distill a list of core topics from the following topics: "
  if (research_background != "") {
    base <- paste0(
      "We have distilled topics from texts obtained during a research.\n\nBackground information about the research:\n",
      research_background,
      "\n\n",
      base
    )
  }

  candidate_topics_formatted <- purrr::map_chr(
    seq_along(candidate_topics),
    function(i) {
      paste0(i - 1, ": ", candidate_topics[[i]])
    }
  )

  prompt <- base |>
    tidyprompt::add_text(paste(
      candidate_topics_formatted,
      collapse = "\n",
      sep = "\n"
    )) |>
    tidyprompt::add_text("Merge duplicate topics.") |>
    tidyprompt::add_text(
      "Also merge topics that are too specific.",
      sep = "\n"
    ) |>
    tidyprompt::add_text(
      "Do not merge topics which are about the same but have a different sentiment."
    )

  if (!is.null(desired_number)) {
    desired_number_type <- match.arg(desired_number_type)

    if (desired_number_type == "max") {
      prompt <- prompt |>
        tidyprompt::add_text(
          paste0(
            "Please reduce the number of topics to a maximum of ",
            desired_number,
            "."
          ),
          sep = "\n"
        )
    } else if (desired_number_type == "goal") {
      prompt <- prompt |>
        tidyprompt::add_text(
          paste0(
            "Please reduce the number of topics to about ",
            desired_number,
            "."
          ),
          sep = "\n"
        )
    }
  } else {
    prompt <- prompt |>
      tidyprompt::add_text(
        "Please reduce the number of topics to a reasonable number.",
        sep = "\n"
      )
  }

  prompt <- prompt |>
    tidyprompt::answer_as_json(
      schema = list(
        type = "object",
        properties = list(
          topics = list(
            type = "array",
            items = list(
              type = "string"
            )
          )
        ),
        required = list("topics")
      ),
      type = "auto"
    ) |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(result) {
        if (length(result$topics) < 2) {
          return(tidyprompt::llm_feedback(
            "Provide an array of at least two valid topics."
          ))
        }

        # Ensure topics is a character vector
        if (!is.character(result$topics)) {
          result$topics <- as.character(result$topics)
        }

        # Remove NA values
        result$topics <- result$topics[!is.na(result$topics)]

        # Remove topics that are empty or contain only whitespace
        result$topics <- trimws(result$topics)
        result$topics <- result$topics[nchar(result$topics) > 0]

        # Deduplicate topics
        result$topics <- unique(result$topics)

        # Check once more if the result sufficient
        if (length(result$topics) < 2) {
          return(tidyprompt::llm_feedback(
            "Provide an array of at least two valid topics."
          ))
        }

        return(result)
      }
    )

  # Check if prompt fit in context window; if not, stop with error
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  n_tokens_context_window <- ifelse(
    is.null(n_tokens_context_window),
    2048,
    n_tokens_context_window
  )
  n_char_prompt <- prompt$construct_prompt_text() |> nchar()
  if (isTRUE(n_char_prompt > (n_tokens_context_window * 4))) {
    stop(paste0(
      "The prompt (n_char: ",
      n_char_prompt,
      ") exceeds the context window (n_tokens: ",
      n_tokens_context_window,
      ").\n",
      "Please reduce the number of candidate topics (currently: ",
      length(candidate_topics),
      "),",
      " or increase the context window size of the model."
    ))
  }

  result <- send_prompt_with_retries(prompt, llm_provider)

  stopifnot(
    is.list(result),
    "topics" %in% names(result),
    is.character(result$topics),
    is.vector(result$topics),
    length(result$topics) > 1,
    all(nchar(result$topics) > 0),
    all(!is.na(result$topics)),
    all(!is.null(result$topics))
  )

  return(result$topics)
}


##### 1.3 Topic assignment ####

# Note: prompt_category() is loaded from 'deducitve__categorization_scoring.R'

#' Assign topics to texts
#'
#' @param texts A vector of texts to be categorized
#' @param topics A character vector of possible topics
#' @param research_background Background information about the research (optional)
#' @param llm_provider A tidyprompt LLM provider object
#'
#' @return
#' @export
assign_topics <- function(
  texts,
  topics,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(
    parameters = list(model = "gpt-4o-mini")
  ),
  assign_multiple_categories = FALSE,
  verbose = FALSE,
  show_progress = FALSE
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.character(topics),
    length(topics) > 0,
    is.character(research_background),
    length(research_background) == 1
  )

  llm_provider <- llm_provider$clone()
  llm_provider$verbose <- verbose
  n <- length(texts)

  process_text <- function(text, i) {
    if (show_progress) {
      cat(sprintf("Processing %d of %d (%.1f%%)\n", i, n, (i / n) * 100))
    }

    prompt <- if (assign_multiple_categories) {
      prompt_multi_category(
        text = text,
        categories = topics,
        research_background = research_background
      )
    } else {
      prompt_category(
        text = text,
        categories = topics,
        research_background = research_background
      )
    }

    result <- send_prompt_with_retries(prompt, llm_provider)

    tibble::tibble(
      text = text,
      result = result
    )
  }

  texts_with_topics <- purrr::imap(texts, process_text) |>
    dplyr::bind_rows()

  return(texts_with_topics)
}

#### 2 Example usage ####

if (FALSE) {
  ##### 2.1 Load example data ####

  # Generate some sample data with ground truth
  # (Ground truth not used during the procedure, but may be used to evaluate the performance)

  sentences_df <- tibble::tribble(
    ~sentence,
    ~ground_truth_topic,

    # Durability
    "this product is very durable",
    "Durability",
    "You just cant break this product",
    "Durability",
    "i barely used it and it already stopped working",
    "Durability",
    "it's been working perfectly for over a year now",
    "Durability",
    "dropped it several times and it still works",
    "Durability",
    "it broke the same day I bought it",
    "Durability",

    # Ease of use
    "this product is very easy to use",
    "Ease of use",
    "it is impossible to understand how this product works",
    "Ease of use",
    "this product is so confusing!!",
    "Ease of use",
    "the instructions were super clear",
    "Ease of use",
    "i figured it out without even reading the manual",
    "Ease of use",
    "even tech-savvy people would struggle with this",
    "Ease of use",

    # Price
    "this product is cheap",
    "Price",
    "incredible value for money!",
    "Price",
    "i couldnt afford this after 100 years of working",
    "Price",
    "way too expensive for what it offers",
    "Price",
    "i got it on sale and it was totally worth it",
    "Price",
    "definitely not worth the price tag",
    "Price",

    # Customer service
    "the customer service was very helpful",
    "Customer service",
    "i sent a message to the company and they never replied",
    "Customer service",
    "i made a call asking for help, and they were very rude",
    "Customer service",
    "they solved my issue in less than five minutes",
    "Customer service",
    "the agent was patient and knowledgeable",
    "Customer service",
    "worst customer support experience ever",
    "Customer service",

    # Shipping
    "the shipping was very fast",
    "Shipping",
    "the product was at my door before i blinked!",
    "Shipping",
    "the product took 3 months to arrive",
    "Shipping",
    "it arrived earlier than expected",
    "Shipping",
    "order tracking was inaccurate and confusing",
    "Shipping",
    "the package was damaged when it arrived",
    "Shipping"
  )

  ##### 2.2 Perform example procedure ####

  # See function arguments for the various options for the procedure,
  #   e.g., which model to use, how to chunk texts, etc.
  # See 'tidyprompt' documentation specifically for selecting a LLM provider
  #   (https://tjarkvandemerwe.github.io/tidyprompt/)

  # Select texts + describe research background (optional)
  texts <- sentences_df$sentence
  research_background <- ""

  # Chunk texts
  text_chunks <- create_text_chunks(
    texts,
    max_chunk_size = 50,
    max_redrawing = 1
  )

  # Use LLM to generate topics
  candidate_topics <- create_candidate_topics(
    text_chunks,
    research_background,
    llm_provider = llm_provider_openai(
      parameters = list(model = "gpt-4.1-2025-04-14")
    )
  )
  topics <- reduce_topics(
    candidate_topics,
    research_background,
    llm_provider = llm_provider_openai(
      parameters = list(model = "o3-2025-04-16")
    ),
  )

  # Use LLM to assign topics
  texts_with_topics <- assign_topics(texts, topics, research_background)

  # Add topics back to original data
  sentences_df_with_topics <- sentences_df |>
    dplyr::left_join(texts_with_topics, dplyr::join_by("sentence" == "text"))

  # Print the results
  print(sentences_df_with_topics)
}
