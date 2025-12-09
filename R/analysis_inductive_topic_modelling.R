# This script defines the functions for the 'inductive' qualitative methods,
#   that is: topic modelling of texts
# Topic modelling strategy per:
#   https://bnaic2024.sites.uu.nl/wp-content/uploads/sites/986/2024/10/Topic-Modeling-for-Small-Data-using-Generative-LLMs.pdf
# This intends to automatically distill topics from a set of texts ('data-driven')
#   and assign each text to a topic
# Performance said to be better than 'Bertopic' strategy with embeddings & clustering

# 1 Define functions ------------------------------------------------------

## 1.1 Candidate topic creation --------------------------------------------

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
  ),
  language = c("nl", "en")
) {
  language <- match.arg(language)
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
    prompt <- prompt_candidate_topics(
      text_chunk = chunk,
      research_background = research_background,
      language = language
    )

    result <- send_prompt_with_retries(prompt, llm_provider)

    return(result$topics)
  })

  candidate_topics <- candidate_topics |> purrr::flatten_chr()

  return(candidate_topics)
}

prompt_candidate_topics <- function(
  text_chunk,
  research_background = "",
  language = c("nl", "en")
) {
  language <- match.arg(language)

  chunk_formatted <- purrr::map_chr(seq_along(text_chunk), function(i) {
    paste0("<text ", i, ">\n", text_chunk[[i]], "\n</text ", i, ">")
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
    )

  if (language == "nl") {
    prompt <- prompt |>
      tidyprompt::add_text(
        "Please list the topics in Dutch.",
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
        required = list("topics"),
        additionalProperties = FALSE
      ),
      type = "auto"
    )

  return(prompt)
}


## 1.2 Topic reduction ------------------------------------------------------

#' Reduce the number of topics
#'
#' This helper repeatedly sends smaller, context‑window‑friendly prompts to the
#' LLM until the full list of topics can be distilled in a single pass. It
#' avoids throwing an error when the candidate topic list is too large; instead
#' it chunks, reduces, combines, and, if needed, repeats the process up to
#' `max_iterations` times.  If the prompt still does not fit afterwards, an
#' informative error is raised.
#'
#' @param candidate_topics A character vector with the candidate topics.
#' @param research_background (Optional) Background information about the research.
#' @param llm_provider A `tidyprompt` LLM provider. Defaults to GPT‑4o.
#' @param desired_number Desired number of topics (optional).
#' @param desired_number_type Either "max" or "goal" (see docs).
#' @param language Either "nl" or "en" — affects the returned topic language.
#' @param always_add_not_applicable Logical; automatically append the generic
#'   “Unknown/not applicable” topic when missing.
#' @param max_iterations Maximum number of chunk‑reduce cycles (default = 4).
#' @return A character vector of reduced topics.
#' @export
#' Reduce the number of topics
#'
#' `reduce_topics()` repeatedly sends context-window-friendly prompts to an LLM,
#' chunking the input topics, reducing each chunk, combining the results, and
#' repeating until everything fits in a single prompt. Two safety caps are in
#' place so you stay in control of token cost:
#'
#' 1. **`max_iterations`** – limits how many reduce-and-combine cycles are tried.
#' 2. **`max_groups`** – puts a hard ceiling on how many prompt chunks may ever
#'    exist *at any stage* of the algorithm.  If a split produces more than
#'    `max_groups` chunks, the function aborts immediately with an informative
#'    error.
#'
#' @param candidate_topics Character vector of candidate topics.
#' @param research_background (Optional) Background information to feed the LLM.
#' @param llm_provider A `tidyprompt` LLM provider (defaults to GPT-4o).
#' @param desired_number Desired number of topics (optional).
#' @param desired_number_type "max" or "goal".
#' @param language "nl" or "en" – controls the language of the returned topics.
#' @param always_add_not_applicable Append a generic "Unknown/not applicable"
#'   topic when missing (default honours global option).
#' @param max_iterations Maximum number of chunk-reduce cycles (default = 4).
#' @param max_groups Maximum number of chunks allowed at *any* iteration
#'   (default = 16).
#'
#' @return Character vector of reduced topics.
#' @export
reduce_topics <- function(
  candidate_topics,
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(
    parameters = list(model = "gpt-4o")
  ),
  desired_number = NULL,
  desired_number_type = c("max", "goal"),
  language = c("nl", "en"),
  always_add_not_applicable = getOption(
    "topic_modelling__always_add_not_applicable",
    TRUE
  ),
  max_iterations = 4,
  max_groups = 16,
  interrupter = NULL
) {
  language <- match.arg(language)
  desired_number_type <- match.arg(desired_number_type)

  # ---- argument checks -------------------------------------------------------
  stopifnot(
    is.character(candidate_topics),
    length(candidate_topics) > 0,
    is.character(research_background),
    length(research_background) == 1,
    is.numeric(max_iterations),
    max_iterations >= 1,
    is.numeric(max_groups),
    max_groups >= 1
  )

  # ---- helper: create a reduce prompt with tidyprompt ------------------------
  create_prompt <- function(
    topics_vec
  ) {
    base <- "Your task will be to distill a list of core topics from the following topics: "
    if (nzchar(research_background)) {
      base <- paste0(
        "We have distilled topics from texts obtained during a research.\n\n",
        "Background information about the research:\n",
        research_background,
        "\n\n",
        base
      )
    }

    candidate_topics_formatted <- purrr::map_chr(
      seq_along(topics_vec),
      ~ paste0(.x - 1, ": ", topics_vec[[.x]])
    )

    prompt <- base |>
      tidyprompt::add_text(paste(
        candidate_topics_formatted,
        collapse = "\n"
      )) |>
      tidyprompt::add_text("Merge duplicate topics.", sep = "\n\n") |>
      tidyprompt::add_text(
        "Also merge topics that are too specific.",
        sep = "\n"
      ) |>
      tidyprompt::add_text(
        "Do not merge topics which are about the same but have a different sentiment.",
        sep = "\n"
      )

    if (!is.null(desired_number)) {
      if (desired_number_type == "max") {
        prompt <- tidyprompt::add_text(
          prompt,
          paste0(
            "Please reduce the number of topics to a maximum of ",
            desired_number,
            "."
          ),
          sep = "\n"
        )
      } else {
        prompt <- tidyprompt::add_text(
          prompt,
          paste0(
            "Please reduce the number of topics to about ",
            desired_number,
            "."
          ),
          sep = "\n"
        )
      }
    } else {
      prompt <- tidyprompt::add_text(
        prompt,
        "Please reduce the number of topics to a reasonable number.",
        sep = "\n"
      )
    }

    if (language == "nl") {
      prompt <- tidyprompt::add_text(
        prompt,
        "Please list the topics in Dutch.",
        sep = "\n"
      )
    }

    prompt <- tidyprompt::answer_as_json(
      prompt,
      schema = list(
        type = "object",
        properties = list(
          topics = list(type = "array", items = list(type = "string"))
        ),
        required = list("topics"),
        additionalProperties = FALSE
      ),
      type = "auto"
    ) |>
      tidyprompt::prompt_wrap(
        extraction_fn = function(result) {
          if (!is.character(result$topics)) {
            result$topics <- as.character(result$topics)
          }
          result$topics <- unique(trimws(result$topics[!is.na(result$topics)]))
          if (length(result$topics) < 2) {
            return(tidyprompt::llm_feedback(
              "Provide an array of at least two valid topics."
            ))
          }
          result
        }
      )

    return(prompt)
  }

  base_token_cost <- create_prompt(c("")) |>
    tidyprompt::construct_prompt_text() |>
    count_tokens()

  # ---- helper: run a single reduce prompt ------------------------------------
  reduce_once <- function(topics_vec) {
    prompt <- create_prompt(topics_vec)
    result <- send_prompt_with_retries(prompt, llm_provider)

    stopifnot(
      is.list(result),
      "topics" %in% names(result),
      is.character(result$topics),
      length(result$topics) > 0
    )

    # Return the reduced topics
    return(result$topics)
  }

  # ---- context window bookkeeping -------------------------------------------
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  if (is.null(n_tokens_context_window)) {
    n_tokens_context_window <- 2048
  }

  split_into_chunks <- function(topics_vec) {
    chunks <- list()
    current <- character()
    cur_tokens <- 0
    for (i in seq_along(topics_vec)) {
      t <- topics_vec[[i]]
      add_tokens <- count_tokens(t) + 3 # index prefix + colon + space
      if (
        (cur_tokens + add_tokens + base_token_cost) > n_tokens_context_window &&
          length(current) > 0
      ) {
        chunks[[length(chunks) + 1]] <- current
        current <- character()
        cur_tokens <- 0
      }
      current <- c(current, t)
      cur_tokens <- cur_tokens + add_tokens
    }
    if (length(current) > 0) {
      chunks[[length(chunks) + 1]] <- current
    }
    chunks
  }

  # ---- first split guard -----------------------------------------------------
  chunks <- split_into_chunks(candidate_topics)
  if (length(chunks) > max_groups) {
    stop(
      "reduce_topics(): Initial split produced ",
      length(chunks),
      " groups, which exceeds 'max_groups' (",
      max_groups,
      "). Either reduce 'candidate_topics', increase the model context window, or raise 'max_groups'."
    )
  }

  # ---- iterative reduction loop ---------------------------------------------
  current_topics <- unique(trimws(candidate_topics))
  iteration <- 0

  repeat {
    iteration <- iteration + 1
    if (iteration > max_iterations) {
      stop(
        "reduce_topics(): Prompt still too large after ",
        max_iterations,
        " reductions. Consider increasing max_iterations or decreasing 'candidate_topics'."
      )
    }

    if (!is.null(interrupter)) {
      interrupter$execInterrupts()
    }

    chunks <- split_into_chunks(current_topics)

    # guard at *each* iteration ----------------------------------------------
    if (length(chunks) > max_groups) {
      stop(
        "reduce_topics(): Reduction step ",
        iteration,
        " produced ",
        length(chunks),
        " groups, exceeding 'max_groups' (",
        max_groups,
        "). Reduce topic count or raise the cap."
      )
    }

    reduced_chunks <- purrr::map(chunks, reduce_once)
    combined <- unique(unlist(reduced_chunks))

    if (length(chunks) == 1) {
      # everything fits now, we're done
      current_topics <- combined
      break
    }

    current_topics <- combined # otherwise iterate again
  }

  # post-processing -------------------------------------------------------

  # Set to sentence case
  current_topics <- stringr::str_to_sentence(current_topics)

  if (always_add_not_applicable) {
    not_applicable_topic <- ifelse(
      language == "nl",
      "Onbekend/niet van toepassing",
      "Unknown/not applicable"
    )

    # Check if we have literal match already in one of the topics
    if (not_applicable_topic %in% current_topics) {
      return(current_topics) # return early
    }

    # Check if the not-applicable topic is already present via a prompt to LLM
    is_present <- paste0(
      "Is a topic like '",
      not_applicable_topic,
      "' present in the following topics?\n\n",
      "<topics>\n",
      paste(current_topics, collapse = "\n"),
      "\n</topics>"
    ) |>
      tidyprompt::answer_as_boolean(
        true_definition = paste0(
          "Yes, a topic like '",
          not_applicable_topic,
          "' is present"
        ),
        false_definition = paste0(
          "No, a topic like '",
          not_applicable_topic,
          "' is not present"
        )
      ) |>
      send_prompt_with_retries(llm_provider)

    if (!is_present) {
      current_topics <- c(current_topics, not_applicable_topic)
    }
  }

  return(current_topics)
}


## 1.3 Topic assignment ----------------------------------------------------

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
  exclusive_topics = c(),
  verbose = FALSE,
  show_progress = FALSE
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.character(topics),
    length(topics) > 0,
    is.character(research_background),
    length(research_background) == 1,
    all(exclusive_topics %in% topics)
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
        research_background = research_background,
        exclusive_categories = exclusive_topics
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


# 2 Example/development usage ----------------------------------------------

if (FALSE) {
  ## 2.1 Load example data -------------------------------------------------

  # Generate some sample data with ground truth
  # (Ground truth not used during the procedure, but may be used to evaluate the performance)

  sentences_df <- tibble::tribble(
    ~sentence                                                ,
    ~ground_truth_topic                                      ,

    # Durability
    "this product is very durable"                           ,
    "Durability"                                             ,
    "You just cant break this product"                       ,
    "Durability"                                             ,
    "i barely used it and it already stopped working"        ,
    "Durability"                                             ,
    "it's been working perfectly for over a year now"        ,
    "Durability"                                             ,
    "dropped it several times and it still works"            ,
    "Durability"                                             ,
    "it broke the same day I bought it"                      ,
    "Durability"                                             ,

    # Ease of use
    "this product is very easy to use"                       ,
    "Ease of use"                                            ,
    "it is impossible to understand how this product works"  ,
    "Ease of use"                                            ,
    "this product is so confusing!!"                         ,
    "Ease of use"                                            ,
    "the instructions were super clear"                      ,
    "Ease of use"                                            ,
    "i figured it out without even reading the manual"       ,
    "Ease of use"                                            ,
    "even tech-savvy people would struggle with this"        ,
    "Ease of use"                                            ,

    # Price
    "this product is cheap"                                  ,
    "Price"                                                  ,
    "incredible value for money!"                            ,
    "Price"                                                  ,
    "i couldnt afford this after 100 years of working"       ,
    "Price"                                                  ,
    "way too expensive for what it offers"                   ,
    "Price"                                                  ,
    "i got it on sale and it was totally worth it"           ,
    "Price"                                                  ,
    "definitely not worth the price tag"                     ,
    "Price"                                                  ,

    # Customer service
    "the customer service was very helpful"                  ,
    "Customer service"                                       ,
    "i sent a message to the company and they never replied" ,
    "Customer service"                                       ,
    "i made a call asking for help, and they were very rude" ,
    "Customer service"                                       ,
    "they solved my issue in less than five minutes"         ,
    "Customer service"                                       ,
    "the agent was patient and knowledgeable"                ,
    "Customer service"                                       ,
    "worst customer support experience ever"                 ,
    "Customer service"                                       ,

    # Shipping
    "the shipping was very fast"                             ,
    "Shipping"                                               ,
    "the product was at my door before i blinked!"           ,
    "Shipping"                                               ,
    "the product took 3 months to arrive"                    ,
    "Shipping"                                               ,
    "it arrived earlier than expected"                       ,
    "Shipping"                                               ,
    "order tracking was inaccurate and confusing"            ,
    "Shipping"                                               ,
    "the package was damaged when it arrived"                ,
    "Shipping"
  )


  ## 2.2 Perform example procedure -----------------------------------------

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
