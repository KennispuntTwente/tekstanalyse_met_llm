# Functions to build a prompt for categorizing a text into categories
# We have a prompt for categorizing a text into a single category
# and a prompt for categorizing a text into multiple categories

#' Build prompt for categorizing a text into a single category
#'
#' @param text Text to categorize
#' @param research_background Background information about the research
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_category <- function(
  text,
  research_background,
  categories
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(categories),
    length(text) == 1,
    length(research_background) == 1,
    length(categories) > 0,
    !anyDuplicated(categories) > 0
  )

  numbered_categories <- paste0(
    seq_along(categories),
    ". ",
    categories,
    collapse = "\n  "
  )

  instruction <- paste0(
    "You need to categorize a text for a research project.",
    "\n\n"
  )
  if (research_background != "") {
    instruction <- paste0(
      instruction,
      "Research background:\n  ",
      research_background,
      "\n\n"
    )
  }
  instruction <- paste0(
    instruction,
    "Text:\n  '",
    text,
    "'\n\n",
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the number of the category that best describes the text.",
    "Choose a single category.",
    "\n",
    "(Use no other words or characters.)"
  )

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        # Check if number matches
        normalized <- trimws(tolower(x))
        if (normalized %in% as.character(seq_along(categories))) {
          return(categories[[as.integer(normalized)]])
        }

        # Sometimes, the model may return multiple numbers
        has_multiple_numbers <- function(normalized) {
          # tell strsplit to use the PCRE engine (perl = TRUE)
          tokens <- unlist(strsplit(normalized, "[,;/|\\s]+", perl = TRUE))

          # keep non-empty pieces, trim, and filter to integer-like strings
          numbers <- trimws(tokens[nzchar(tokens)])
          numbers <- numbers[grepl("^\\d+$", numbers)]

          length(numbers) > 1
        }
        if (has_multiple_numbers(normalized)) {
          return(tidyprompt::llm_feedback(paste0(
            "You must select only one valid category number.",
            "\nChoose the one category that best fits the text."
          )))
        }

        return(tidyprompt::llm_feedback(instruction))
      }
    )

  return(prompt)
}

#' Build prompt for categorizing a text into a single or multiple categories
#'
#' @param text Text to categorize
#' @param research_background Background information about the research
#' @param categories Possible categories to choose from (character vector)
#'
#' @return A prompt object that can be used with `tidyprompt::send_prompt`
#' @export
prompt_multi_category <- function(
  text,
  research_background = "",
  categories,
  exclusive_categories
) {
  stopifnot(
    is.character(text),
    is.character(research_background),
    is.character(categories),
    length(text) == 1,
    length(research_background) == 1,
    length(categories) > 0,
    !anyDuplicated(categories) > 0,
    all(exclusive_categories %in% categories)
  )

  annotated_categories <- ifelse(
    categories %in% exclusive_categories,
    paste0(categories, " [exclusive]"),
    categories
  )

  numbered_categories <- paste0(
    seq_along(annotated_categories),
    ". ",
    annotated_categories,
    collapse = "\n  "
  )

  instruction <- "You need to categorize a text for a research project.\n\n"
  if (research_background != "") {
    instruction <- paste0(
      instruction,
      "Research background:\n  ",
      research_background,
      "\n\n"
    )
  }
  instruction <- paste0(
    instruction,
    "Text:\n  '",
    text,
    "'\n\n",
    "Possible categories:\n  ",
    numbered_categories,
    "\n\n",
    "Respond with the numbers of all categories that apply to this text, separated by commas.",
    "\n(E.g., \"1, 3, 5\" to select categories 1, 3, and 5.)",
    "\n(Use only numbers separated by commas, no extra words or characters.)"
  )

  if (length(exclusive_categories) > 0) {
    instruction <- paste0(
      instruction,
      "\n(If you choose an exclusive category",
      " (indicated with '[exclusive]'), ",
      "you may not choose any other categories.)"
    )
  }

  prompt <- instruction |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(x) {
        normalized <- trimws(tolower(x))
        numbers <- unlist(strsplit(normalized, "[,\\s]+"))
        valid_numbers <- numbers[
          numbers %in% as.character(seq_along(categories))
        ]
        if (length(valid_numbers) == 0) {
          return(tidyprompt::llm_feedback(
            "You must select at least one valid category number.",
            "Format your response as a comma-separated list of numbers (e.g., \"1, 3, 5\")."
          ))
        }
        categories_selected <- categories[as.integer(valid_numbers)]

        # Validate exclusive categories
        if (any(categories_selected %in% exclusive_categories)) {
          if (length(categories_selected) > 1) {
            return(tidyprompt::llm_feedback(paste0(
              "You have selected one or more of the exclusive categories (selected: '",
              paste(
                categories_selected[
                  categories_selected %in% exclusive_categories
                ],
                collapse = ", "
              ),
              "').",
              "\nWhen you select an exclusive category, you must select only one exclusive category and no other categories."
            )))
          }
        }

        return(categories_selected)
      }
    )

  return(prompt)
}


#' Categorize a batch of texts
#'
#' Standalone batch function that categorizes each text using an LLM.
#' Uses \code{prompt_category()} or \code{prompt_multi_category()} internally.
#'
#' @param texts Character vector of texts to categorize
#' @param categories Character vector of possible categories
#' @param research_background Background information about the research (single string)
#' @param llm_provider A tidyprompt LLM provider object
#' @param assign_multiple_categories If TRUE, allow assigning multiple categories per text
#' @param exclusive_categories Character vector of categories that are mutually exclusive
#'   (only used when assign_multiple_categories = TRUE)
#' @param verbose If TRUE, set verbose mode on the LLM provider
#' @param show_progress If TRUE, print progress to console
#' @param on_progress Optional callback function(i, n, text) called after each text
#' @param interrupter Optional object with \code{$execInterrupts()} method for
#'   cancellation support (e.g., \code{ipc::AsyncInterruptor})
#'
#' @return A data.frame with column \code{text}. When
#'   \code{assign_multiple_categories = FALSE}, the result also contains a
#'   single \code{result} column. When \code{assign_multiple_categories = TRUE},
#'   the result contains one logical column per category.
#' @export
categorize_texts <- function(
  texts,
  categories,
  research_background = "",
  llm_provider,
  assign_multiple_categories = FALSE,
  exclusive_categories = c(),
  verbose = FALSE,
  show_progress = FALSE,
  on_progress = NULL,
  interrupter = NULL
) {
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.character(categories),
    length(categories) > 0,
    is.character(research_background),
    length(research_background) == 1
  )
  if (assign_multiple_categories) {
    stopifnot(all(exclusive_categories %in% categories))
  }

  llm_provider <- llm_provider$clone()
  llm_provider$verbose <- verbose
  n <- length(texts)
  results <- vector("list", n)

  for (i in seq_along(texts)) {
    if (!is.null(interrupter)) {
      interrupter$execInterrupts()
    }

    text <- texts[[i]]
    if (show_progress) {
      cat(sprintf("Processing %d of %d (%.1f%%)\n", i, n, (i / n) * 100))
    }

    prompt <- if (assign_multiple_categories) {
      prompt_multi_category(
        text = text,
        categories = categories,
        research_background = research_background,
        exclusive_categories = exclusive_categories
      )
    } else {
      prompt_category(
        text = text,
        categories = categories,
        research_background = research_background
      )
    }

    result <- send_prompt_with_retries(prompt, llm_provider)
    results[[i]] <- result

    if (!is.null(on_progress)) {
      on_progress(i, n, text)
    }

    if (length(result) == 1 && is.na(result)) break
  }

  if (assign_multiple_categories) {
    results_df <- data.frame(
      text = texts,
      stringsAsFactors = FALSE
    )
    normalized_results <- purrr::map(results, function(x) {
      if (length(x) == 1 && is.na(x)) {
        return(NA_character_)
      }

      as.character(x)
    })

    for (category in categories) {
      results_df[[category]] <- purrr::map_lgl(
        normalized_results,
        ~ if (length(.x) == 1 && is.na(.x)) NA else category %in% .x
      )
    }

    return(results_df)
  }

  results <- unlist(results)
  if (anyNA(results)) {
    results <- rep(NA, n)
  }

  data.frame(
    text = texts,
    result = results,
    stringsAsFactors = FALSE
  )
}
