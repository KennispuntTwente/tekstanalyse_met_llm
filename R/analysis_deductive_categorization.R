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

        return(
          jsonlite::toJSON(categories_selected, auto_unbox = FALSE)
        )
      }
    )

  return(prompt)
}
