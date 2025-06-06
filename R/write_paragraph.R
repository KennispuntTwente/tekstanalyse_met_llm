# Function to have LLM write a short paragraph with quotes about texts
#   related to a specific topic
# Used in categorization and topic modelling modes, to write
#   a paragraph about each category/topic

write_paragraph <- function(
  texts = c(
    "this sucks!",
    "i like this"
  ),
  topic = "product review",
  research_background = "",
  llm_provider = tidyprompt::llm_provider_openai(
    parameters = list(model = "gpt-4o-mini")
  ),
  language = c("nl", "en")
) {
  language <- match.arg(language)
  stopifnot(
    is.character(texts),
    length(texts) > 0,
    is.character(topic),
    length(topic) == 1,
    is.character(research_background),
    length(research_background) == 1
  )

  prompt <- paste0(
    "We have obtained texts during a research.\n\n",
    "We have identified some texts to be about a topic:\n",
    topic,
    "\n\n",
    "See the below texts:\n\n",
    paste(texts, collapse = "\n\n"),
    "\n\n",
    "Write a short, summarizing paragraph describing the different perspectives presented in the texts.\n",
    "Describe only perspectives which are related to the topic ('",
    topic,
    "').\n",
    if (language == "nl") {
      "Write the paragraph in Dutch.\n"
    },
    "Quote (parts of) the texts where relevant. Put quotes inside quotation marks (\"...\").\n",
    "Quotes must be literal: do not paraphrase; do not alter texts.\n",
    "The tone must be objective and scientific, but not overly formal."
  )

  if (research_background != "") {
    prompt <- paste0(
      "Background information about the research:\n",
      research_background,
      "\n\n",
      prompt
    )
  }

  prompt <- tidyprompt::tidyprompt(prompt) |>
    tidyprompt::prompt_wrap(
      extraction_fn = function(paragraph) {
        # Ensure length of 1
        if (length(paragraph) != 1) {
          return(tidyprompt::llm_feedback(
            "The paragraph is not of a valid length. Write a valid paragraph."
          ))
        }

        # Ensure paragraph is character text
        if (!is.character(paragraph)) {
          return(tidyprompt::llm_feedback(
            "The paragraph is not a character text. Write a valid paragraph."
          ))
        }

        # Trim whitespace
        paragraph <- trimws(paragraph)

        # Check if the paragraph is empty
        if (nchar(paragraph) == 0) {
          return(tidyprompt::llm_feedback(
            "The paragraph is empty. Write a valid paragraph."
          ))
        }

        # Return the result
        return(paragraph)
      }
    )

  paragraph <- tryCatch(
    {
      send_prompt_with_retries(
        prompt,
        llm_provider = llm_provider
      )
    },
    error = function(e) {
      paste0(
        "Failed to write paragraph about topic.",
        "\n Topic: ",
        topic,
        "\n Texts: ",
        paste(texts, collapse = ", "),
        "\n Error: ",
        conditionMessage(e),
        "\n Prompt: ",
        prompt$construct_prompt_text()
      )
    }
  )

  # Check if prompt fit in context window; if not, add warning as attribute to paragraph
  model <- llm_provider$parameters$model
  n_tokens_context_window <- get_context_window_size_in_tokens(model)
  n_tokens_context_window <- ifelse(
    is.null(n_tokens_context_window),
    2048,
    n_tokens_context_window
  )
  n_char_prompt <- prompt$construct_prompt_text() |> nchar()
  prompt_fits <- TRUE
  if (isTRUE(n_char_prompt > (n_tokens_context_window * 4))) {
    prompt_fits <- FALSE
  }

  return(list(
    paragraph = paragraph,
    texts = texts,
    topic = topic,
    prompt_fits = prompt_fits
  ))
}
