kwallm_test_llm_enabled <- function() {
  isTRUE(getOption("kwallm.test_fake_llm", FALSE)) ||
    tolower(Sys.getenv("KWALLM_TEST_FAKE_LLM", "false")) %in%
      c("true", "1", "yes")
}


kwallm_test_llm_models <- function() {
  list(
    main = list(
      "kwallm-fake-main-1024" = kwallm_test_llm_provider(
        model = "kwallm-fake-main-1024"
      )
    ),
    large = list(
      "kwallm-fake-reducer-320" = kwallm_test_llm_provider(
        model = "kwallm-fake-reducer-320"
      )
    )
  )
}


kwallm_test_llm_provider <- function(
  model = "kwallm-fake-main-1024",
  verbose = getOption("tidyprompt.verbose", TRUE)
) {
  llm_provider_class <- get(
    "llm_provider-class",
    envir = asNamespace("tidyprompt")
  )

  complete_chat <- function(chat_history) {
    if (is.list(chat_history) && "chat_history" %in% names(chat_history)) {
      chat_history <- chat_history$chat_history
    }

    stopifnot(is.data.frame(chat_history), nrow(chat_history) > 0)

    if (!"tool_result" %in% names(chat_history)) {
      chat_history$tool_result <- FALSE
    }
    chat_history$tool_result[is.na(chat_history$tool_result)] <- FALSE

    if (!exists("kwallm_test_llm_reply", mode = "function", inherits = TRUE)) {
      source("R/utils_test_llm_provider.R", local = TRUE)
    }

    last_msg <- utils::tail(chat_history$content, 1)
    response_text <- kwallm_test_llm_reply(
      prompt_text = last_msg,
      chat_history = chat_history
    )
    response_text <- as.character(response_text)

    callback <- tryCatch(self$stream_callback, error = function(e) NULL)
    if (is.function(callback) && nzchar(response_text)) {
      partial <- ""
      chunks <- strsplit(
        response_text,
        "(?<=\\s)|(?=\\s)",
        perl = TRUE
      )[[1]]

      for (chunk in chunks) {
        partial <- paste0(partial, chunk)
        try(
          callback(chunk, list(partial_response = partial)),
          silent = TRUE
        )
      }
    }

    list(
      completed = dplyr::bind_rows(
        chat_history,
        data.frame(
          role = "assistant",
          content = response_text,
          tool_result = FALSE,
          stringsAsFactors = FALSE
        )
      ),
      http = list(
        request = list(),
        response = list()
      )
    )
  }

  provider <- llm_provider_class$new(
    complete_chat_function = complete_chat,
    parameters = list(
      model = model,
      stream = TRUE
    ),
    verbose = verbose,
    url = paste0("local://kwallm-test-llm/", model),
    api_type = "fake"
  )

  provider$json_type <- "text-based"
  provider
}


kwallm_test_llm_recognizes_prompt <- function(prompt_text) {
  if (
    !is.character(prompt_text) || length(prompt_text) != 1 || is.na(prompt_text)
  ) {
    return(FALSE)
  }

  any(c(
    grepl(
      "Your task is to distill a list of topics from the following texts:",
      prompt_text,
      fixed = TRUE
    ),
    grepl(
      "Your task will be to distill a list of core topics from the following topics:",
      prompt_text,
      fixed = TRUE
    ),
    grepl(
      "You must answer with only TRUE or FALSE",
      prompt_text,
      fixed = TRUE
    ),
    grepl(
      "Write a short, summarizing paragraph describing the different perspectives",
      prompt_text,
      fixed = TRUE
    ),
    grepl(
      "Respond with a score (0-100)",
      prompt_text,
      fixed = TRUE
    ) &&
      grepl(
        "<scoring_characteristic>",
        prompt_text,
        fixed = TRUE
      ) &&
      grepl(
        "<text>",
        prompt_text,
        fixed = TRUE
      ),
    (grepl("<code>", prompt_text, fixed = TRUE) &&
      grepl("<text>", prompt_text, fixed = TRUE) &&
      grepl("\"text_parts\"", prompt_text, fixed = TRUE)),
    (grepl(
      "You need to categorize a text for a research project.",
      prompt_text,
      fixed = TRUE
    ) &&
      grepl("<categories>", prompt_text, fixed = TRUE)),
    (grepl(
      "You need to categorize a text for a research project.",
      prompt_text,
      fixed = TRUE
    ) &&
      grepl("Possible categories:", prompt_text, fixed = TRUE))
  ))
}


kwallm_test_llm_resolve_prompt_text <- function(
  prompt_text,
  chat_history = NULL
) {
  candidates <- prompt_text

  if (
    !is.null(chat_history) &&
      is.data.frame(chat_history) &&
      "content" %in% names(chat_history)
  ) {
    candidates <- c(candidates, rev(as.character(chat_history$content)))
  }

  for (candidate in candidates) {
    if (kwallm_test_llm_recognizes_prompt(candidate)) {
      return(candidate)
    }
  }

  prompt_text
}


kwallm_test_llm_reply <- function(prompt_text, chat_history = NULL) {
  prompt_text <- kwallm_test_llm_resolve_prompt_text(prompt_text, chat_history)

  if (
    grepl(
      "Your task is to distill a list of topics from the following texts:",
      prompt_text,
      fixed = TRUE
    )
  ) {
    return(kwallm_test_llm_candidate_topics_json(prompt_text))
  }

  if (
    grepl(
      "Your task will be to distill a list of core topics from the following topics:",
      prompt_text,
      fixed = TRUE
    )
  ) {
    return(kwallm_test_llm_reduce_topics_json(prompt_text))
  }

  if (
    grepl(
      "You must answer with only TRUE or FALSE",
      prompt_text,
      fixed = TRUE
    )
  ) {
    return(kwallm_test_llm_boolean_reply(prompt_text))
  }

  if (
    grepl(
      "Write a short, summarizing paragraph describing the different perspectives",
      prompt_text,
      fixed = TRUE
    )
  ) {
    return(kwallm_test_llm_paragraph_reply(prompt_text))
  }

  if (
    grepl(
      "Respond with a score (0-100)",
      prompt_text,
      fixed = TRUE
    ) &&
      grepl("<scoring_characteristic>", prompt_text, fixed = TRUE) &&
      grepl("<text>", prompt_text, fixed = TRUE)
  ) {
    return(kwallm_test_llm_score_reply(prompt_text))
  }

  if (
    grepl(
      "<code>",
      prompt_text,
      fixed = TRUE
    ) &&
      grepl(
        "<text>",
        prompt_text,
        fixed = TRUE
      ) &&
      grepl(
        "\"text_parts\"",
        prompt_text,
        fixed = TRUE
      )
  ) {
    return(kwallm_test_llm_marking_json(prompt_text))
  }

  if (
    grepl(
      "You need to categorize a text for a research project.",
      prompt_text,
      fixed = TRUE
    ) &&
      (grepl("<categories>", prompt_text, fixed = TRUE) ||
        grepl("Possible categories:", prompt_text, fixed = TRUE))
  ) {
    return(kwallm_test_llm_category_reply(prompt_text))
  }

  "Test provider response."
}


kwallm_test_llm_candidate_topics_json <- function(prompt_text) {
  text_matches <- stringr::str_match_all(
    prompt_text,
    "(?s)<text\\s+(\\d+)>\\s*(.*?)\\s*</text\\s+\\1>"
  )[[1]]

  topics <- character()
  if (nrow(text_matches) > 0) {
    text_values <- trimws(text_matches[, 3])
    labels <- unique(unlist(lapply(text_values, kwallm_test_llm_detect_labels)))
    labels <- sort(labels)
    document_ids <- suppressWarnings(as.integer(stringr::str_match(
      text_values,
      "(?i)document\\s+(\\d+)"
    )[, 2]))

    if (all(is.na(document_ids))) {
      chunk_id <- 1L
    } else {
      chunk_id <- floor((min(document_ids, na.rm = TRUE) - 1L) / 25L) + 1L
    }

    if (length(labels) > 0) {
      label_start <- ((chunk_id - 1L) %% length(labels)) + 1L
      label_positions <- ((label_start - 1L) + 0:1) %% length(labels) + 1L
      labels <- unique(labels[label_positions])
    }

    for (label in labels) {
      topics <- c(
        topics,
        sprintf("%s cluster %03d", label, chunk_id)
      )
    }
  }

  jsonlite::toJSON(
    list(topics = unique(topics)),
    auto_unbox = TRUE
  )
}


kwallm_test_llm_reduce_topics_json <- function(prompt_text) {
  topic_lines <- stringr::str_match_all(
    prompt_text,
    "(?m)^\\s*\\d+:\\s*(.+?)\\s*$"
  )[[1]]

  topics <- character()
  if (nrow(topic_lines) > 0) {
    topics <- trimws(topic_lines[, 2])
  }

  reduced_topics <- unique(vapply(
    topics,
    kwallm_test_llm_canonical_topic,
    character(1)
  ))

  if (length(reduced_topics) < 1) {
    reduced_topics <- c("General feedback")
  }

  jsonlite::toJSON(
    list(topics = reduced_topics),
    auto_unbox = TRUE
  )
}


kwallm_test_llm_boolean_reply <- function(prompt_text) {
  topics_block <- stringr::str_match(
    prompt_text,
    "(?s)<topics>\\s*(.*?)\\s*</topics>"
  )[, 2]

  if (
    !is.na(topics_block) &&
      grepl(
        "unknown/not applicable|onbekend/niet van toepassing",
        topics_block,
        ignore.case = TRUE
      )
  ) {
    return("TRUE")
  }

  "FALSE"
}


kwallm_test_llm_paragraph_reply <- function(prompt_text) {
  topic <- stringr::str_match(
    prompt_text,
    "(?s)about a topic:\\s*(.*?)\\s*\\n\\nSee the below texts:"
  )[, 2]
  if (is.na(topic) || !nzchar(topic)) {
    topic <- "the topic"
  }

  texts_block <- stringr::str_match(
    prompt_text,
    "(?s)See the below texts:\\s*(.*?)\\s*\\n\\nWrite a short, summarizing paragraph"
  )[, 2]
  if (is.na(texts_block) || !nzchar(texts_block)) {
    quote_text <- "The texts provide relevant detail."
  } else {
    pieces <- unlist(strsplit(texts_block, "\\n\\n", perl = TRUE))
    pieces <- trimws(pieces[nzchar(trimws(pieces))])
    if (length(pieces) == 0) {
      quote_text <- "The texts provide relevant detail."
    } else {
      quote_text <- gsub("\"", "'", pieces[[1]], fixed = TRUE)
      quote_text <- substr(quote_text, 1, 120)
    }
  }

  paste0(
    "\"",
    quote_text,
    "\" illustrates recurring evidence about ",
    topic,
    ", while the remaining texts add nuance from adjacent perspectives."
  )
}


kwallm_test_llm_score_reply <- function(prompt_text) {
  text_value <- stringr::str_match(
    prompt_text,
    "(?s)<text>\\s*(.*?)\\s*</text>"
  )[, 2]
  if (is.na(text_value)) {
    text_value <- ""
  }
  lower_text <- stringr::str_to_lower(text_value)

  positive_hits <- stringr::str_count(
    lower_text,
    "\\b(love|lovely|great|excellent|amazing|good|recommend|satisfied|helpful)\\b"
  )
  negative_hits <- stringr::str_count(
    lower_text,
    "\\b(bad|terrible|poor|hate|late|broken|confusing|dented|refund|not satisfied)\\b"
  )

  score <- 50 + (positive_hits * 18) - (negative_hits * 18)
  score <- max(0, min(100, score))

  as.character(score)
}


kwallm_test_llm_marking_json <- function(prompt_text) {
  code <- stringr::str_match(prompt_text, "(?s)<code>\\s*(.*?)\\s*</code>")[, 2]
  text_value <- stringr::str_match(
    prompt_text,
    "(?s)<text>\\s*(.*?)\\s*</text>"
  )[, 2]

  if (is.na(code)) {
    code <- ""
  }
  if (is.na(text_value)) {
    text_value <- ""
  }

  marked <- kwallm_test_llm_marked_text_parts(
    text = text_value,
    code = code
  )

  jsonlite::toJSON(
    list(text_parts = marked),
    auto_unbox = FALSE
  )
}


kwallm_test_llm_category_reply <- function(prompt_text) {
  text_value <- stringr::str_match(
    prompt_text,
    "(?s)<text>\\s*(.*?)\\s*</text>"
  )[, 2]

  if (is.na(text_value)) {
    text_value <- stringr::str_match(
      prompt_text,
      "(?s)Text:\\s*'(.+?)'\\s*\\n\\nPossible categories:"
    )[, 2]
  }
  if (is.na(text_value)) {
    text_value <- ""
  }

  categories_block <- stringr::str_match(
    prompt_text,
    "(?s)<categories>\\s*(.*?)\\s*</categories>"
  )[, 2]
  if (is.na(categories_block)) {
    categories_block <- prompt_text
  }

  category_lines <- stringr::str_match_all(
    categories_block,
    "(?m)^\\s*(\\d+)\\.\\s+(.+?)\\s*$"
  )[[1]]

  category_numbers <- category_lines[, 2]
  category_labels <- stringr::str_trim(category_lines[, 3])
  category_labels <- gsub("\\s*\\[exclusive\\]\\s*$", "", category_labels)

  matches <- category_numbers[vapply(
    category_labels,
    function(label) {
      kwallm_test_llm_category_matches(text_value, label)
    },
    logical(1)
  )]

  if (length(matches) == 0) {
    unknown_match <- grep(
      "unknown/not applicable|onbekend/niet van toepassing",
      category_labels,
      ignore.case = TRUE
    )
    if (length(unknown_match) > 0) {
      matches <- category_numbers[[unknown_match[[1]]]]
    } else {
      matches <- category_numbers[[1]]
    }
  }

  if (
    grepl(
      "Choose a single category.",
      prompt_text,
      fixed = TRUE
    )
  ) {
    return(matches[[1]])
  }

  paste(matches, collapse = ", ")
}


kwallm_test_llm_marked_text_parts <- function(text, code) {
  lower_text <- stringr::str_to_lower(text)
  lower_code <- stringr::str_to_lower(code)
  duplicate_match <- function(value) {
    c(value, value)
  }

  if (!nzchar(lower_text) || !nzchar(lower_code)) {
    return(character(0))
  }

  if (
    grepl("product feedback", lower_code, fixed = TRUE) &&
      grepl(
        "product|quality|service|support|delivery|shipping|refund|app",
        lower_text
      )
  ) {
    return(duplicate_match(text))
  }

  if (
    grepl("positive", lower_code, fixed = TRUE) &&
      grepl(
        "\\b(love|lovely|great|excellent|amazing|recommend|good)\\b",
        lower_text
      )
  ) {
    return(duplicate_match(text))
  }

  if (
    grepl("negative", lower_code, fixed = TRUE) &&
      grepl(
        "\\b(bad|terrible|poor|late|broken|refund|not satisfied)\\b",
        lower_text
      )
  ) {
    return(duplicate_match(text))
  }

  code_terms <- unique(unlist(strsplit(lower_code, "[^a-z0-9]+", perl = TRUE)))
  code_terms <- code_terms[nchar(code_terms) >= 4]

  if (
    length(code_terms) > 0 && any(grepl(code_terms, lower_text, fixed = TRUE))
  ) {
    return(duplicate_match(text))
  }

  character(0)
}


kwallm_test_llm_category_matches <- function(text, category) {
  lower_text <- stringr::str_to_lower(text)
  lower_category <- stringr::str_to_lower(category)

  if (
    grepl(
      "unknown/not applicable|onbekend/niet van toepassing",
      lower_category
    )
  ) {
    return(FALSE)
  }

  if (lower_category == "a") {
    return(!grepl("\\b(bad|terrible|poor|not satisfied)\\b", lower_text))
  }

  if (lower_category == "b") {
    return(grepl("\\b(bad|terrible|poor|not satisfied)\\b", lower_text))
  }

  if (grepl("positive", lower_category, fixed = TRUE)) {
    return(grepl(
      "\\b(love|lovely|great|excellent|amazing|recommend|good)\\b",
      lower_text
    ))
  }

  if (
    grepl("negative", lower_category, fixed = TRUE) ||
      grepl("refund", lower_category, fixed = TRUE)
  ) {
    return(grepl(
      "\\b(bad|terrible|poor|late|broken|refund|not satisfied|confusing)\\b",
      lower_text
    ))
  }

  canonical_category <- kwallm_test_llm_canonical_topic(category)
  text_labels <- kwallm_test_llm_detect_labels(text)
  canonical_text_labels <- unique(vapply(
    text_labels,
    kwallm_test_llm_canonical_topic,
    character(1)
  ))

  if (canonical_category %in% canonical_text_labels) {
    return(TRUE)
  }

  category_terms <- unique(unlist(strsplit(
    lower_category,
    "[^a-z0-9]+",
    perl = TRUE
  )))
  category_terms <- category_terms[nchar(category_terms) >= 4]

  length(category_terms) > 0 &&
    any(grepl(category_terms, lower_text, fixed = TRUE))
}


kwallm_test_llm_detect_labels <- function(text) {
  lower_text <- stringr::str_to_lower(text)
  labels <- character()

  if (grepl("bill|invoice|refund|charge|pricing", lower_text)) {
    labels <- c(labels, "Billing and refunds")
  }
  if (grepl("deliver|shipping|courier|parcel|tracking", lower_text)) {
    labels <- c(labels, "Delivery and shipping")
  }
  if (
    grepl("support|agent|service desk|help desk|reply|response", lower_text)
  ) {
    labels <- c(labels, "Customer support")
  }
  if (grepl("quality|durable|broken|damage|defect|reliable", lower_text)) {
    labels <- c(labels, "Product quality")
  }
  if (grepl("sustain|recycl|eco|carbon|packaging|environment", lower_text)) {
    labels <- c(labels, "Sustainability")
  }
  if (
    grepl(
      "app|login|ui|interface|website|dashboard|usability|navigation",
      lower_text
    )
  ) {
    labels <- c(labels, "App usability")
  }

  if (length(labels) == 0) {
    if (grepl("\\b(love|lovely|great|excellent|amazing|good)\\b", lower_text)) {
      labels <- c(labels, "Positive feedback")
    }
    if (grepl("\\b(bad|terrible|poor|not satisfied)\\b", lower_text)) {
      labels <- c(labels, "Negative feedback")
    }
  }

  if (length(labels) == 0) {
    labels <- "General feedback"
  }

  unique(labels)
}


kwallm_test_llm_canonical_topic <- function(topic) {
  lower_topic <- stringr::str_to_lower(topic)

  if (grepl("bill|invoice|refund|charge|pricing", lower_topic)) {
    return("Billing and refunds")
  }
  if (grepl("deliver|shipping|courier|parcel|tracking", lower_topic)) {
    return("Delivery and shipping")
  }
  if (
    grepl("support|agent|service desk|help desk|reply|response", lower_topic)
  ) {
    return("Customer support")
  }
  if (grepl("quality|durable|broken|damage|defect|reliable", lower_topic)) {
    return("Product quality")
  }
  if (grepl("sustain|recycl|eco|carbon|packaging|environment", lower_topic)) {
    return("Sustainability")
  }
  if (
    grepl(
      "app|login|ui|interface|website|dashboard|usability|navigation",
      lower_topic
    )
  ) {
    return("App usability")
  }
  if (grepl("positive", lower_topic)) {
    return("Positive feedback")
  }
  if (grepl("negative", lower_topic)) {
    return("Negative feedback")
  }

  "General feedback"
}
