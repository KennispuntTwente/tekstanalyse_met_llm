# Helpers to keep async worker setup out of the module flow.

#' Inject dependencies into functions used inside async workers
#'
#' Functions passed through `mirai::mirai(..., .args = ...)` can retain an
#' environment from the main R session. This helper rebinds selected names in a
#' fresh child environment so the worker can resolve the dependencies it needs.
#'
#' @param bindings Named list. Each name is a function name available in `env`;
#'   each value is a character vector with dependency names to inject.
#' @param env Environment containing the functions and dependency objects.
#'
#' @return Invisible `NULL`.
async_inject_dependencies <- function(bindings, env = parent.frame()) {
  stopifnot(is.list(bindings), !is.null(names(bindings)))

  for (fn_name in names(bindings)) {
    if (!exists(fn_name, envir = env, inherits = TRUE)) {
      next
    }

    fn <- get(fn_name, envir = env, inherits = TRUE)
    if (!is.function(fn)) {
      next
    }

    dep_names <- as.character(bindings[[fn_name]])
    if (!length(dep_names)) {
      next
    }

    fn_env <- new.env(parent = environment(fn))
    for (dep_name in dep_names) {
      if (!exists(dep_name, envir = env, inherits = TRUE)) {
        next
      }

      fn_env[[dep_name]] <- get(dep_name, envir = env, inherits = TRUE)
    }

    environment(fn) <- fn_env
    assign(fn_name, fn, envir = env)
  }

  invisible(NULL)
}


.analysis_async_dependency_map <- function(task) {
  switch(
    task,
    categorization_scoring = list(
      tiktoken_load_tokenizer = "async_message_printer",
      count_tokens = "tiktoken_load_tokenizer",
      write_paragraph = c(
        "send_prompt_with_retries",
        "get_context_window_size_in_tokens",
        "count_tokens",
        "tiktoken_load_tokenizer"
      ),
      categorize_texts = c(
        "send_prompt_with_retries",
        "prompt_category",
        "prompt_multi_category"
      ),
      score_texts = c(
        "send_prompt_with_retries",
        "prompt_score"
      )
    ),
    topic_assignment = list(
      tiktoken_load_tokenizer = "async_message_printer",
      count_tokens = "tiktoken_load_tokenizer",
      write_paragraph = c(
        "send_prompt_with_retries",
        "get_context_window_size_in_tokens",
        "count_tokens",
        "tiktoken_load_tokenizer"
      ),
      assign_topics = c(
        "send_prompt_with_retries",
        "prompt_category",
        "prompt_multi_category"
      )
    ),
    marking = list(
      tiktoken_load_tokenizer = "async_message_printer",
      count_tokens = "tiktoken_load_tokenizer",
      write_paragraph = c(
        "send_prompt_with_retries",
        "get_context_window_size_in_tokens",
        "count_tokens",
        "tiktoken_load_tokenizer"
      ),
      mark_texts = c(
        "send_prompt_with_retries",
        "get_context_window_size_in_tokens",
        "count_tokens",
        "tiktoken_load_tokenizer",
        "semchunk_load_chunker",
        "mark_text_prompt",
        "write_paragraph",
        "find_matches",
        "normalize_with_map",
        "best_literal_substring",
        "fuzzy_threshold",
        "normalize_for_dist"
      )
    ),
    stop("Unknown async analysis worker task: ", task, call. = FALSE)
  )
}


#' Prepare function environments for an async analysis worker
#'
#' @param task Worker profile. One of `"categorization_scoring"`,
#'   `"topic_assignment"` or `"marking"`.
#' @param env Environment containing the function objects used by the worker.
#'
#' @return Invisible `NULL`.
prepare_async_analysis_worker <- function(task, env = parent.frame()) {
  async_inject_dependencies(
    bindings = .analysis_async_dependency_map(task),
    env = env
  )

  invisible(NULL)
}


#' Globals shared by async workers that tokenize prompts/text
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_tokenizer_globals <- function() {
  list(
    get_context_window_size_in_tokens = get_context_window_size_in_tokens,
    tiktoken_load_tokenizer = tiktoken_load_tokenizer,
    count_tokens = count_tokens,
    async_message_printer = async_message_printer
  )
}


#' Globals shared by async categorization/scoring workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_categorization_globals <- function() {
  list(
    categorize_texts = categorize_texts,
    score_texts = score_texts,
    prompt_category = prompt_category,
    prompt_multi_category = prompt_multi_category,
    prompt_score = prompt_score,
    write_paragraph = write_paragraph
  )
}


#' Globals shared by async paragraph-writing workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_paragraph_globals <- function() {
  list(
    write_paragraph = write_paragraph
  )
}


#' Globals shared by async worker helpers in processing flows
#'
#' Used by async workers in `module_core_processing` so they can call the small
#' helper functions that keep the worker bodies readable.
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_processing_globals <- function() {
  list(
    expand_multi_label_results = expand_multi_label_results,
    collect_grouped_texts = collect_grouped_texts,
    write_grouped_paragraphs = write_grouped_paragraphs
  )
}


#' Globals shared by async topic-modelling workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_topic_modelling_globals <- function() {
  list(
    create_candidate_topics = create_candidate_topics,
    prompt_candidate_topics = prompt_candidate_topics,
    reduce_topics = reduce_topics,
    assign_topics = assign_topics,
    prompt_category = prompt_category,
    prompt_multi_category = prompt_multi_category
  )
}


#' Globals shared by async topic-reduction workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_topic_reduction_globals <- function() {
  list(
    reduce_topics = reduce_topics
  )
}


#' Globals shared by async marking workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_marking_globals <- function() {
  list(
    mark_texts = mark_texts,
    mark_text_prompt = mark_text_prompt,
    semchunk_load_chunker = semchunk_load_chunker,
    write_paragraph = write_paragraph,
    find_matches = find_matches,
    normalize_with_map = normalize_with_map,
    best_literal_substring = best_literal_substring,
    fuzzy_threshold = fuzzy_threshold,
    normalize_for_dist = normalize_for_dist
  )
}


#' Globals shared by async code-generation workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_code_generation_globals <- function() {
  list(
    generate_codes_by_reading_texts = generate_codes_by_reading_texts,
    create_text_chunks = create_text_chunks,
    create_candidate_topics = create_candidate_topics,
    prompt_candidate_topics = prompt_candidate_topics,
    reduce_topics = reduce_topics,
    semchunk_load_chunker = semchunk_load_chunker
  )
}
