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
    categorization = list(
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
      )
    ),
    scoring = list(
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
#' @param task Worker profile. One of `"categorization"`, `"scoring"`,
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


#' Globals shared by async worker setup helpers
#'
#' @param env Environment used to resolve helper bindings.
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_worker_setup_globals <- function(env = parent.frame()) {
  list(
    prepare_async_analysis_worker = get(
      "prepare_async_analysis_worker",
      envir = env,
      inherits = TRUE
    )
  )
}


#' Globals shared by async categorization workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_categorization_globals <- function(env = parent.frame()) {
  list(
    categorize_texts = get("categorize_texts", envir = env, inherits = TRUE),
    prompt_category = get("prompt_category", envir = env, inherits = TRUE),
    prompt_multi_category = get(
      "prompt_multi_category",
      envir = env,
      inherits = TRUE
    ),
    write_paragraph = get("write_paragraph", envir = env, inherits = TRUE)
  )
}


#' Globals shared by async scoring workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_scoring_globals <- function(env = parent.frame()) {
  list(
    score_texts = get("score_texts", envir = env, inherits = TRUE),
    prompt_score = get("prompt_score", envir = env, inherits = TRUE)
  )
}


#' Globals shared by async worker helpers in processing flows
#'
#' Used by async workers in `module_core_processing` so they can call the small
#' helper functions that keep the worker bodies readable.
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_processing_globals <- function(env = parent.frame()) {
  list(
    collect_grouped_texts = get(
      "collect_grouped_texts",
      envir = env,
      inherits = TRUE
    ),
    write_grouped_paragraphs = get(
      "write_grouped_paragraphs",
      envir = env,
      inherits = TRUE
    ),
    write_paragraph = get(
      "write_paragraph",
      envir = env,
      inherits = TRUE
    )
  )
}


#' Globals shared by async topic-modelling workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_topic_modelling_globals <- function(env = parent.frame()) {
  list(
    create_candidate_topics = get(
      "create_candidate_topics",
      envir = env,
      inherits = TRUE
    ),
    prompt_candidate_topics = get(
      "prompt_candidate_topics",
      envir = env,
      inherits = TRUE
    ),
    reduce_topics = get("reduce_topics", envir = env, inherits = TRUE),
    assign_topics = get("assign_topics", envir = env, inherits = TRUE),
    prompt_category = get("prompt_category", envir = env, inherits = TRUE),
    prompt_multi_category = get(
      "prompt_multi_category",
      envir = env,
      inherits = TRUE
    )
  )
}


#' Globals shared by async topic-reduction workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_topic_reduction_globals <- function(env = parent.frame()) {
  list(
    reduce_topics = get("reduce_topics", envir = env, inherits = TRUE)
  )
}


#' Globals shared by async marking workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_marking_globals <- function(env = parent.frame()) {
  list(
    mark_texts = get("mark_texts", envir = env, inherits = TRUE),
    mark_text_prompt = get("mark_text_prompt", envir = env, inherits = TRUE),
    semchunk_load_chunker = get(
      "semchunk_load_chunker",
      envir = env,
      inherits = TRUE
    ),
    write_paragraph = get("write_paragraph", envir = env, inherits = TRUE),
    find_matches = get("find_matches", envir = env, inherits = TRUE),
    normalize_with_map = get(
      "normalize_with_map",
      envir = env,
      inherits = TRUE
    ),
    best_literal_substring = get(
      "best_literal_substring",
      envir = env,
      inherits = TRUE
    ),
    fuzzy_threshold = get("fuzzy_threshold", envir = env, inherits = TRUE),
    normalize_for_dist = get("normalize_for_dist", envir = env, inherits = TRUE)
  )
}


#' Globals shared by async code-generation workers
#'
#' @return Named list for `mirai::mirai(..., .args = ...)`.
analysis_async_code_generation_globals <- function(env = parent.frame()) {
  list(
    generate_codes_by_reading_texts = get(
      "generate_codes_by_reading_texts",
      envir = env,
      inherits = TRUE
    ),
    create_text_chunks = get(
      "create_text_chunks",
      envir = env,
      inherits = TRUE
    ),
    create_candidate_topics = get(
      "create_candidate_topics",
      envir = env,
      inherits = TRUE
    ),
    prompt_candidate_topics = get(
      "prompt_candidate_topics",
      envir = env,
      inherits = TRUE
    ),
    reduce_topics = get("reduce_topics", envir = env, inherits = TRUE),
    semchunk_load_chunker = get(
      "semchunk_load_chunker",
      envir = env,
      inherits = TRUE
    )
  )
}
