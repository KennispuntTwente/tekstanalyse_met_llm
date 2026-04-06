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
      semchunk_load_chunker = "async_message_printer",
      tiktoken_load_tokenizer = "async_message_printer",
      count_tokens = "tiktoken_load_tokenizer",
      write_paragraph = c(
        "send_prompt_with_retries",
        "get_context_window_size_in_tokens",
        "count_tokens",
        "tiktoken_load_tokenizer"
      ),
      write_grouped_paragraphs = "write_paragraph",
      normalize_for_dist = "normalize_with_map",
      best_literal_substring = c(
        "normalize_for_dist",
        "normalize_with_map",
        "fuzzy_threshold"
      ),
      find_matches = "best_literal_substring",
      mark_text_prompt = c(
        "find_matches",
        ".kwallm_marking_status_row",
        ".kwallm_marking_matches_from_find_matches"
      ),
      .kwallm_normalize_marking_matches = c(
        "find_matches",
        ".kwallm_empty_marking_matches",
        ".kwallm_marking_matches_from_find_matches"
      ),
      .kwallm_marking_clean_results = ".kwallm_marking_find_absolute_span",
      .kwallm_marking_collect_paragraph_inputs = ".kwallm_marking_build_highlighted_excerpt",
      mark_texts = c(
        "log_info",
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
        "normalize_for_dist",
        ".kwallm_normalize_marking_matches",
        ".kwallm_marking_clean_results",
        ".kwallm_marking_collect_paragraph_inputs",
        "write_grouped_paragraphs"
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
  prepare_async_analysis_worker_fn <- get(
    "prepare_async_analysis_worker",
    envir = env,
    inherits = TRUE
  )
  fn_env <- new.env(parent = environment(prepare_async_analysis_worker_fn))
  fn_env$async_inject_dependencies <- get(
    "async_inject_dependencies",
    envir = env,
    inherits = TRUE
  )
  fn_env$.analysis_async_dependency_map <- get(
    ".analysis_async_dependency_map",
    envir = env,
    inherits = TRUE
  )
  environment(prepare_async_analysis_worker_fn) <- fn_env

  list(
    prepare_async_analysis_worker = prepare_async_analysis_worker_fn
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
    collect_grouped_paragraph_inputs = get(
      "collect_grouped_paragraph_inputs",
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
    write_grouped_paragraphs = get(
      "write_grouped_paragraphs",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_empty_marking_matches = get(
      ".kwallm_empty_marking_matches",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_status_row = get(
      ".kwallm_marking_status_row",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_matches_from_find_matches = get(
      ".kwallm_marking_matches_from_find_matches",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_normalize_marking_matches = get(
      ".kwallm_normalize_marking_matches",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_find_absolute_span = get(
      ".kwallm_marking_find_absolute_span",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_clean_results = get(
      ".kwallm_marking_clean_results",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_build_highlighted_excerpt = get(
      ".kwallm_marking_build_highlighted_excerpt",
      envir = env,
      inherits = TRUE
    ),
    .kwallm_marking_collect_paragraph_inputs = get(
      ".kwallm_marking_collect_paragraph_inputs",
      envir = env,
      inherits = TRUE
    ),
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
    create_text_batches = get(
      "create_text_batches",
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
