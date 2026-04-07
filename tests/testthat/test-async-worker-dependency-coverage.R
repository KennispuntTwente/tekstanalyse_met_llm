library(testthat)


analysis_async_is_app_function <- function(name, env) {
  if (!exists(name, envir = env, inherits = TRUE)) {
    return(FALSE)
  }

  obj <- get(name, envir = env, inherits = TRUE)
  is.function(obj) && identical(environment(obj), env)
}


analysis_async_direct_app_dependencies <- function(fn_name, env) {
  fn <- get(fn_name, envir = env, inherits = TRUE)

  globals <- codetools::findGlobals(fn, merge = FALSE)$functions
  globals <- unlist(globals, use.names = FALSE)
  globals <- unique(globals[vapply(
    globals,
    function(x) is.character(x) && length(x) == 1 && nzchar(x),
    logical(1)
  )])

  sort(unique(globals[vapply(
    globals,
    analysis_async_is_app_function,
    logical(1),
    env = env
  )]))
}


test_that("marking helper empty-match path works in a real mirai worker", {
  skip_if_not_installed("mirai")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)

  can_start_daemons <- TRUE
  tryCatch(
    {
      mirai::daemons(1)
      on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
    },
    error = function(e) {
      can_start_daemons <<- FALSE
    }
  )
  if (!isTRUE(can_start_daemons)) {
    skip("mirai daemons not available in this environment")
  }

  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      prepare_async_analysis_worker("marking")

      .kwallm_marking_matches_from_find_matches(
        tibble::tibble(
          needle = character(),
          match = character(),
          start = integer(),
          end = integer(),
          distance = integer()
        ),
        response_status = "matched_all"
      )
    },
    .args = c(
      analysis_async_marking_globals(),
      analysis_async_worker_setup_globals(),
      analysis_async_tokenizer_globals(),
      send_prompt_with_retries_async_globals()
    )
  )

  result <- worker[]

  if (mirai::is_error_value(result)) {
    fail(paste("mirai worker error:", as.character(result)))
  }

  expect_s3_class(result, "tbl_df")
  expect_identical(nrow(result), 1L)
  expect_identical(result$response_status[[1]], "matched_all")
  expect_true(all(is.na(result$marked_text)))
})


test_that("async dependency maps cover direct app-function calls in worker helpers", {
  helper_env <- environment(analysis_async_tokenizer_globals)
  tasks <- c(
    "text_split",
    "gliner",
    "categorization",
    "scoring",
    "topic_generation",
    "topic_reduction",
    "topic_assignment",
    "code_generation",
    "marking"
  )

  # Logging helpers are validated separately and are usually wrapped in
  # try/tryCatch inside analysis helpers, so they should not make this test
  # fail when the goal is catching crash-causing missing worker bindings.
  ignorable_functions <- c(
    "log_info",
    "log_debug",
    "log_warn",
    "log_error"
  )

  # This helper brings its own async-globals bundle and resolves its internal
  # helpers dynamically, so we only need worker helpers to declare direct calls
  # to it, not its entire internal dependency tree here.
  self_contained_functions <- "send_prompt_with_retries"

  for (task in tasks) {
    dep_map <- .analysis_async_dependency_map(task)
    inspect_fns <- unique(c(names(dep_map), unlist(dep_map, use.names = FALSE)))
    inspect_fns <- inspect_fns[vapply(
      inspect_fns,
      analysis_async_is_app_function,
      logical(1),
      env = helper_env
    )]

    for (fn_name in inspect_fns) {
      if (fn_name %in% c(ignorable_functions, self_contained_functions)) {
        next
      }

      direct_deps <- setdiff(
        analysis_async_direct_app_dependencies(fn_name, env = helper_env),
        c(fn_name, ignorable_functions)
      )

      declared_deps <- unique(c(
        as.character(dep_map[[fn_name]]),
        self_contained_functions
      ))

      missing <- setdiff(direct_deps, declared_deps)

      expect_true(
        length(missing) == 0,
        info = sprintf(
          paste0(
            "Task '%s': helper '%s' directly calls app functions [%s] ",
            "that are not declared in .analysis_async_dependency_map()."
          ),
          task,
          fn_name,
          paste(missing, collapse = ", ")
        )
      )
    }
  }
})
