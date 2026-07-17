library(testthat)


skip_mori_integration <- function() {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("mori")
  testthat::skip_if_not_installed("openssl")
  testthat::skip_if_not_installed("digest")
}


kwallm_mori_bootstrapped_worker <- function(expr, args, n_daemons = 1L) {
  skip_mori_integration()
  kwallm_test_start_mirai_daemons(n = n_daemons)

  worker <- mirai::mirai(
    eval(expr),
    .args = c(
      list(
        expr = expr,
        app_root = normalizePath(here::here(), winslash = "/", mustWork = TRUE),
        worker_options = list()
      ),
      args,
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]
  if (mirai::is_error_value(result)) {
    fail(paste("mirai worker error:", as.character(result)))
  }

  result
}


test_that("mori payload round-trips through a real bootstrapped worker", {
  payload <- kwallm_mori_share_worker_payload(
    list(
      texts = c("alpha", "beta", "gamma"),
      analysis_unit_ids = c(10L, 11L, 12L)
    ),
    enabled = TRUE
  )
  guard <- payload$guard

  result <- kwallm_mori_bootstrapped_worker(
    quote({
      kwallm_worker_bootstrap(
        task = "mori_round_trip",
        app_root = app_root,
        worker_options = worker_options
      )

      list(
        texts = as.character(kwallm_mori_resolve_worker_arg(
          texts,
          mori_scope_key
        )),
        analysis_unit_ids = as.integer(kwallm_mori_resolve_worker_arg(
          analysis_unit_ids,
          mori_scope_key
        ))
      )
    }),
    args = list(
      texts = payload$args$texts,
      analysis_unit_ids = payload$args$analysis_unit_ids,
      mori_scope_key = payload$scope_key
    )
  )
  force(guard)

  expect_identical(result$texts, c("alpha", "beta", "gamma"))
  expect_identical(result$analysis_unit_ids, c(10L, 11L, 12L))
})


test_that("bootstrapped workers reject wrong mori scope keys", {
  payload <- kwallm_mori_share_worker_payload(
    list(texts = c("private text")),
    enabled = TRUE
  )
  guard <- payload$guard

  result <- kwallm_mori_bootstrapped_worker(
    quote({
      kwallm_worker_bootstrap(
        task = "mori_wrong_key",
        app_root = app_root,
        worker_options = worker_options
      )

      tryCatch(
        {
          kwallm_mori_resolve_worker_arg(texts, wrong_scope_key)
          "unexpected-success"
        },
        error = function(e) conditionMessage(e)
      )
    }),
    args = list(
      texts = payload$args$texts,
      wrong_scope_key = kwallm_mori_random_token()
    )
  )
  force(guard)

  expect_match(result, "Rejected invalid mori worker payload capability")
})


test_that("mori payload capability keys isolate concurrent worker payloads", {
  first <- kwallm_mori_share_worker_payload(
    list(texts = c("session-a")),
    enabled = TRUE
  )
  second <- kwallm_mori_share_worker_payload(
    list(texts = c("session-b")),
    enabled = TRUE
  )
  guard <- c(first$guard, second$guard)

  result <- kwallm_mori_bootstrapped_worker(
    quote({
      kwallm_worker_bootstrap(
        task = "mori_isolation",
        app_root = app_root,
        worker_options = worker_options
      )

      list(
        first = as.character(kwallm_mori_resolve_worker_arg(
          first_ref,
          first_key
        )),
        second = as.character(kwallm_mori_resolve_worker_arg(
          second_ref,
          second_key
        )),
        cross = tryCatch(
          {
            kwallm_mori_resolve_worker_arg(second_ref, first_key)
            "unexpected-success"
          },
          error = function(e) conditionMessage(e)
        )
      )
    }),
    args = list(
      first_ref = first$args$texts,
      first_key = first$scope_key,
      second_ref = second$args$texts,
      second_key = second$scope_key
    ),
    n_daemons = 2L
  )
  force(guard)

  expect_identical(result$first, "session-a")
  expect_identical(result$second, "session-b")
  expect_match(result$cross, "Rejected invalid mori worker payload capability")
})


test_that("concurrent workers preserve mori attributes and copy-on-write", {
  skip_mori_integration()
  kwallm_test_start_mirai_daemons(n = 2L)

  source_data <- data.frame(
    label = c("alpha", "beta", NA_character_),
    group = factor(c("a", "b", "a"), levels = c("a", "b", "unused")),
    observed_on = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    value = c(1, NA_real_, 3),
    stringsAsFactors = FALSE
  )
  payload <- kwallm_mori_share_worker_payload(
    list(data = source_data),
    enabled = TRUE
  )
  guard <- payload$guard

  worker_expr <- quote({
    kwallm_worker_bootstrap(
      task = "mori_concurrent_copy_on_write",
      app_root = app_root,
      worker_options = worker_options
    )
    mapped <- kwallm_mori_resolve_worker_arg(data_ref, mori_scope_key)
    before <- list(
      labels = as.character(mapped$label),
      groups = as.character(mapped$group),
      group_levels = levels(mapped$group),
      dates = as.character(mapped$observed_on),
      date_class = class(mapped$observed_on),
      missing = is.na(mapped$value)
    )
    Sys.sleep(0.1)
    mapped$label[[1L]] <- replacement
    list(before = before, mutated_label = mapped$label[[1L]])
  })
  worker_args <- function(replacement) {
    c(
      list(
        expr = worker_expr,
        app_root = normalizePath(here::here(), winslash = "/", mustWork = TRUE),
        worker_options = list(),
        data_ref = payload$args$data,
        mori_scope_key = payload$scope_key,
        replacement = replacement
      ),
      kwallm_worker_bootstrap_globals()
    )
  }

  first <- mirai::mirai(eval(expr), .args = worker_args("worker-a"))
  second <- mirai::mirai(eval(expr), .args = worker_args("worker-b"))
  first_result <- first[]
  second_result <- second[]
  force(guard)

  for (result in list(first_result, second_result)) {
    if (mirai::is_error_value(result)) {
      fail(paste("mirai worker error:", as.character(result)))
      next
    }
    expect_identical(result$before$labels, source_data$label)
    expect_identical(result$before$groups, as.character(source_data$group))
    expect_identical(result$before$group_levels, levels(source_data$group))
    expect_identical(result$before$dates, as.character(source_data$observed_on))
    expect_identical(result$before$date_class, "Date")
    expect_identical(result$before$missing, is.na(source_data$value))
  }
  expect_identical(first_result$mutated_label, "worker-a")
  expect_identical(second_result$mutated_label, "worker-b")

  host_mapping <- kwallm_mori_resolve_worker_arg(
    payload$args$data,
    payload$scope_key
  )
  expect_identical(as.character(host_mapping$label), source_data$label)
  kwallm_mori_release_guard(guard)
})


test_that("serialized AnalysisResult travels through mori shared memory", {
  skip_mori_integration()

  analysis_result <- AnalysisResult(
    metadata = AnalysisMetadata(
      run_id = "mori-download-test",
      mode_id = "scoring",
      language = "en",
      timestamp = Sys.time(),
      research_background = ""
    ),
    text_lineage = TextLineage(
      source_documents = data.frame(
        source_document_id = 1L,
        source_document_text = "Shared source text",
        stringsAsFactors = FALSE
      ),
      documents = data.frame(
        document_id = 1L,
        source_document_id = 1L,
        document_text = "Shared source text",
        stringsAsFactors = FALSE
      ),
      analysis_units = data.frame(
        analysis_unit_id = 1L,
        preprocessed_text = "Shared source text",
        stringsAsFactors = FALSE
      ),
      document_units = data.frame(
        document_id = 1L,
        analysis_unit_id = 1L,
        stringsAsFactors = FALSE
      )
    ),
    results = ScoringResult(
      scores = data.frame(
        analysis_unit_id = 1L,
        score = 10,
        stringsAsFactors = FALSE
      ),
      characteristic = "helpfulness"
    ),
    mode_config = ScoringConfig(scoring_characteristic = "helpfulness")
  )

  payload <- kwallm_mori_share_worker_payload(
    list(analysis_result = serialize(analysis_result, NULL, version = 3)),
    enabled = TRUE
  )
  guard <- payload$guard

  result <- kwallm_mori_bootstrapped_worker(
    quote({
      kwallm_worker_bootstrap(
        task = "mori_serialized_analysis_result",
        app_root = app_root,
        worker_options = worker_options
      )
      serialized <- kwallm_mori_resolve_worker_arg(
        serialized,
        mori_scope_key
      )
      restored <- unserialize(serialized)
      list(
        is_analysis_result = inherits(restored, "AnalysisResult"),
        score = restored@results@scores$score,
        document_text = restored@text_lineage@documents$document_text
      )
    }),
    args = list(
      serialized = payload$args$analysis_result,
      mori_scope_key = payload$scope_key
    )
  )
  force(guard)
  kwallm_mori_release_guard(guard)

  expect_true(result$is_analysis_result)
  expect_identical(result$score, 10)
  expect_identical(result$document_text, "Shared source text")
})
