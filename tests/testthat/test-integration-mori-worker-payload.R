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
