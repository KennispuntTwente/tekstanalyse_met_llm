library(testthat)

build_large_volume_analysis_texts <- function(n = 3000) {
  templates <- c(
    paste(
      "The invoice had duplicate charges, the refund was slow,",
      "and parcel tracking stayed incorrect."
    ),
    paste(
      "Support replied late, the help desk answer was generic,",
      "and the app login flow was confusing."
    ),
    paste(
      "Product quality felt unreliable because a part arrived damaged,",
      "while the brand also emphasized eco packaging."
    ),
    paste(
      "Delivery was fast, yet the dashboard and tracking page became confusing",
      "when the courier changed the schedule."
    ),
    paste(
      "The packaging sounded recyclable,",
      "but the invoice and refund policy were hard to understand."
    ),
    paste(
      "Customer support solved the issue eventually,",
      "but the replacement item still had a quality defect."
    )
  )

  vapply(
    seq_len(n),
    function(i) {
      template <- templates[[(i - 1L) %% length(templates) + 1L]]
      paste0(
        "Document ",
        sprintf("%04d", i),
        ": ",
        template,
        " Context note ",
        i,
        " compared similar experiences across teams."
      )
    },
    character(1)
  )
}


build_large_volume_marking_texts <- function(n = 3000) {
  templates <- c(
    "Refund request remained unresolved after duplicate charges appeared on the invoice.",
    "Support replied late and the help desk message stayed generic.",
    "Refund terms were confusing, but support eventually clarified the process.",
    "Support escalated the case, yet the refund still took too long.",
    "The refund policy looked simple, although support contact was difficult to find.",
    "Support solved the problem after a refund dispute over the invoice."
  )

  vapply(
    seq_len(n),
    function(i) {
      template <- templates[[(i - 1L) %% length(templates) + 1L]]
      paste0(
        "Marking document ",
        sprintf("%04d", i),
        ": ",
        template
      )
    },
    character(1)
  )
}


test_that("categorization async integration handles 3000 texts with fake LLM", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  source(here::here("R", "utils_async_message_printer.R"), local = TRUE)
  source(here::here("R", "utils_context_window.R"), local = TRUE)
  source(here::here("R", "utils_tokenizer.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)
  source(here::here("R", "utils_logger.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  old_opts <- options(
    logger__dir = file.path(
      tempdir(),
      "kwallm-categorization-large-volume-logs"
    ),
    logger__level = "INFO",
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  log_init(mode = "test")
  log_context <- log_context_capture(is_async = TRUE, mode = "test")

  texts <- build_large_volume_analysis_texts(3000)
  categories <- c(
    "Billing and refunds",
    "Delivery and shipping",
    "Customer support",
    "Product quality",
    "Sustainability",
    "App usability",
    "Unknown/not applicable"
  )

  provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)
  mirai::daemons(2)
  withr::defer(tryCatch(mirai::daemons(0), error = function(e) NULL))
  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      log_context_apply(log_context)
      prepare_async_analysis_worker("categorization")

      categorize_texts(
        texts = texts,
        analysis_unit_ids = analysis_unit_ids,
        categories = categories,
        research_background = research_background,
        llm_provider = llm_provider,
        assign_multiple_categories = TRUE,
        exclusive_categories = "Unknown/not applicable",
        on_progress = function(i, n, text) {
          if (i == 1 || i %% 500 == 0 || i == n) {
            log_info(
              sprintf("Categorization integration progress: %d/%d", i, n),
              component = "analysis"
            )
          }
        }
      )
    },
    .args = c(
      list(
        texts = texts,
        analysis_unit_ids = seq_along(texts),
        categories = categories,
        research_background = "",
        llm_provider = provider
      ),
      analysis_async_categorization_globals(),
      analysis_async_worker_setup_globals(),
      log_async_globals(log_context),
      send_prompt_with_retries_async_globals()
    )
  )

  results <- worker[]
  if (mirai::is_error_value(results)) {
    fail(paste("categorization worker error:", as.character(results)))
  }

  expect_identical(nrow(results), 3000L)
  expect_identical(sort(results$text), sort(texts))
  expect_identical(
    names(results),
    c("analysis_unit_id", "text", categories)
  )
  expect_identical(results$analysis_unit_id, seq_along(texts))
  expect_true(all(vapply(results[-c(1, 2)], is.logical, logical(1))))
  expect_true(all(rowSums(results[-c(1, 2)]) > 0))
  expect_true(sum(results[["Billing and refunds"]]) > 500)
  expect_true(sum(results[["Customer support"]]) >= 500)
  expect_true(sum(results[["Unknown/not applicable"]]) == 0)

  log_file <- file.path(
    getOption("logger__dir"),
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))

  log_lines <- readLines(log_file, warn = FALSE)
  progress_log <- log_lines[
    grepl("\\[async\\].*Categorization integration progress:", log_lines)
  ]

  expect_true(length(progress_log) >= 3)
  expect_true(any(grepl("3000/3000", progress_log, fixed = TRUE)))
})


test_that("scoring async integration handles 3000 texts with fake LLM", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  source(here::here("R", "utils_async_message_printer.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
  source(
    here::here("R", "analysis_deductive_scoring_characteristic.R"),
    local = TRUE
  )
  source(here::here("R", "utils_logger.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  old_opts <- options(
    logger__dir = file.path(tempdir(), "kwallm-scoring-large-volume-logs"),
    logger__level = "INFO",
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  log_init(mode = "test")
  log_context <- log_context_capture(is_async = TRUE, mode = "test")

  texts <- build_large_volume_analysis_texts(3000)
  provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)
  mirai::daemons(2)
  withr::defer(tryCatch(mirai::daemons(0), error = function(e) NULL))
  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      log_context_apply(log_context)
      prepare_async_analysis_worker("scoring")

      score_texts(
        texts = texts,
        analysis_unit_ids = analysis_unit_ids,
        scoring_characteristic = scoring_characteristic,
        research_background = research_background,
        llm_provider = llm_provider,
        on_progress = function(i, n, text) {
          if (i == 1 || i %% 500 == 0 || i == n) {
            log_info(
              sprintf("Scoring integration progress: %d/%d", i, n),
              component = "analysis"
            )
          }
        }
      )
    },
    .args = c(
      list(
        texts = texts,
        analysis_unit_ids = seq_along(texts),
        scoring_characteristic = "Customer satisfaction",
        research_background = "",
        llm_provider = provider
      ),
      analysis_async_scoring_globals(),
      analysis_async_worker_setup_globals(),
      log_async_globals(log_context),
      send_prompt_with_retries_async_globals()
    )
  )

  results <- worker[]
  if (mirai::is_error_value(results)) {
    fail(paste("scoring worker error:", as.character(results)))
  }

  expect_identical(nrow(results), 3000L)
  expect_identical(sort(results$text), sort(texts))
  expect_identical(results$analysis_unit_id, seq_along(texts))
  expect_true(is.numeric(results$result))
  expect_true(all(results$result >= 0 & results$result <= 100))
  expect_true(length(unique(results$result)) >= 3)
  expect_true(min(results$result) < max(results$result))

  log_file <- file.path(
    getOption("logger__dir"),
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))

  log_lines <- readLines(log_file, warn = FALSE)
  progress_log <- log_lines[
    grepl("\\[async\\].*Scoring integration progress:", log_lines)
  ]

  expect_true(length(progress_log) >= 3)
  expect_true(any(grepl("3000/3000", progress_log, fixed = TRUE)))
})


test_that("marking async integration handles 3000 texts with fake LLM", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  source(here::here("R", "utils_async_message_printer.R"), local = TRUE)
  source(here::here("R", "utils_context_window.R"), local = TRUE)
  source(here::here("R", "utils_tokenizer.R"), local = TRUE)
  source(here::here("R", "utils_semchunk.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
  source(here::here("R", "analysis_marking.R"), local = TRUE)
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)
  source(here::here("R", "utils_logger.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  old_opts <- options(
    logger__dir = file.path(tempdir(), "kwallm-marking-large-volume-logs"),
    logger__level = "INFO",
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  log_init(mode = "test")
  log_context <- log_context_capture(is_async = TRUE, mode = "test")

  texts <- build_large_volume_marking_texts(3000)
  codes <- c("refund", "support")
  provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)
  mirai::daemons(2)
  withr::defer(tryCatch(mirai::daemons(0), error = function(e) NULL))
  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      log_context_apply(log_context)
      prepare_async_analysis_worker("marking")

      mark_texts(
        texts = texts,
        analysis_unit_ids = analysis_unit_ids,
        codes = codes,
        text_size_tokens = text_size_tokens,
        overlap_size_tokens = overlap_size_tokens,
        research_background = research_background,
        llm_provider = llm_provider,
        write_paragraphs = FALSE,
        lang = NULL
      )
    },
    .args = c(
      list(
        texts = texts,
        analysis_unit_ids = seq_along(texts),
        codes = codes,
        text_size_tokens = 256,
        overlap_size_tokens = 0,
        research_background = "",
        llm_provider = provider
      ),
      analysis_async_marking_globals(),
      analysis_async_worker_setup_globals(),
      analysis_async_tokenizer_globals(),
      log_async_globals(log_context),
      send_prompt_with_retries_async_globals()
    )
  )

  results <- worker[]
  if (mirai::is_error_value(results)) {
    fail(paste("marking worker error:", as.character(results)))
  }

  expect_true(nrow(results) >= 3000L)
  expect_true(all(results$code %in% codes))
  expect_identical(length(unique(results$analysis_unit_id)), 3000L)
  expect_true(all(vapply(results$marked_text, is.character, logical(1))))
  expect_true(!all(is.na(results$marked_text)))
  expect_true(all(
    results$marked_text[!is.na(results$marked_text)] ==
      results$chunk_text[!is.na(results$marked_text)]
  ))
  expect_true(all(nchar(results$marked_text[!is.na(results$marked_text)]) > 0))

  log_file <- file.path(
    getOption("logger__dir"),
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))

  log_lines <- readLines(log_file, warn = FALSE)
  progress_log <- log_lines[
    grepl("\\[async\\].*Marking progress:", log_lines)
  ]

  expect_true(length(progress_log) >= 10)
  expect_true(any(grepl("6000/6000", progress_log, fixed = TRUE)))
})
