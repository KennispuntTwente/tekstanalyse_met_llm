library(testthat)

build_large_volume_topic_texts <- function(n = 3000) {
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


test_that("topic modelling async integration handles 3000 texts with fake LLM", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())
  set.seed(1)

  source(here::here("R", "utils_async_message_printer.R"), local = TRUE)
  source(here::here("R", "utils_context_window.R"), local = TRUE)
  source(here::here("R", "utils_tokenizer.R"), local = TRUE)
  source(here::here("R", "utils_create_text_batches.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
  source(here::here("R", "analysis_deductive_categorization.R"), local = TRUE)
  source(here::here("R", "analysis_inductive_topic_modelling.R"), local = TRUE)
  source(here::here("R", "utils_logger.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  old_opts <- options(
    logger__dir = file.path(tempdir(), "kwallm-large-volume-logs"),
    logger__level = "INFO",
    topic_modelling__always_add_not_applicable = TRUE,
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  log_init(mode = "test")
  log_context <- log_context_capture(is_async = TRUE, mode = "test")

  texts <- build_large_volume_topic_texts(3000)
  base_prompt_text <- prompt_candidate_topics(
    text_batch = c(""),
    research_background = "",
    language = "en"
  ) |>
    tidyprompt::construct_prompt_text()

  text_batches <- create_text_batches(
    texts = texts,
    batch_size = 25,
    draws = 1,
    n_tokens_context_window = 1024,
    base_prompt_text = base_prompt_text
  )

  expect_false(is.null(text_batches))
  expect_true(length(text_batches) >= 100)

  main_provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")
  large_provider <- kwallm_test_llm_provider("kwallm-fake-reducer-320")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  Sys.sleep(0.2)
  mirai::daemons(2)
  withr::defer(tryCatch(mirai::daemons(0), error = function(e) NULL))
  Sys.sleep(0.5)

  generation_worker <- mirai::mirai(
    {
      log_context_apply(log_context)

      candidate_topics <- create_candidate_topics(
        text_batches = text_batches,
        research_background = research_background,
        llm_provider = llm_provider_main,
        language = "en"
      )

      reduced_topics <- reduce_topics(
        candidate_topics = candidate_topics,
        research_background = research_background,
        llm_provider = llm_provider_large,
        language = "en"
      )

      list(
        n_batches = length(text_batches),
        n_candidates = length(candidate_topics),
        reduced_topics = reduced_topics
      )
    },
    .args = c(
      list(
        text_batches = text_batches,
        research_background = "",
        llm_provider_main = main_provider,
        llm_provider_large = large_provider
      ),
      analysis_async_topic_modelling_globals(),
      analysis_async_tokenizer_globals(),
      log_async_globals(log_context),
      send_prompt_with_retries_async_globals()
    )
  )

  generation_result <- generation_worker[]
  if (mirai::is_error_value(generation_result)) {
    fail(paste(
      "topic generation worker error:",
      as.character(generation_result)
    ))
  }

  expect_true(generation_result$n_batches >= 100)
  expect_true(generation_result$n_candidates >= 200)
  expect_true(length(generation_result$reduced_topics) <= 7)
  expect_true("Unknown/not applicable" %in% generation_result$reduced_topics)

  assignment_worker <- mirai::mirai(
    {
      log_context_apply(log_context)
      prepare_async_analysis_worker("topic_assignment")

      assign_topics(
        texts = texts,
        analysis_unit_ids = seq_along(texts),
        topics = topics,
        research_background = research_background,
        llm_provider = llm_provider,
        assign_multiple_categories = TRUE,
        exclusive_topics = "Unknown/not applicable"
      )
    },
    .args = c(
      list(
        texts = texts,
        topics = generation_result$reduced_topics,
        research_background = "",
        llm_provider = main_provider
      ),
      analysis_async_topic_modelling_globals(),
      analysis_async_worker_setup_globals(),
      log_async_globals(log_context),
      send_prompt_with_retries_async_globals()
    )
  )

  results <- assignment_worker[]
  if (mirai::is_error_value(results)) {
    fail(paste("topic assignment worker error:", as.character(results)))
  }

  expect_identical(nrow(results), 3000L)
  expect_identical(results$analysis_unit_id, seq_along(texts))
  expect_identical(sort(results$text), sort(texts))
  expect_true(ncol(results) >= 5)
  topic_columns <- setdiff(names(results), c("analysis_unit_id", "text"))
  expect_true(all(vapply(results[topic_columns], is.logical, logical(1))))
  expect_true(all(rowSums(results[topic_columns]) > 0))

  log_file <- file.path(
    getOption("logger__dir"),
    paste0(format(Sys.Date(), "%Y-%m-%d"), ".log")
  )
  expect_true(file.exists(log_file))
  log_lines <- readLines(log_file, warn = FALSE)

  topic_generation_log <- log_lines[
    grepl("Topic generation: n_batches=", log_lines, fixed = TRUE)
  ]
  topic_reduction_log <- log_lines[
    grepl("Topic reduction complete: n_input=", log_lines, fixed = TRUE)
  ]

  expect_true(length(topic_generation_log) >= 1)
  expect_true(length(topic_reduction_log) >= 1)

  generation_match <- stringr::str_match(
    utils::tail(topic_generation_log, 1),
    "n_batches=(\\d+), n_candidates=(\\d+)"
  )
  reduction_match <- stringr::str_match(
    utils::tail(topic_reduction_log, 1),
    "n_input=(\\d+), n_output=(\\d+), iterations=(\\d+)"
  )

  expect_true(as.integer(generation_match[, 2]) >= 100)
  expect_true(as.integer(generation_match[, 3]) >= 200)
  expect_true(as.integer(reduction_match[, 2]) >= 200)
  expect_true(as.integer(reduction_match[, 3]) <= 7)
  expect_true(as.integer(reduction_match[, 4]) >= 2)
})
