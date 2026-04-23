library(testthat)
source(here::here("R", "utils_prompt_sanitization.R"), local = TRUE)


test_that("categorization async integration writes paragraphs in a real mirai worker", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_dir(here::here())

  source(here::here("R", "load_dependencies.R"), local = TRUE)
  source(here::here("R", "utils_async_message_printer.R"), local = TRUE)
  source(here::here("R", "utils_context_window.R"), local = TRUE)
  source(here::here("R", "utils_tokenizer.R"), local = TRUE)
  source(here::here("R", "utils_send_prompt_with_retries.R"), local = TRUE)
  source(here::here("R", "utils_async_analysis_workers.R"), local = TRUE)
  source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
  source(
    here::here("R", "analysis_deductive_categorization.R"),
    local = TRUE
  )
  source(here::here("R", "analysis_write_paragraph.R"), local = TRUE)
  source(here::here("R", "utils_test_llm_provider.R"), local = TRUE)

  old_opts <- options(
    kwallm.test_fake_llm = TRUE,
    tidyprompt.warn.auto.json = FALSE
  )
  withr::defer(options(old_opts), testthat::teardown_env())

  texts <- c(
    "I loved the quick support reply and the helpful service.",
    "The support reply was late and the service felt unhelpful.",
    "Great experience overall and I would recommend it.",
    "A bad experience overall because the help desk replied too late."
  )
  categories <- c("Positive feedback", "Negative feedback")
  provider <- kwallm_test_llm_provider("kwallm-fake-main-1024")

  tryCatch(mirai::daemons(0), error = function(e) NULL)
  mirai::daemons(2)
  withr::defer(tryCatch(mirai::daemons(0), error = function(e) NULL))
  Sys.sleep(0.5)

  worker <- mirai::mirai(
    {
      kwallm_worker_bootstrap(
        task = "categorization",
        app_root = app_root,
        worker_options = worker_options
      )

      results <- categorize_texts(
        texts = texts,
        analysis_unit_ids = seq_along(texts),
        categories = categories,
        research_background = "Paragraph integration test",
        llm_provider = llm_provider,
        assign_multiple_categories = TRUE,
        exclusive_categories = NULL
      )

      grouped_inputs <- collect_grouped_paragraph_inputs(
        results = results,
        labels = categories,
        assign_multiple_categories = TRUE
      )

      lang <- list(
        t = function(value) value,
        get_translation_language = function() "en"
      )

      paragraphs <- write_grouped_paragraphs(
        grouped_texts = grouped_inputs,
        research_background = "Paragraph integration test",
        style_prompt = "",
        llm_provider = llm_provider,
        lang = lang,
        subject_kind = "category",
        streaming_enabled = FALSE
      )

      list(
        results = results,
        paragraphs = paragraphs
      )
    },
    .args = c(
      list(
        app_root = kwallm_worker_app_root(),
        worker_options = kwallm_worker_capture_options(),
        texts = texts,
        categories = categories,
        llm_provider = provider
      ),
      kwallm_worker_bootstrap_globals()
    )
  )

  result <- worker[]
  if (mirai::is_error_value(result)) {
    fail(paste(
      "categorization paragraph worker error:",
      conditionMessage(result)
    ))
  }

  expect_named(result, c("results", "paragraphs"))
  expect_true(length(result$paragraphs) > 0)
  expect_true(all(names(result$paragraphs) %in% categories))

  first_paragraph <- result$paragraphs[[1]]
  expect_true(is.character(first_paragraph$paragraph))
  expect_true(nchar(first_paragraph$paragraph) > 0)
  expect_true(is.logical(first_paragraph$prompt_fits))
  expect_true(isTRUE(first_paragraph$prompt_fits))
  expect_true(is.character(first_paragraph$texts))
  expect_true(length(first_paragraph$texts) > 0)
  expect_true(is.numeric(first_paragraph$analysis_unit_ids))
  expect_identical(
    length(first_paragraph$analysis_unit_ids),
    length(first_paragraph$texts)
  )
})
