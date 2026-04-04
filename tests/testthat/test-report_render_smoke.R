library(testthat)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

source(here::here("R", "result_model.R"), local = TRUE)
source(here::here("R", "result_builders.R"), local = TRUE)
source(here::here("R", "result_serializers.R"), local = TRUE)
source(here::here("R", "utils_processing_helpers.R"), local = TRUE)

.render_test_models <- function() {
  list(
    main = list(
      parameters = list(model = "test-model"),
      url = "https://api.example.com/v1/chat/completions"
    ),
    large = list(
      parameters = list(model = "test-large-model"),
      url = "https://api.example.com/v1/chat/completions"
    )
  )
}

.report_render_env <- function(parent = environment()) {
  list2env(analysis_result_report_globals(), parent = parent)
}

.report_language_from_path <- function(report_path) {
  if (grepl("_en\\.Rmd$", basename(report_path))) {
    return("en")
  }

  "nl"
}

.build_smoke_analysis_result <- function(
  report_path,
  by_column_name = NULL,
  by_column_lookup = NULL,
  source_texts = NULL
) {
  nm <- basename(report_path)
  language <- .report_language_from_path(report_path)

  irr_kappa <- list(
    subjects = 10,
    raters = 2,
    irr.name = "Kappa",
    stat.name = "z",
    statistic = 1.0,
    p.value = 0.5,
    value = 0.4
  )

  irr_t <- list(
    subjects = 10,
    estimate = 0.0,
    statistic = 0.0,
    p.value = 1.0,
    parameter = 9,
    conf.low = -1,
    conf.high = 1,
    method = "Paired t-test",
    alternative = "two.sided",
    llm_mean = 20,
    llm_sd = 1,
    user_mean = 20,
    user_sd = 1,
    sensitivity_sentence = ""
  )

  if (grepl("Markeren", nm, fixed = TRUE)) {
    texts_df <- data.frame(
      raw = "A long text",
      preprocessed = "A long text",
      stringsAsFactors = FALSE
    )
    results_table <- data.frame(
      text = "A long text",
      sub_text = "A long text",
      code = "Code 1",
      marked_text = "long",
      stringsAsFactors = FALSE
    )

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      uuid = paste0("smoke-", nm),
      mode = "Markeren",
      research_background = "",
      style_prompt = NULL,
      irr_result = NULL,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      codes = "Code 1",
      assign_multiple_categories = FALSE,
      human_in_the_loop = FALSE,
      write_paragraphs = FALSE,
      stage_prompt_previews = list(marking = "prompt"),
      source_texts = source_texts
    ))
  }

  if (grepl("Scoren", nm, fixed = TRUE)) {
    texts_df <- data.frame(
      raw = c("Text 1", "Text 2"),
      preprocessed = c("Text 1", "Text 2"),
      stringsAsFactors = FALSE
    )
    results_table <- data.frame(
      text = c("Text 1", "Text 2"),
      result = c(10, 20),
      stringsAsFactors = FALSE
    )

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      uuid = paste0("smoke-", nm),
      mode = "Scoren",
      research_background = "",
      style_prompt = NULL,
      irr_result = irr_t,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      scoring_characteristic = "test characteristic",
      write_paragraphs = FALSE,
      stage_prompt_previews = list(scoring = "prompt"),
      source_texts = source_texts
    ))
  }

  if (grepl("Onderwerpextractie", nm, fixed = TRUE)) {
    texts_df <- data.frame(
      raw = c("Text 1", "Text 2"),
      preprocessed = c("Text 1", "Text 2"),
      stringsAsFactors = FALSE
    )
    results_table <- data.frame(
      text = c("Text 1", "Text 2"),
      result = c("Topic A", "Topic B"),
      stringsAsFactors = FALSE
    )

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      uuid = paste0("smoke-", nm),
      mode = "Onderwerpextractie",
      research_background = "",
      style_prompt = NULL,
      irr_result = irr_kappa,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      topics = c("Topic A", "Topic B"),
      exclusive_topics = character(),
      assign_multiple_categories = FALSE,
      human_in_the_loop = FALSE,
      write_paragraphs = FALSE,
      context_window = list(
        chunk_size = 5,
        draws = 2,
        n_chunks = 2,
        n_tokens_context_window = 1000
      ),
      stage_prompt_previews = list(
        topic_candidate_generation = "candidate prompt",
        topic_reduction = "reduction prompt",
        topic_assignment = "assignment prompt"
      ),
      candidate_topics = c("Topic A", "Topic B"),
      reduced_topics = c("Topic A", "Topic B"),
      topics_were_edited = FALSE,
      source_texts = source_texts
    ))
  }

  texts_df <- data.frame(
    raw = c("Text 1", "Text 2"),
    preprocessed = c("Text 1", "Text 2"),
    stringsAsFactors = FALSE
  )
  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = paste0("smoke-", nm),
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    irr_result = irr_kappa,
    language = language,
    by_column_name = by_column_name,
    by_column_lookup = by_column_lookup,
    models = .render_test_models(),
    categories = c("A", "B"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt"),
    source_texts = source_texts
  )
}

test_that("report templates render (smoke)", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")

  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  report_dir <- here::here("R")
  reports <- list.files(
    report_dir,
    pattern = "^report_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(reports) > 0)

  out_dir <- withr::local_tempdir()

  withr::with_dir(here::here(), {
    for (report_path in reports) {
      out_file <- file.path(
        out_dir,
        paste0(tools::file_path_sans_ext(basename(report_path)), ".html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_smoke_analysis_result(report_path)
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)
    }
  })
})


test_that("Categorisatie report renders with by_column_* set", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  irr_kappa <- list(
    subjects = 10,
    raters = 2,
    irr.name = "Kappa",
    stat.name = "z",
    statistic = 1.0,
    p.value = 0.5,
    value = 0.4
  )

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Categorisatie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      out_file <- file.path(
        out_dir,
        paste0(tools::file_path_sans_ext(basename(report_path)), ".html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_smoke_analysis_result(
              report_path,
              by_column_name = "group",
              by_column_lookup = data.frame(
                text = c("Text 1", "Text 2", "Text 3"),
                by_value = c("G1", "G1", "G2"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)
    }
  })
})
