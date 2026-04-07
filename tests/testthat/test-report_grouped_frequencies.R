library(testthat)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

# Source helpers needed by the grouped-frequency functions
source(here::here("R", "result_model.R"), local = TRUE)
source(here::here("R", "result_builders.R"), local = TRUE)
source(here::here("R", "result_serializers.R"), local = TRUE)
source(here::here("R", "utils_processing_helpers.R"), local = TRUE)
source(here::here("R", "style_datatable_config.R"), local = TRUE)
source(here::here("R", "report_grouped_frequencies.R"), local = TRUE)

.grouped_report_models <- function() {
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

.grouped_render_env <- function(parent = environment()) {
  list2env(analysis_result_report_globals(), parent = parent)
}

.grouped_language_from_path <- function(report_path) {
  if (grepl("_en\\.Rmd$", basename(report_path))) {
    return("en")
  }

  "nl"
}

.make_grouped_texts_df <- function(
  document_text,
  preprocessed = document_text,
  source_document_id = seq_along(document_text),
  source_document_text = document_text,
  analysis_unit_id = match(preprocessed, unique(preprocessed))
) {
  data.frame(
    source_document_id = as.integer(source_document_id),
    document_id = seq_along(document_text),
    source_document_text = as.character(source_document_text),
    document_text = as.character(document_text),
    preprocessed = as.character(preprocessed),
    analysis_unit_id = as.integer(analysis_unit_id),
    stringsAsFactors = FALSE
  )
}

.build_grouped_categorization_result <- function(
  report_path,
  results_table,
  by_column_lookup,
  texts_df = NULL
) {
  texts_df <- texts_df %||% .make_grouped_texts_df(results_table$text)

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = paste0("grouped-", basename(report_path)),
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = .grouped_language_from_path(report_path),
    by_column_name = "group",
    by_column_lookup = by_column_lookup,
    models = .grouped_report_models(),
    categories = c("A", "B"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(categorization = "prompt")
  )
}

.build_grouped_scoring_result <- function(
  report_path,
  results_table,
  by_column_lookup,
  texts_df = NULL
) {
  texts_df <- texts_df %||% .make_grouped_texts_df(results_table$text)

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = paste0("grouped-", basename(report_path)),
    mode = "Scoren",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = .grouped_language_from_path(report_path),
    by_column_name = "group",
    by_column_lookup = by_column_lookup,
    models = .grouped_report_models(),
    scoring_characteristic = "test characteristic",
    write_paragraphs = FALSE,
    stage_prompt_previews = list(scoring = "prompt")
  )
}

.build_grouped_topic_result <- function(
  report_path,
  results_table,
  by_column_lookup,
  texts_df = NULL,
  topics = c("Topic A", "Topic B"),
  assign_multiple_categories = FALSE
) {
  texts_df <- texts_df %||% .make_grouped_texts_df(results_table$text)

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = paste0("grouped-", basename(report_path)),
    mode = "Onderwerpextractie",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = .grouped_language_from_path(report_path),
    by_column_name = "group",
    by_column_lookup = by_column_lookup,
    models = .grouped_report_models(),
    topics = topics,
    exclusive_topics = character(),
    assign_multiple_categories = assign_multiple_categories,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    context_window = list(
      batch_size = 5,
      draws = 2,
      n_batches = 2,
      n_tokens_context_window = 1000
    ),
    stage_prompt_previews = list(
      topic_candidate_generation = "candidate prompt",
      topic_reduction = "reduction prompt",
      topic_assignment = "assignment prompt"
    ),
    candidate_topics = topics,
    reduced_topics = topics,
    topics_were_edited = FALSE
  )
}

.build_grouped_marking_result <- function(
  report_path,
  results_table,
  by_column_lookup,
  texts_df = NULL,
  codes = c("Code 1", "Code 2")
) {
  if (is.null(texts_df)) {
    document_text <- unique(as.character(results_table$text))
    if (!length(document_text)) {
      document_text <- "Text 1"
    }
    texts_df <- .make_grouped_texts_df(document_text = document_text)
  }

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    uuid = paste0("grouped-", basename(report_path)),
    mode = "Markeren",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = .grouped_language_from_path(report_path),
    by_column_name = "group",
    by_column_lookup = by_column_lookup,
    models = .grouped_report_models(),
    codes = codes,
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = FALSE,
    stage_prompt_previews = list(marking = "prompt")
  )
}

# -- Unit tests for .join_by_group and grouped helpers -------------------------

test_that(".join_by_group works with data frame by_values", {
  df <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 2"),
    by_value = c("G1", "G2"),
    stringsAsFactors = FALSE
  )

  out <- .join_by_group(df, by_vals)
  expect_equal(nrow(out), 2)
  expect_equal(out$.by_group, c("G1", "G2"))
})

test_that(".join_by_group fans out duplicate texts in different groups", {
  # Deduped result df has 2 rows, but by_values maps "Text 1" to two groups
  df <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 1", "Text 2"),
    by_value = c("G1", "G2", "G1"),
    stringsAsFactors = FALSE
  )

  out <- .join_by_group(df, by_vals)
  # "Text 1" should appear twice (once per group), "Text 2" once
  expect_equal(nrow(out), 3)
  expect_equal(sort(out$.by_group), c("G1", "G1", "G2"))
})

test_that(".join_by_group uses document_id to avoid overcounting same-text rows", {
  df <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 1"),
    result = c("A", "A"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 1"),
    by_value = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  out <- .join_by_group(df, by_vals)

  expect_equal(nrow(out), 2)
  expect_equal(out$document_id, c(1L, 2L))
  expect_equal(out$.by_group, c("G1", "G1"))
})

test_that(".join_by_group handles result df rows without a group match", {
  # e.g. text splitting created a row not in the by_values lookup
  df <- data.frame(
    text = c("Text 1", "Split chunk"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = "Text 1",
    by_value = "G1",
    stringsAsFactors = FALSE
  )

  out <- .join_by_group(df, by_vals)
  expect_equal(nrow(out), 2)
  expect_equal(out$.by_group, c("G1", NA))
})

test_that(".join_by_group works with chunk-aware lookup from split texts", {
  # After splitting, the by_column_lookup is remapped so that each chunk

  # text is associated with the groups of its original source text.
  df <- data.frame(
    text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
    result = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  # Chunk-aware lookup: each chunk maps to the group(s) of its source text.
  by_vals <- data.frame(
    text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
    by_value = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  out <- .join_by_group(df, by_vals)
  expect_equal(nrow(out), 3)
  expect_equal(out$.by_group, c("G1", "G1", "G2"))
  # No NAs — every chunk is matched
  expect_false(anyNA(out$.by_group))
})

test_that("generate_grouped_freq_table_single works with dedup data frame", {
  df <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 1", "Text 2"),
    by_value = c("G1", "G2", "G1"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_freq_table_single(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    categories = c("A", "B"),
    language = "en"
  )
  # Should succeed and return a DT::datatable
  expect_s3_class(tbl, "datatables")
})

test_that("generate_grouped_freq_table_single does not overcount same-text rows in one group", {
  df <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 1"),
    result = c("A", "A"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 1"),
    by_value = c("G1", "G1"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_freq_table_single(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    categories = c("A"),
    language = "en"
  )

  expect_s3_class(tbl, "datatables")
  expect_equal(tbl$x$data$Group, "G1")
  expect_equal(tbl$x$data$Category, "A")
  expect_equal(tbl$x$data$Number, 2L)
  expect_equal(tbl$x$data$Percentage, 100)
})

test_that("generate_grouped_freq_table_multi works with dedup data frame", {
  df <- data.frame(
    text = c("Text 1", "Text 2"),
    CatA = c(TRUE, FALSE),
    CatB = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 2"),
    by_value = c("G1", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_freq_table_multi(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    categories = c("CatA", "CatB"),
    language = "en"
  )
  expect_s3_class(tbl, "datatables")
})

test_that("generate_grouped_score_table works with dedup data frame", {
  df <- data.frame(
    text = c("Text 1", "Text 2", "Text 3"),
    result = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 2", "Text 3"),
    by_value = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_score_table(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    language = "en"
  )
  expect_s3_class(tbl, "datatables")
})

test_that("generate_grouped_topic_prevalence_table_single shows overall and group prevalence", {
  df <- data.frame(
    text = c("Text 1", "Text 2", "Text 3"),
    result = c("Topic A", "Topic A", "Topic B"),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 2", "Text 3"),
    by_value = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_topic_prevalence_table_single(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    topics = c("Topic A", "Topic B"),
    language = "en"
  )

  expect_s3_class(tbl, "datatables")
  expect_equal(tbl$x$data$Topic, c("Topic A", "Topic B"))
  expect_equal(tbl$x$data$Overall, c(66.67, 33.33))
  expect_equal(tbl$x$data$G1, c(100, 0))
  expect_equal(tbl$x$data$G2, c(0, 100))
})

test_that("generate_grouped_topic_prevalence_table_multi shows overall and group prevalence", {
  df <- data.frame(
    text = c("Text 1", "Text 2", "Text 3", "Text 4"),
    `Topic A` = c(TRUE, FALSE, TRUE, TRUE),
    `Topic B` = c(FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  by_vals <- data.frame(
    text = c("Text 1", "Text 2", "Text 3", "Text 4"),
    by_value = c("G1", "G1", "G2", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_topic_prevalence_table_multi(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    topics = c("Topic A", "Topic B"),
    language = "en"
  )

  expect_s3_class(tbl, "datatables")
  expect_equal(tbl$x$data$Topic, c("Topic A", "Topic B"))
  expect_equal(tbl$x$data$Overall, c(75, 50))
  expect_equal(tbl$x$data$G1, c(50, 50))
  expect_equal(tbl$x$data$G2, c(100, 50))
})

test_that("generate_grouped_marking_prevalence_table shows document-level code prevalence", {
  df <- data.frame(
    document_id = c(1L, 1L, 2L, 2L, 2L),
    text = c("Text 1", "Text 1", "Text 2", "Text 2", "Text 2"),
    chunk_text = c("Chunk 1A", "Chunk 1B", "Chunk 2A", "Chunk 2A", "Chunk 2B"),
    code = c("Code 1", "Code 1", "Code 1", "Code 2", "Code 2"),
    marked_text = c("alpha", NA, NA, "beta", NA),
    response_status = c(
      "matched_all",
      "completed",
      "completed",
      "matched_all",
      "completed"
    ),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 2"),
    by_value = c("G1", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_marking_prevalence_table(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    codes = c("Code 1", "Code 2"),
    language = "en"
  )

  expect_s3_class(tbl, "datatables")
  expect_equal(tbl$x$data$Code, c("Code 1", "Code 2"))
  expect_equal(tbl$x$data$Overall, c(50, 50))
  expect_equal(tbl$x$data$G1, c(100, 0))
  expect_equal(tbl$x$data$G2, c(0, 100))
})

test_that("generate_grouped_marking_frequency_table counts texts and spans by group", {
  df <- data.frame(
    document_id = c(1L, 1L, 1L, 2L, 2L, 2L),
    text = c("Text 1", "Text 1", "Text 1", "Text 2", "Text 2", "Text 2"),
    chunk_text = c(
      "Chunk 1A",
      "Chunk 1A",
      "Chunk 1B",
      "Chunk 2A",
      "Chunk 2A",
      "Chunk 2B"
    ),
    code = c("Code 1", "Code 1", "Code 2", "Code 1", "Code 2", "Code 2"),
    marked_text = c("alpha", "gamma", NA, NA, "beta", "delta"),
    response_status = c(
      "matched_all",
      "matched_all",
      "completed",
      "completed",
      "matched_all",
      "matched_all"
    ),
    stringsAsFactors = FALSE
  )
  by_vals <- data.frame(
    document_id = c(1L, 2L),
    text = c("Text 1", "Text 2"),
    by_value = c("G1", "G2"),
    stringsAsFactors = FALSE
  )

  tbl <- generate_grouped_marking_frequency_table(
    df = df,
    by_values = by_vals,
    by_column_name = "group",
    codes = c("Code 1", "Code 2"),
    language = "en"
  )

  expect_s3_class(tbl, "datatables")
  expect_equal(tbl$x$data$Group, c("G1", "G1", "G2", "G2"))
  expect_equal(tbl$x$data$Code, c("Code 1", "Code 2", "Code 1", "Code 2"))
  expect_equal(tbl$x$data$Number, c(1, 0, 0, 1))
  expect_equal(tbl$x$data$Percentage, c(100, 0, 0, 100))
  expect_equal(tbl$x$data$`Marked spans`, c(2, 0, 0, 2))
})


# -- Render smoke test: dedup scenario (was the original bug) ------------------

test_that("Categorisatie report renders with deduped by_column_values (no error text)", {
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

  # Scenario: original upload had 3 source rows with two rows sharing the same
  # text content but different groups. The runtime now preserves all rows, and
  # grouped reports must still render without grouped-frequency shape errors.
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
            analysis_result = .build_grouped_categorization_result(
              report_path = report_path,
              results_table = data.frame(
                text = c("Text 1", "Text 1", "Text 2"),
                result = c("A", "A", "B"),
                stringsAsFactors = FALSE
              ),
              texts_df = .make_grouped_texts_df(
                document_text = c("Text 1", "Text 1", "Text 2")
              ),
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L, 3L),
                by_value = c("G1", "G2", "G1"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with dedup by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)

      # The rendered HTML must not contain error text from error=TRUE chunks
      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      expect_false(
        grepl("must be size", html_text, fixed = TRUE),
        info = paste0(
          "Report ",
          basename(report_path),
          " contains error text from grouped frequency chunk"
        )
      )
    }
  })
})


test_that("Scoren report renders with deduped by_column_values (no error text)", {
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

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Scoren_.*\\.Rmd$",
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
            analysis_result = .build_grouped_scoring_result(
              report_path = report_path,
              results_table = data.frame(
                text = c("Text 1", "Text 1", "Text 2"),
                result = c(10, 10, 20),
                stringsAsFactors = FALSE
              ),
              texts_df = .make_grouped_texts_df(
                document_text = c("Text 1", "Text 1", "Text 2")
              ),
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L, 3L),
                by_value = c("G1", "G2", "G1"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with dedup by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      expect_false(
        grepl("must be size", html_text, fixed = TRUE),
        info = paste0(
          "Report ",
          basename(report_path),
          " contains error text from grouped score chunk"
        )
      )
    }
  })
})


test_that("Scoren report renders correctly with split-chunk by_column_lookup", {
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

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Scoren_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      out_file <- file.path(
        out_dir,
        paste0(tools::file_path_sans_ext(basename(report_path)), "_split.html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_grouped_scoring_result(
              report_path = report_path,
              results_table = data.frame(
                text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
                result = c(10, 12, 20),
                stringsAsFactors = FALSE
              ),
              texts_df = .make_grouped_texts_df(
                document_text = c(
                  "Text 1 chunk A",
                  "Text 1 chunk B",
                  "Text 2 chunk A"
                ),
                source_document_id = c(1L, 1L, 2L),
                source_document_text = c("Text 1", "Text 1", "Text 2")
              ),
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L),
                by_value = c("G1", "G2"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with split-chunk by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)

      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      expect_false(
        grepl("must be size", html_text, fixed = TRUE),
        info = paste0(
          "Report ",
          basename(report_path),
          " contains error text from grouped score chunk"
        )
      )
    }
  })
})


# -- Integration: split texts + by-column grouped report -----------------------

test_that("Categorisatie report renders correctly with split-chunk by_column_lookup", {
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

  # Scenario: original upload had 2 texts ("Text 1" in G1, "Text 2" in G2).
  # After splitting, "Text 1" became two chunks. The chunk-aware lookup maps
  # each chunk back to its source text's group.
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
        paste0(tools::file_path_sans_ext(basename(report_path)), "_split.html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_grouped_categorization_result(
              report_path = report_path,
              results_table = data.frame(
                text = c("Text 1 chunk A", "Text 1 chunk B", "Text 2 chunk A"),
                result = c("A", "B", "A"),
                stringsAsFactors = FALSE
              ),
              texts_df = .make_grouped_texts_df(
                document_text = c(
                  "Text 1 chunk A",
                  "Text 1 chunk B",
                  "Text 2 chunk A"
                ),
                source_document_id = c(1L, 1L, 2L),
                source_document_text = c("Text 1", "Text 1", "Text 2")
              ),
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L),
                by_value = c("G1", "G2"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with split-chunk by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)

      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      # No grouped-frequency error text
      expect_false(
        grepl("must be size", html_text, fixed = TRUE),
        info = paste0(
          "Report ",
          basename(report_path),
          " contains error text from grouped frequency chunk (split scenario)"
        )
      )
      # The grouped table should show two distinct groups
      expect_true(
        grepl("G1", html_text, fixed = TRUE) &&
          grepl("G2", html_text, fixed = TRUE),
        info = paste0(
          "Report ",
          basename(report_path),
          " missing expected groups G1/G2 in split scenario"
        )
      )
    }
  })
})

test_that("Onderwerpextractie report renders grouped topic prevalence", {
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

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Onderwerpextractie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      out_file <- file.path(
        out_dir,
        paste0(
          tools::file_path_sans_ext(basename(report_path)),
          "_grouped.html"
        )
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_grouped_topic_result(
              report_path = report_path,
              results_table = data.frame(
                text = c("Text 1", "Text 2", "Text 3"),
                result = c("Topic A", "Topic A", "Topic B"),
                stringsAsFactors = FALSE
              ),
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L, 3L),
                by_value = c("G1", "G1", "G2"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with grouped topic prevalence failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))

      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      expect_false(grepl("must be size", html_text, fixed = TRUE))
      expect_true(grepl("G1", html_text, fixed = TRUE))
      expect_true(grepl("G2", html_text, fixed = TRUE))

      if (grepl("_en\\.Rmd$", basename(report_path))) {
        expect_true(grepl("Topic prevalence by group", html_text, fixed = TRUE))
        expect_true(grepl("Frequency per group", html_text, fixed = TRUE))
        expect_true(grepl("Topic prevalence per ", html_text, fixed = TRUE))
        expect_true(grepl("Frequencies per ", html_text, fixed = TRUE))
      } else {
        expect_true(grepl(
          "Onderwerpprevalentie per groep",
          html_text,
          fixed = TRUE
        ))
        expect_true(grepl("Frequentie per groep", html_text, fixed = TRUE))
        expect_true(grepl(
          "Onderwerpprevalentie per ",
          html_text,
          fixed = TRUE
        ))
        expect_true(grepl(
          "Frequenties per ",
          html_text,
          fixed = TRUE
        ))
      }
    }
  })
})

test_that("Markeren report renders grouped marking summaries", {
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

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Markeren_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      out_file <- file.path(
        out_dir,
        paste0(
          tools::file_path_sans_ext(basename(report_path)),
          "_grouped.html"
        )
      )

      results_table <- data.frame(
        analysis_unit_id = c(1L, 1L, 2L, 2L),
        chunk_id = c(1L, 1L, 2L, 2L),
        chunk_index = c(1L, 1L, 1L, 1L),
        text = c("Text 1", "Text 1", "Text 2", "Text 2"),
        chunk_text = c(
          "Text 1 chunk",
          "Text 1 chunk",
          "Text 2 chunk",
          "Text 2 chunk"
        ),
        code = c("Code 1", "Code 2", "Code 1", "Code 2"),
        marked_text = c("alpha", NA, NA, "beta"),
        response_status = c(
          "matched_all",
          "completed",
          "completed",
          "matched_all"
        ),
        stringsAsFactors = FALSE
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_grouped_marking_result(
              report_path = report_path,
              results_table = results_table,
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L),
                by_value = c("G1", "G2"),
                stringsAsFactors = FALSE
              ),
              texts_df = .make_grouped_texts_df(
                document_text = c("Text 1", "Text 2")
              )
            )
          ),
          quiet = TRUE,
          envir = .grouped_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with grouped marking summaries failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      html_content <- readLines(out_file, warn = FALSE)
      html_text <- paste(html_content, collapse = "\n")
      expect_false(grepl("must be size", html_text, fixed = TRUE))
      expect_true(grepl("G1", html_text, fixed = TRUE))
      expect_true(grepl("G2", html_text, fixed = TRUE))

      if (grepl("_en\\.Rmd$", basename(report_path))) {
        expect_true(grepl("Code prevalence by group", html_text, fixed = TRUE))
        expect_true(grepl(
          "Texts with marked spans per group",
          html_text,
          fixed = TRUE
        ))
        expect_true(grepl("Code prevalence per ", html_text, fixed = TRUE))
        expect_true(grepl(
          "Texts with marked spans per ",
          html_text,
          fixed = TRUE
        ))
      } else {
        expect_true(grepl("Codeprevalentie per groep", html_text, fixed = TRUE))
        expect_true(grepl(
          "Teksten met gemarkeerde fragmenten per groep",
          html_text,
          fixed = TRUE
        ))
        expect_true(grepl("Codeprevalentie per ", html_text, fixed = TRUE))
        expect_true(grepl(
          "Gemarkeerde teksten per ",
          html_text,
          fixed = TRUE
        ))
      }
    }
  })
})
