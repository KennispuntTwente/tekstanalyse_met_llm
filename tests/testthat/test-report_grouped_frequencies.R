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
