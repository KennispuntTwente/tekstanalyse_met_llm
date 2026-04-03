library(testthat)

# Source helpers needed by the grouped-frequency functions
source(here::here("R", "style_datatable_config.R"), local = TRUE)
source(here::here("R", "report_grouped_frequencies.R"), local = TRUE)

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

  # Scenario: original upload had 3 rows with "Text 1" appearing in two
  # groups (G1 and G2). After discard_empty() dedup, result df has 2 rows,
  # but the by_column_values lookup preserves both group memberships.
  result_list <- list(
    df = data.frame(
      text = c("Text 1", "Text 2"),
      result = c("A", "B"),
      stringsAsFactors = FALSE
    ),
    categories = c("A", "B"),
    model = "test-model",
    assign_multiple_categories = FALSE,
    research_background = "",
    irr = NULL,
    paragraphs = NULL,
    by_column_name = "group",
    by_column_values = data.frame(
      text = c("Text 1", "Text 1", "Text 2"),
      by_value = c("G1", "G2", "G1"),
      stringsAsFactors = FALSE
    )
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
          params = list(result_list = result_list),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
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

  result_list <- list(
    df = data.frame(
      text = c("Text 1", "Text 2"),
      result = c(10, 20),
      stringsAsFactors = FALSE
    ),
    model = "test-model",
    scoring_characteristic = "test characteristic",
    research_background = "",
    irr = NULL,
    by_column_name = "group",
    by_column_values = data.frame(
      text = c("Text 1", "Text 1", "Text 2"),
      by_value = c("G1", "G2", "G1"),
      stringsAsFactors = FALSE
    )
  )

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
          params = list(result_list = result_list),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
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
