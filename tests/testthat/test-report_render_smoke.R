library(testthat)

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

  make_result_list <- function(report_path) {
    nm <- basename(report_path)

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
      return(list(
        df = data.frame(
          text = c("A long text"),
          sub_text = c("A long text"),
          code = c("Code 1"),
          marked_text = c("long"),
          stringsAsFactors = FALSE
        ),
        codes = c("Code 1"),
        model = "test-model",
        research_background = "",
        irr = NULL
      ))
    }

    if (grepl("Scoren", nm, fixed = TRUE)) {
      return(list(
        df = data.frame(
          text = c("Text 1", "Text 2"),
          result = c(10, 20),
          stringsAsFactors = FALSE
        ),
        model = "test-model",
        scoring_characteristic = "test characteristic",
        research_background = "",
        irr = irr_t
      ))
    }

    if (grepl("Onderwerpextractie", nm, fixed = TRUE)) {
      return(list(
        df = data.frame(
          text = c("Text 1", "Text 2"),
          result = c("Topic A", "Topic B"),
          stringsAsFactors = FALSE
        ),
        model = "test-model",
        model_reductie = "test-large-model",
        assign_multiple_categories = FALSE,
        research_background = "",
        irr = irr_kappa,
        paragraphs = NULL
      ))
    }

    # Categorisatie (default)
    list(
      df = data.frame(
        text = c("Text 1", "Text 2"),
        result = c("A", "B"),
        stringsAsFactors = FALSE
      ),
      categories = c("A", "B"),
      model = "test-model",
      assign_multiple_categories = FALSE,
      research_background = "",
      irr = irr_kappa,
      paragraphs = NULL
    )
  }

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
          params = list(result_list = make_result_list(report_path)),
          quiet = TRUE,
          envir = new.env(parent = globalenv())
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

  result_list <- list(
    df = data.frame(
      text = c("Text 1", "Text 2", "Text 3"),
      result = c("A", "B", "A"),
      stringsAsFactors = FALSE
    ),
    categories = c("A", "B"),
    model = "test-model",
    assign_multiple_categories = FALSE,
    research_background = "",
    irr = irr_kappa,
    paragraphs = NULL,
    by_column_name = "group",
    by_column_values = data.frame(
      text = c("Text 1", "Text 2", "Text 3"),
      by_value = c("G1", "G1", "G2"),
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
