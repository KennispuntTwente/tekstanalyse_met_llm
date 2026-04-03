library(testthat)
library(shiny)

source(here::here("R", "module_input_text_upload.R"))

make_fileinput_df <- function(path, filename, mime = "text/plain") {
  data.frame(
    name = filename,
    size = as.numeric(file.info(path)$size %||% 0),
    type = mime,
    datapath = normalizePath(path, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
}


test_that("text_upload_server: txt split-lines mode returns unique non-empty lines", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      # Extract the reactives from the returned list
      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        lang = lang
      )
    },
    {
      txt <- "a\n\nb\na\n"
      path <- withr::local_tempfile(fileext = ".txt")
      writeLines(txt, path, useBytes = TRUE)

      session$setInputs(`text_upload-txt_split_lines` = lang()$t("Ja"))
      session$flushReact()

      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(path, "texts.txt")
      )
      session$flushReact()

      expect_equal(sort(raw_texts()), sort(c("a", "b")))
      # No by_column for txt files
      expect_null(by_col_name())
      expect_null(by_col_values())
    }
  )
})


test_that("text_upload_server: txt single-text mode returns one combined text", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = reactiveVal(FALSE),
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        lang = lang
      )
    },
    {
      txt <- "a\n\n b\n"
      path <- withr::local_tempfile(fileext = ".txt")
      writeLines(txt, path, useBytes = TRUE)

      session$setInputs(`text_upload-txt_split_lines` = lang()$t("Nee"))
      session$flushReact()

      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(path, "texts.txt")
      )
      session$flushReact()

      expect_true(is.character(raw_texts()))
      expect_equal(length(raw_texts()), 1)
      expect_true(grepl("a", raw_texts()[[1]], fixed = TRUE))
      expect_true(grepl("b", raw_texts()[[1]], fixed = TRUE))
    }
  )
})


test_that("text_upload_server: csv with by_column returns correct grouping values", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        lang = lang
      )
    },
    {
      # Create a CSV file with text and group columns using vroom-compatible format
      path <- withr::local_tempfile(fileext = ".csv")
      vroom::vroom_write(
        data.frame(
          text = c("Text A", "Text B", "Text C"),
          group = c("Group1", "Group1", "Group2"),
          stringsAsFactors = FALSE
        ),
        path,
        delim = ","
      )

      # Upload the file
      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.csv",
          "text/csv"
        )
      )
      session$flushReact()

      # Select text column
      session$setInputs(`text_upload-column` = "text")
      session$flushReact()

      # Select by column
      session$setInputs(`text_upload-by_column` = "group")
      session$flushReact()

      # Verify texts
      expect_equal(sort(raw_texts()), sort(c("Text A", "Text B", "Text C")))

      # Verify by_column_name
      expect_equal(by_col_name(), "group")

      # Verify by_column_values aligned with texts
      expect_equal(by_col_values(), c("Group1", "Group1", "Group2"))
    }
  )
})


test_that("text_upload_server: by_column is NULL when not selected", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        lang = lang
      )
    },
    {
      # Create a CSV file with text and group columns
      path <- withr::local_tempfile(fileext = ".csv")
      vroom::vroom_write(
        data.frame(
          text = c("Text A", "Text B"),
          group = c("Group1", "Group2"),
          stringsAsFactors = FALSE
        ),
        path,
        delim = ","
      )

      # Upload the file
      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.csv",
          "text/csv"
        )
      )
      session$flushReact()

      # Select text column but NOT by column
      session$setInputs(`text_upload-column` = "text")
      session$flushReact()

      # Verify texts are present
      expect_equal(sort(raw_texts()), sort(c("Text A", "Text B")))

      # Verify by_column is NULL
      expect_null(by_col_name())
      expect_null(by_col_values())
    }
  )
})


test_that("text_upload_server: clearing by_column works", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        lang = lang
      )
    },
    {
      # Create a CSV file
      path <- withr::local_tempfile(fileext = ".csv")
      vroom::vroom_write(
        data.frame(
          text = c("Text A", "Text B"),
          group = c("Group1", "Group2"),
          stringsAsFactors = FALSE
        ),
        path,
        delim = ","
      )

      # Upload and set up
      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.csv",
          "text/csv"
        )
      )
      session$flushReact()
      session$setInputs(`text_upload-column` = "text")
      session$flushReact()
      session$setInputs(`text_upload-by_column` = "group")
      session$flushReact()

      # Verify by_column is set
      expect_equal(by_col_name(), "group")

      # Clear by_column by selecting empty string
      session$setInputs(`text_upload-by_column` = "")
      session$flushReact()

      # Verify by_column is cleared
      expect_null(by_col_name())
      expect_null(by_col_values())
    }
  )
})


test_that("text_upload_server: reuploading the same xlsx file refreshes sheet-backed data", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values
      )
    },
    {
      path <- withr::local_tempfile(fileext = ".xlsx")
      first_data <- data.frame(
        text = c("Alpha", "Beta"),
        group = c("G1", "G2"),
        stringsAsFactors = FALSE
      )
      second_data <- data.frame(
        text = c("Delta", "Epsilon", "Zeta"),
        group = c("H1", "H1", "H2"),
        stringsAsFactors = FALSE
      )

      writexl::write_xlsx(list(Sheet1 = first_data), path)

      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.xlsx",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      )
      session$flushReact()
      session$setInputs(`text_upload-column` = "text")
      session$flushReact()
      session$setInputs(`text_upload-by_column` = "group")
      session$flushReact()

      expect_equal(sort(raw_texts()), sort(first_data$text))
      expect_equal(by_col_name(), "group")
      expect_equal(by_col_values(), first_data$group)

      writexl::write_xlsx(list(Sheet1 = second_data), path)

      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.xlsx",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      )
      session$flushReact()

      expect_equal(sort(raw_texts()), sort(second_data$text))
      expect_equal(by_col_name(), "group")
      expect_equal(by_col_values(), second_data$group)
    }
  )
})


test_that("text_upload_server: duplicate texts get by_column_values aligned after dedup", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("en")
      processing <- reactiveVal(FALSE)

      upload_result <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      raw_texts <- upload_result$texts
      by_col_name <- upload_result$by_column_name
      by_col_values <- upload_result$by_column_values
      by_col_lookup <- upload_result$by_column_lookup

      list(
        raw_texts = raw_texts,
        by_col_name = by_col_name,
        by_col_values = by_col_values,
        by_col_lookup = by_col_lookup,
        lang = lang
      )
    },
    {
      # CSV with a duplicated text in two different groups
      path <- withr::local_tempfile(fileext = ".csv")
      vroom::vroom_write(
        data.frame(
          text = c("Text A", "Text A", "Text B"),
          group = c("Group1", "Group2", "Group2"),
          stringsAsFactors = FALSE
        ),
        path,
        delim = ","
      )

      session$setInputs(
        `text_upload-text_file` = make_fileinput_df(
          path,
          "data.csv",
          "text/csv"
        )
      )
      session$flushReact()

      session$setInputs(`text_upload-column` = "text")
      session$flushReact()

      session$setInputs(`text_upload-by_column` = "group")
      session$flushReact()

      # discard_empty() applies unique() -> 2 unique texts
      expect_equal(length(raw_texts()), 2)
      expect_equal(sort(raw_texts()), sort(c("Text A", "Text B")))

      # by_column_values must be same length as raw_texts (first occurrence kept)
      expect_equal(length(by_col_values()), length(raw_texts()))
      expect_equal(by_col_values(), c("Group1", "Group2"))

      expect_equal(
        by_col_lookup(),
        data.frame(
          text = c("Text A", "Text A", "Text B"),
          by_value = c("Group1", "Group2", "Group2"),
          stringsAsFactors = FALSE
        )
      )
    }
  )
})
