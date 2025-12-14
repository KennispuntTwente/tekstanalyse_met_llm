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

      raw_texts <- text_upload_server(
        id = "text_upload",
        processing = processing,
        lang = lang
      )

      list(raw_texts = raw_texts, lang = lang)
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
    }
  )
})


test_that("text_upload_server: txt single-text mode returns one combined text", {
  shiny::testServer(
    function(input, output, session) {
      lang <- make_test_lang("nl")

      raw_texts <- text_upload_server(
        id = "text_upload",
        processing = reactiveVal(FALSE),
        lang = lang
      )

      list(raw_texts = raw_texts, lang = lang)
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
      expect_true(grepl("a", raw_texts()[[1]]))
      expect_true(grepl("b", raw_texts()[[1]]))
    }
  )
})
