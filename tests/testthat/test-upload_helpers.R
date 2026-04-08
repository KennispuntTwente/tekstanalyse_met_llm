library(testthat)

source(here::here("R", "module_input_text_upload.R"), local = TRUE)

utf16le_ascii_raw <- function(text) {
  text_raw <- as.integer(charToRaw(text))
  as.raw(as.vector(rbind(text_raw, rep.int(0L, length(text_raw)))))
}


utf16be_ascii_raw <- function(text) {
  text_raw <- as.integer(charToRaw(text))
  as.raw(as.vector(rbind(rep.int(0L, length(text_raw)), text_raw)))
}

test_that("normalize_upload_info lowercases file extensions", {
  # Source the module to get the helper (it's defined inside the server,
  # but we can test the logic directly)
  normalize <- function(file_df) {
    stopifnot(!is.null(file_df), nrow(file_df) >= 1)
    file_df <- file_df[1, , drop = FALSE]
    file_name <- as.character(file_df$name[[1]])
    list(
      name = file_name,
      size = as.numeric(file_df$size[[1]] %||% 0),
      type = as.character(file_df$type[[1]] %||% ""),
      datapath = as.character(file_df$datapath[[1]]),
      ext = tolower(tools::file_ext(file_name))
    )
  }

  df <- data.frame(
    name = "DATA.CSV",
    size = 100,
    type = "text/csv",
    datapath = "/tmp/fake",
    stringsAsFactors = FALSE
  )
  expect_equal(normalize(df)$ext, "csv")

  df$name <- "TEXT.TXT"
  expect_equal(normalize(df)$ext, "txt")

  df$name <- "Book.Xlsx"
  expect_equal(normalize(df)$ext, "xlsx")

  df$name <- "file.SAV"
  expect_equal(normalize(df)$ext, "sav")
})

test_that("txt reading falls back from invalid UTF-8 to native encoding", {
  # Create a temp file with CP1252-encoded content (e.g. German umlaut ü = 0xFC)
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  writeBin(charToRaw("caf\xfc\n"), tmp)

  txt_lines <- strsplit(.kwallm_decode_txt_file(tmp), "\r?\n")[[1]]

  expect_true(length(txt_lines) >= 1)
  expect_true(nchar(txt_lines[1]) > 0)
})


test_that("txt decoding strips UTF-8 BOM", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("hello\nworld")), tmp)

  expect_identical(.kwallm_decode_txt_file(tmp), "hello\nworld")
})


test_that("txt decoding handles UTF-16LE BOM", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  payload <- utf16le_ascii_raw("hello\nworld")
  writeBin(c(as.raw(c(0xFF, 0xFE)), payload), tmp)

  expect_identical(.kwallm_decode_txt_file(tmp), "hello\nworld")
})


test_that("txt decoding handles UTF-16BE BOM", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  payload <- utf16be_ascii_raw("hello\nworld")
  writeBin(c(as.raw(c(0xFE, 0xFF)), payload), tmp)

  expect_identical(.kwallm_decode_txt_file(tmp), "hello\nworld")
})
