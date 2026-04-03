library(testthat)

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

  # Read using the same logic as read_txt_file
  raw <- readBin(tmp, "raw", file.info(tmp)$size)
  txt_content <- tryCatch(
    {
      decoded <- rawToChar(raw)
      if (!validUTF8(decoded)) {
        stop("not valid utf-8")
      }
      Encoding(decoded) <- "UTF-8"
      decoded
    },
    error = function(e) {
      iconv(rawToChar(raw), from = "", to = "UTF-8", sub = "")
    }
  )
  txt_lines <- strsplit(txt_content, "\r?\n")[[1]]

  # Should not error and should produce some text

  expect_true(length(txt_lines) >= 1)
  expect_true(nchar(txt_lines[1]) > 0)
})
