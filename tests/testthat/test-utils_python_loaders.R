library(testthat)

async_message_printer <- function(...) {
  function(...) invisible(NULL)
}

initialize_python_environment <- function(...) invisible(NULL)

source(here::here("R", "utils_semchunk.R"), local = TRUE)
source(here::here("R", "utils_gliner.R"), local = TRUE)


test_that("semchunk_load_chunker initializes Python and constructs a chunker", {
  calls <- new.env(parent = emptyenv())
  calls$import_module <- character()
  calls$tokenizer <- NULL
  calls$chunk_size <- NULL

  local_mocked_bindings(
    import = function(module) {
      calls$import_module <- c(calls$import_module, module)

      list(
        chunkerify = function(tokenizer, chunk_size) {
          calls$tokenizer <- tokenizer
          calls$chunk_size <- chunk_size

          function(texts, ...) {
            force(texts)
            list(texts)
          }
        }
      )
    },
    .package = "reticulate"
  )

  chunker <- semchunk_load_chunker(chunk_size = 16)

  expect_identical(calls$import_module, "semchunk")
  expect_identical(calls$tokenizer, "gpt-4")
  expect_identical(calls$chunk_size, 16L)
  expect_true(is.function(chunker))
})


test_that("gliner_load_model initializes Python and loads the configured model", {
  calls <- new.env(parent = emptyenv())
  calls$import_module <- character()
  calls$model_name <- NULL
  calls$cache_dir <- NULL

  local_mocked_bindings(
    import = function(module) {
      calls$import_module <- c(calls$import_module, module)

      list(
        GLiNER = list(
          from_pretrained = function(model_name, cache_dir) {
            calls$model_name <- model_name
            calls$cache_dir <- cache_dir

            list(predict_entities = function(...) list())
          }
        )
      )
    },
    .package = "reticulate"
  )

  model <- gliner_load_model(model_name = "urchade/gliner_multi_pii-v1")

  expect_identical(calls$import_module, "gliner")
  expect_identical(calls$model_name, "urchade/gliner_multi_pii-v1")
  expect_identical(calls$cache_dir, "./.venv")
  expect_true(is.list(model))
})
