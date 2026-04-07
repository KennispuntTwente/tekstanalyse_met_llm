# 1 Load tokenizer -----------------------------------------------------
# Allows you to interrupt Python without an R crash
Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

tiktoken_load_tokenizer <- function(
  encoding = "gpt-4o",
  test_tokenizer = FALSE,
  reload_if_exists = FALSE, # if TRUE, reload existing tokenizer if already in global env
  sync_uv = FALSE,
  queue = NULL
) {
  ## ── Argument checks ─────────────────────────────────────────────
  stopifnot(
    is.character(encoding) && length(encoding) == 1,
    is.logical(test_tokenizer) && length(test_tokenizer) == 1,
    is.logical(reload_if_exists) && length(reload_if_exists) == 1,
    is.logical(sync_uv) && length(sync_uv) == 1,
    is.null(queue) || inherits(queue, "Queue")
  )

  ## ── Helper function for async messaging ─────────────────────────
  print_message <- async_message_printer(
    queue = queue,
    reactive_value_name = "tiktoken_message"
  )

  ## ── Check existence ────────────────────────────────────────────
  if (
    !reload_if_exists &&
      exists("tiktoken_tokenizer", envir = .GlobalEnv, inherits = FALSE)
  ) {
    return(invisible(get("tiktoken_tokenizer", envir = .GlobalEnv)))
  }

  ## ── Load Python & tiktoken module ───────────────────────────────
  print_message("Loading Python and tiktoken module...")

  initialize_python_environment(sync_uv = sync_uv)
  tk <- reticulate::import("tiktoken")

  ## ── Load tokenizer ───────────────────────────────────────────────
  tok <- tryCatch(
    {
      if (grepl("^gpt[-0-9]", encoding, ignore.case = TRUE)) {
        tk$encoding_for_model(encoding)
      } else {
        tk$get_encoding(encoding)
      }
    },
    error = function(e) {
      stop("Failed to create tokenizer: ", e$message, call. = FALSE)
    }
  )

  ## ── Test ────────────────────────────────────────────────────────
  if (test_tokenizer) {
    print_message("Testing tokenizer on sample text …")
    sample <- "The quick brown fox jumps over the lazy dog."
    print_message(paste0("Token count: ", length(tok$encode(sample))))
    print_message(paste(tok$encode(sample), collapse = ", "))
  }

  ## ── Stash in global env & return invisibly ──────────────────────
  assign("tiktoken_tokenizer", tok, envir = .GlobalEnv)
  print_message("tiktoken tokenizer ready!", type = "success")
  invisible(tok)
}


# 2 Count tokens with tokenizer ------------------------------------
count_tokens <- function(
  text,
  tokenizer = NULL
) {
  if (is.null(tokenizer)) {
    tokenizer <- tiktoken_load_tokenizer()
  }

  # Vectorise over character vectors
  vapply(
    text,
    function(x) length(tokenizer$encode(as.character(x))),
    integer(1)
  )
}
