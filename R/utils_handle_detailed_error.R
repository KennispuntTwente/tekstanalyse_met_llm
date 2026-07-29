# Utility: format detailed errors for tryCatch/future handlers
# Returns a closure suitable for use in `error = ...` handlers.

kwallm_error_message <- function(error) {
  message <- if (inherits(error, "condition")) {
    tryCatch(conditionMessage(error), error = function(e) NULL)
  } else if (is.character(error)) {
    error
  } else {
    tryCatch(conditionMessage(error), error = function(e) NULL)
  }

  if (is.null(message) || !length(message)) {
    message <- tryCatch(as.character(error), error = function(e) NULL)
  }
  if (is.null(message) || !length(message)) {
    message <- tryCatch(
      capture.output(print(error)),
      error = function(e) "Unknown error"
    )
  }

  paste(as.character(message), collapse = "\n")
}


handle_detailed_error <- function(context = "An operation") {
  force(context)
  function(e) {
    error_message <- paste0(
      context,
      " failed:\n",
      "Message: ",
      kwallm_error_message(e)
    )
    stop(error_message)
  }
}
