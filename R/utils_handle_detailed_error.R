# Utility: format detailed errors for tryCatch/future handlers
# Returns a closure suitable for use in `error = ...` handlers.

handle_detailed_error <- function(context = "An operation") {
  force(context)
  function(e) {
    error_message <- paste0(
      context,
      " failed:\n",
      "Message: ",
      conditionMessage(e)
    )
    stop(error_message)
  }
}
