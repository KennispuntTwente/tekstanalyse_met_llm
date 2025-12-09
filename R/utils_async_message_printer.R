# Function factory to create a message printer which we can use to print messages
#   from an async process
# This is used in most functions where we load Python modules or models,
#   e.g., load_gliner_model(). By using the function we can show
#   progress in the Shiny app about the loading process
# Function factory takes a ipc::shinyQueue object & a reactive value name,
#   which will be used to send messages to the Shiny app
# Message is also printed to console

async_message_printer <- function(queue, reactive_value_name) {
  stopifnot(
    is.null(queue) || inherits(queue, "Queue"),
    is.character(reactive_value_name) && length(reactive_value_name) == 1
  )

  function(message, type = c("info", "success")) {
    type <- match.arg(type)

    # Format the message and print it
    if (type == "success") {
      cli::cli_alert_success(message)
      formatted <- paste0(cli::col_green("✔"), " ", message)
    } else {
      formatted <- paste0(cli::col_blue("ℹ"), " ", message)
      cli::cli_alert_info(message)
    }

    # Send to reactive if available
    if (!is.null(queue)) {
      try(queue$producer$fireAssignReactive(
        reactive_value_name,
        formatted
      ))
    }
  }
}
