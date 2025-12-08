# Helper function to create a styled description box
# Used for informational text boxes in card bodies

#' Create a styled description box
#'
#' @param content The content to display (can be text or HTML)
#' @param use_html If TRUE, wrap content in HTML() to allow HTML formatting
#' @param extra_class Additional CSS classes to add
#'
#' @return A div element with consistent styling
description_box <- function(content, use_html = FALSE, extra_class = NULL) {
  classes <- c("llm-narrow-container", extra_class)
  
  div(
    class = paste(classes, collapse = " "),
    style = "
      margin: 10px auto 15px auto;
      padding: 15px 20px;
      background-color: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 5px;
      font-size: 0.9em;
      color: #495057;
      text-align: center;
      word-break: normal;
      overflow-wrap: normal;
    ",
    if (use_html) HTML(content) else content
  )
}
