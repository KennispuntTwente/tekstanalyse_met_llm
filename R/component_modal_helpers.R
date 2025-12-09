# Modal footer layout helpers
# Helps with consistent footer button positioning for modal dialogs

#' Create a modal footer with left/center/right button layout
#'
#' Provides consistent flexbox layout for modal footer buttons.
#'
#' @param left UI element(s) for left side (e.g., modalButton("Cancel"))
#' @param center UI element(s) for center (optional)
#' @param right UI element(s) for right side (e.g., actionButton for confirm)
#'
#' @return A div with flexbox layout for modal footer
#' @export
#'
#' @examples
#' modal_footer_buttons(
#'   left = modalButton("Cancel"),
#'   right = actionButton("confirm", "OK", class = "btn btn-danger")
#' )
modal_footer_buttons <- function(
  left = NULL,
  center = NULL,
  right = NULL
) {
  tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    tags$div(class = "d-flex align-items-center", left),
    tags$div(class = "d-flex align-items-center", center),
    tags$div(class = "d-flex align-items-center", right)
  )
}


#' Create a simple confirm/cancel modal footer
#'
#' Shorthand for common pattern: cancel button on left, confirm on right.
#'
#' @param cancel_label Label for cancel button
#' @param confirm_id Input ID for confirm button
#' @param confirm_label Label for confirm button
#' @param confirm_class CSS class for confirm button
#' @param ns Namespace function (optional)
#'
#' @return A modal footer div
#' @export
modal_footer_confirm <- function(
  cancel_label = "Cancel",
  confirm_id,
  confirm_label = "Confirm",
  confirm_class = "btn btn-danger",
  ns = identity
) {
  modal_footer_buttons(
    left = modalButton(cancel_label),
    right = actionButton(
      ns(confirm_id),
      confirm_label,
      class = confirm_class
    )
  )
}
