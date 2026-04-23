# Reusable card header with tooltip helper
# Simplifies the common pattern of card headers with info icons

#' Create a card header with an info tooltip
#'
#' @param title The title text (pre-translated)
#' @param tooltip_text The tooltip text (pre-translated)
#' @param extra Optional UI content to add on the right side of the header
#'
#' @return A card_header element
#' @export
card_header_with_tooltip <- function(title, tooltip_text, extra = NULL) {
  info_icon <- tags$span(
    role = "img",
    `aria-label` = tooltip_text,
    bsicons::bs_icon("info-circle")
  )

  header_content <- if (!is.null(extra)) {
    div(
      class = "d-flex justify-content-between align-items-center w-100",
      span(
        title,
        bslib::tooltip(
          info_icon,
          tooltip_text
        )
      ),
      extra
    )
  } else {
    tagList(
      title,
      bslib::tooltip(
        info_icon,
        tooltip_text
      )
    )
  }

  bslib::card_header(header_content)
}
