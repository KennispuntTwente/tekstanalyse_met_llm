# Reusable icon toggle button component
# Used for mode selection via clickable icon buttons (e.g., LLM provider, anonymization mode)

#' Create an icon toggle button
#'
#' @param ns Namespace function from the module
#' @param id_suffix Suffix for the input ID (will be prefixed with "select_")
#' @param icon_name Bootstrap icon name (from bsicons), OR NULL if using custom image
#' @param img_src Path to custom image (use instead of icon_name)
#' @param title Title attribute for the button
#' @param tooltip_text Tooltip text to display on hover
#' @param is_active Whether this button is currently active/selected
#' @param css_prefix CSS class prefix: "llm-icon" or "tm-icon"
#' @param icon_height Height of the icon (default "20px")
#'
#' @return A div element representing the icon button with tooltip
icon_toggle_button <- function(
  ns,
  id_suffix,
  icon_name = NULL,
  img_src = NULL,
  title,
  tooltip_text,
  is_active = FALSE,
  css_prefix = "llm-icon",
  icon_height = "20px"
) {
  stopifnot(
    !is.null(icon_name) || !is.null(img_src),
    is.function(ns)
  )

  full_id <- ns(paste0("select_", id_suffix))

  # Build the icon/image content
  icon_content <- if (!is.null(img_src)) {
    tags$img(
      src = img_src,
      height = gsub("px", "", icon_height, fixed = TRUE),
      alt = title
    )
  } else {
    bsicons::bs_icon(
      icon_name,
      class = paste0(css_prefix, "-img"),
      style = paste0("height:", icon_height, ";")
    )
  }

  tags$button(
    id = full_id,
    type = "button",
    class = paste(css_prefix, if (is_active) paste0(css_prefix, "-active")),
    title = title,
    `aria-label` = title,
    `aria-pressed` = tolower(is_active),
    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", full_id),
    icon_content
  ) |>
    bslib::tooltip(tooltip_text, placement = "bottom")
}


#' Create a group of icon toggle buttons
#'
#' @param ns Namespace function from the module
#' @param buttons List of button configs, each with: id, icon (or img_src), title, tooltip
#' @param active_id The id_suffix of the currently active button
#' @param css_prefix CSS class prefix: "llm-icon" or "tm-icon"
#' @param container_class CSS class for the container div
#'
#' @return A div containing all the icon buttons
icon_toggle_group <- function(
  ns,
  buttons,
  active_id = NULL,
  css_prefix = "llm-icon",
  container_class = "d-flex justify-content-center gap-3"
) {
  button_elements <- lapply(buttons, function(btn) {
    icon_toggle_button(
      ns = ns,
      id_suffix = btn$id,
      icon_name = btn$icon,
      img_src = btn$img_src,
      title = btn$title,
      tooltip_text = btn$tooltip,
      is_active = identical(btn$id, active_id),
      css_prefix = css_prefix
    )
  })

  div(class = container_class, tagList(button_elements))
}


#' CSS styles for icon toggle buttons
#'
#' Include this in your module's UI or in css_js_head.R
#'
#' @param prefix CSS class prefix: "llm-icon" or "tm-icon"
#' @return HTML style tag
icon_toggle_css <- function(prefix = "llm-icon") {
  tags$style(HTML(sprintf(
    "
    .%s {
      padding: 2px;
      border: none;
      background: transparent;
      outline: none;
      appearance: none;
      border-radius: 2px;
      transition: all 0.2s ease;
      cursor: pointer;
    }
    
    .%s:hover:not(:disabled) {
      background-color: #f0f0f0;
      box-shadow: 0 0 5px rgba(0,0,0,0.15);
      transform: scale(1.05);
    }
    
    .%s:disabled {
      opacity: 0.4;
      cursor: not-allowed;
    }
    
    .%s-active {
      background-color: #f0f0f0;
      box-shadow: 0 0 5px rgba(0,0,0,0.15);
      transform: scale(1.05);
      cursor: default;
    }
  ",
    prefix,
    prefix,
    prefix,
    prefix
  )))
}


#' Create an icon button that triggers a modal
#'
#' A simple icon button with dynamic color that opens a modal when clicked.
#' Color changes based on whether the associated value is "active" (has content).
#'
#' @param ns Namespace function from the module
#' @param input_id Input ID for the action link
#' @param icon_name FontAwesome icon name (e.g., "filter", "palette")
#' @param tooltip_text Tooltip text to display on hover
#' @param is_active Whether the icon should be highlighted (e.g., has value)
#' @param font_size Icon font size (default "1.25rem")
#'
#' @return An actionLink element with tooltip
modal_trigger_icon <- function(
  ns,
  input_id,
  icon_name,
  tooltip_text,
  is_active = FALSE,
  font_size = "1.25rem"
) {
  style <- if (is_active) "color:#0d6efd;" else "color:#6c757d;"
  style <- paste0(
    style,
    "font-size:",
    font_size,
    "; border:none; background:transparent;"
  )

  actionLink(
    ns(input_id),
    icon(icon_name, lib = "font-awesome"),
    style = style,
    `aria-label` = tooltip_text
  ) |>
    bslib::tooltip(tooltip_text)
}
