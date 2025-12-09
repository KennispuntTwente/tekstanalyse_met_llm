#' Create standardized DT::datatable configuration for reports
#'
#' This function provides a centralized configuration for DT::datatable
#' with enhanced page size options for better user experience.
#'
#' @param default_page_length Integer. Default number of rows per page (default: 25)
#' @param page_size_options Vector of integers. Available page size options
#'   (default: c(5, 10, 25, 50, 100, 200, "All"))
#' @param include_buttons Logical. Whether to include export buttons (default: TRUE)
#' @param scrollX Logical. Whether to enable horizontal scrolling (default: TRUE)
#' @param scrollY Character. Vertical scroll height (default: "400px")
#' @param additional_options List. Additional DT options to merge
#'
#' @return List of options for DT::datatable
#'
#' @examples
#' # Basic usage
#' df %>% DT::datatable(options = get_datatable_options())
#'
#' # Custom page size
#' df %>% DT::datatable(options = get_datatable_options(default_page_length = 50))
#'
#' # Custom page size options
#' df %>% DT::datatable(options = get_datatable_options(
#'   page_size_options = c(10, 25, 50, 100)
#' ))
get_datatable_options <- function(
  default_page_length = 25,
  page_size_options = c(5, 10, 25, 50, 100, 200, -1),
  include_buttons = TRUE,
  scrollX = TRUE,
  scrollY = "400px",
  additional_options = list()
) {
  # Convert -1 to "All" for lengthMenu display
  page_size_labels <- page_size_options
  page_size_labels[page_size_labels == -1] <- "Alle"

  # Create base options
  base_options <- list(
    pageLength = default_page_length,
    lengthMenu = list(page_size_options, page_size_labels),
    scrollX = scrollX,
    scrollY = scrollY,
    scrollCollapse = TRUE,
    language = list(
      lengthMenu = "Toon _MENU_ rijen per pagina",
      info = "Toont _START_ tot _END_ van _TOTAL_ rijen",
      infoEmpty = "Toont 0 tot 0 van 0 rijen",
      infoFiltered = "(gefilterd uit _MAX_ totaal rijen)",
      search = "Zoeken:",
      paginate = list(
        first = "Eerste",
        last = "Laatste",
        `next` = "Volgende",
        previous = "Vorige"
      ),
      emptyTable = "Geen data beschikbaar in tabel"
    )
  )

  # Add buttons if requested
  if (include_buttons) {
    base_options$dom <- 'Blfrtip'
    base_options$buttons <- list('excel', 'csv', 'pdf', 'print')
  } else {
    base_options$dom <- 'lfrtip'
  }

  # Merge with additional options
  if (length(additional_options) > 0) {
    base_options <- modifyList(base_options, additional_options)
  }

  return(base_options)
}

#' Create standardized DT::datatable configuration for English reports
#'
#' English version of get_datatable_options with English language settings
#'
#' @inheritParams get_datatable_options
#' @return List of options for DT::datatable
get_datatable_options_en <- function(
  default_page_length = 25,
  page_size_options = c(5, 10, 25, 50, 100, 200, -1),
  include_buttons = TRUE,
  scrollX = TRUE,
  scrollY = "400px",
  additional_options = list()
) {
  # Convert -1 to "All" for lengthMenu display
  page_size_labels <- page_size_options
  page_size_labels[page_size_labels == -1] <- "All"

  # Create base options
  base_options <- list(
    pageLength = default_page_length,
    lengthMenu = list(page_size_options, page_size_labels),
    scrollX = scrollX,
    scrollY = scrollY,
    scrollCollapse = TRUE,
    language = list(
      lengthMenu = "Show _MENU_ rows per page",
      info = "Showing _START_ to _END_ of _TOTAL_ rows",
      infoEmpty = "Showing 0 to 0 of 0 rows",
      infoFiltered = "(filtered from _MAX_ total rows)",
      search = "Search:",
      paginate = list(
        first = "First",
        last = "Last",
        `next` = "Next",
        previous = "Previous"
      ),
      emptyTable = "No data available in table"
    )
  )

  # Add buttons if requested
  if (include_buttons) {
    base_options$dom <- 'Blfrtip'
    base_options$buttons <- list('excel', 'csv', 'pdf', 'print')
  } else {
    base_options$dom <- 'lfrtip'
  }

  # Merge with additional options
  if (length(additional_options) > 0) {
    base_options <- modifyList(base_options, additional_options)
  }

  return(base_options)
}
