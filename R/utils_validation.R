# Small validation helpers shared across modules

is_valid_number <- function(x) {
  if (is.null(x) || length(x) != 1) {
    return(FALSE)
  }

  if (is.character(x)) {
    if (!nzchar(x)) {
      return(FALSE)
    }
    x <- suppressWarnings(as.numeric(x))
  }

  is.numeric(x) && !is.na(x) && is.finite(x)
}
