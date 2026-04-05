wait_for_text_upload_input <- function(app, timeout = 30000) {
  app$wait_for_js(
    paste(
      "var el = document.getElementById('text_upload-text_file');",
      "!!el && el.classList.contains('shiny-bound-input');"
    ),
    timeout = timeout
  )
}
