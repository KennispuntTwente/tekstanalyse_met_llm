# Helper utilities for focused Shiny module/component tests

# Keep this helper lightweight and deterministic.

# In-process module tests mock mirai workers by evaluating expressions in the
# same R session, so they do not run the real worker bootstrap that maps mori
# refs. App subprocess and integration tests can still opt into mori normally.
options(mori__enabled = FALSE)

# Provide a simple %||% operator (used throughout the app code without namespacing).
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

make_test_lang <- function(lang_code = "nl") {
  tr <- shiny.i18n::Translator$new(
    translation_json_path = here::here("language", "language.json")
  )
  tr$set_translation_language(lang_code)
  shiny::reactiveVal(tr)
}
