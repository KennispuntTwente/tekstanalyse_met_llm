# Test that the kwallm.test_async option correctly enables mirai daemons
# in the shinytest2 subprocess.
#
# Background: shinytest2 spawns the Shiny app in a subprocess using callr.
# Environment variables set with Sys.setenv() in the test runner process are
# NOT inherited by callr subprocesses. However, R options passed via
# AppDriver$new(options = list(...)) ARE correctly set in the subprocess.
#
# This test verifies that:
# 1. The kwallm.test_async option is passed to the subprocess
# 2. The app reads this option and enables mirai daemons for async processing
# 3. E2E tests actually exercise the async code paths (mirai workers)

library(shinytest2)


test_that("all app entrypoints default to mori and mirai", {
  app_entrypoints <- c("app.R", "package-app.R", "Dockerfile-app.R")

  for (path in app_entrypoints) {
    source_text <- paste(
      readLines(here::here(path), warn = FALSE),
      collapse = "\n"
    )
    expect_true(
      grepl("mori__enabled = TRUE", source_text, fixed = TRUE),
      info = sprintf("Expected mori to default to enabled in %s", path)
    )
    expect_true(
      grepl("kwallm_ensure_mirai_daemons()", source_text, fixed = TRUE),
      info = sprintf("Expected mirai daemon startup in %s", path)
    )
  }
})


test_that("all E2E tests use the production-async app driver", {
  e2e_files <- list.files(
    here::here("tests", "testthat"),
    pattern = "^test-e2e-.*[.]R$",
    full.names = TRUE
  )
  expect_gt(length(e2e_files), 0L)

  for (path in e2e_files) {
    source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_true(
      grepl("kwallm_app_driver(", source_text, fixed = TRUE),
      info = sprintf(
        "Expected %s to inherit enforced mori/mirai defaults",
        basename(path)
      )
    )
    expect_false(
      grepl("AppDriver$new(", source_text, fixed = TRUE),
      info = sprintf("Unexpected direct AppDriver construction in %s", path)
    )
  }
})


test_that("kwallm.test_async option enables mirai daemons in subprocess", {
  # Start app WITH the option - should have daemons
  app_with_async <- AppDriver$new(
    name = "async-option-test",
    height = 600,
    width = 800,
    load_timeout = 30000,
    options = list(
      kwallm.test_async = TRUE,
      kwallm.test_sync_mirai = FALSE,
      mori__enabled = TRUE
    )
  )

  kwallm_expect_async_runtime(app_with_async)

  # The app logs mirai daemon status on startup - check the logs
  logs <- app_with_async$get_logs()
  shiny_logs <- logs[logs$location == "shiny", ]

  # Look for the log message about async workers
  log_messages <- paste(shiny_logs$message, collapse = "\n")

  # Should see "Using X async workers (mirai daemons)" in the logs
  expect_true(
    grepl("Using [0-9]+ async workers \\(mirai daemons", log_messages),
    info = paste(
      "Expected async workers log message. Got logs:\n",
      log_messages
    )
  )

  # Should NOT see "Using no async workers"
  expect_false(
    grepl("Using no async workers", log_messages, fixed = TRUE),
    info = "Should not see 'Using no async workers' when kwallm.test_async = TRUE"
  )

  app_with_async$stop()
})
