library(testthat)

test_that("manual paragraph batch streaming demo builds a Shiny app", {
  demo_env <- new.env(parent = globalenv())
  sys.source(
    here::here("tests", "manual", "paragraph_batch_streaming_app.R"),
    envir = demo_env
  )

  app <- demo_env$paragraph_batch_streaming_demo_app()

  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("manual streaming demo scheduled callbacks run outside reactive context", {
  demo_env <- new.env(parent = globalenv())
  sys.source(
    here::here("tests", "manual", "paragraph_batch_streaming_app.R"),
    envir = demo_env
  )
  server <- demo_env$paragraph_batch_streaming_demo_app()$serverFuncSource()

  shiny::testServer(server, {
    session$setInputs(speed = 0.001, start = 1)
    session$flushReact()
    expect_no_error(later::run_now(timeoutSecs = 0.3))
  })
})
