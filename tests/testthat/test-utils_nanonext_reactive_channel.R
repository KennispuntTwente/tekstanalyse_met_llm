library(testthat)
library(shiny)

source(here::here("R", "utils_nanonext_reactive_channel.R"), local = TRUE)
source(here::here("R", "component_progress_bar.R"), local = TRUE)

spin_until <- function(predicate, timeout = 2, session = NULL) {
  deadline <- Sys.time() + timeout

  repeat {
    if (isTRUE(predicate())) {
      return(TRUE)
    }

    later::run_now(0.05)
    if (!is.null(session) && is.function(session$flushReact)) {
      session$flushReact()
    }

    if (Sys.time() >= deadline) {
      break
    }
  }

  isTRUE(predicate())
}


spin_until_sessions <- function(predicate, timeout = 2, sessions = list()) {
  deadline <- Sys.time() + timeout

  repeat {
    if (isTRUE(predicate())) {
      return(TRUE)
    }

    later::run_now(0.05)
    for (session in sessions) {
      if (is.null(session) || !is.function(session$flushReact)) {
        next
      }

      try(session$flushReact(), silent = TRUE)
    }

    if (Sys.time() >= deadline) {
      break
    }
  }

  isTRUE(predicate())
}


test_that("shinyQueue relays worker reactive assignments from mirai", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("nanonext")

  kwallm_test_start_mirai_daemons(n = 1L)

  shiny::testServer(
    function(input, output, session) {
      semchunk_message <- reactiveVal("...")
      queue <- shinyQueue(session = session, throttle = 0)
      queue$consumer$start()

      list(
        queue = queue,
        semchunk_message = semchunk_message
      )
    },
    {
      worker <- mirai::mirai(
        {
          queue$producer$fireAssignReactive("semchunk_message", value)
          TRUE
        },
        queue = queue,
        value = "Splitting texts..."
      )

      expect_true(isTRUE(worker[]))
      expect_true(spin_until(
        function() {
          identical(shiny::isolate(semchunk_message()), "Splitting texts...")
        },
        session = session
      ))
      expect_identical(
        shiny::isolate(semchunk_message()),
        "Splitting texts..."
      )
    }
  )
})


test_that("AsyncProgressBarController survives mirai serialization", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("nanonext")

  kwallm_test_start_mirai_daemons(n = 1L)

  shiny::testServer(
    function(input, output, session) {
      progress <- reactiveVal(0)
      text <- reactiveVal("...")
      hidden <- reactiveVal(TRUE)

      queue <- shinyQueue(session = session, throttle = 0)
      queue$consumer$start()
      controller <- AsyncProgressBarController$new(queue)

      list(
        progress = progress,
        text = text,
        hidden = hidden,
        controller = controller
      )
    },
    {
      worker <- mirai::mirai(
        {
          ctrl$show()
          ctrl$set_with_total(2, 4, "Document 2 example payload")
          TRUE
        },
        ctrl = controller
      )

      expect_true(isTRUE(worker[]))
      expect_true(spin_until(
        function() {
          identical(shiny::isolate(progress()), 50) &&
            grepl("2/4", shiny::isolate(text()), fixed = TRUE) &&
            grepl(
              "Document 2 example payload",
              shiny::isolate(text()),
              fixed = TRUE
            ) &&
            identical(shiny::isolate(hidden()), FALSE)
        },
        session = session
      ))

      expect_identical(shiny::isolate(progress()), 50)
      expect_false(shiny::isolate(hidden()))

      worker_hide <- mirai::mirai(
        {
          ctrl$hide()
          TRUE
        },
        ctrl = controller
      )

      expect_true(isTRUE(worker_hide[]))
      expect_true(spin_until(
        function() identical(shiny::isolate(hidden()), TRUE),
        session = session
      ))
      expect_true(shiny::isolate(hidden()))
    }
  )
})


test_that("AsyncProgressBarController keeps progress scoped per Shiny session", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("nanonext")

  MockShinySession <- getFromNamespace("MockShinySession", "shiny")
  session_a <- MockShinySession$new()
  session_b <- MockShinySession$new()

  withr::defer({
    if (!isTRUE(session_a$isEnded())) {
      session_a$close()
    }
  })
  withr::defer({
    if (!isTRUE(session_b$isEnded())) {
      session_b$close()
    }
  })

  expect_false(identical(session_a$token, session_b$token))

  env_a <- new.env(parent = environment())
  env_b <- new.env(parent = environment())

  env_a$session <- session_a
  env_a$progress <- reactiveVal(0)
  env_a$text <- reactiveVal("session-a")
  env_a$hidden <- reactiveVal(TRUE)

  env_b$session <- session_b
  env_b$progress <- reactiveVal(0)
  env_b$text <- reactiveVal("session-b")
  env_b$hidden <- reactiveVal(TRUE)

  evalq(
    {
      queue <- shinyQueue(session = session, throttle = 0)
      queue$consumer$start()
      controller <- AsyncProgressBarController$new(queue)
    },
    envir = env_a
  )

  evalq(
    {
      queue <- shinyQueue(session = session, throttle = 0)
      queue$consumer$start()
      controller <- AsyncProgressBarController$new(queue)
    },
    envir = env_b
  )

  kwallm_test_start_mirai_daemons(n = 2L)

  worker_a <- mirai::mirai(
    {
      ctrl$show()
      ctrl$set_with_total(1, 4, "Alpha document")
      TRUE
    },
    ctrl = env_a$controller
  )

  worker_b <- mirai::mirai(
    {
      ctrl$show()
      ctrl$set_with_total(3, 6, "Beta document")
      TRUE
    },
    ctrl = env_b$controller
  )

  expect_true(isTRUE(worker_a[]))
  expect_true(isTRUE(worker_b[]))

  expect_true(spin_until_sessions(
    function() {
      identical(shiny::isolate(env_a$progress()), 25) &&
        grepl("1/4", shiny::isolate(env_a$text()), fixed = TRUE) &&
        grepl("Alpha document", shiny::isolate(env_a$text()), fixed = TRUE) &&
        identical(shiny::isolate(env_a$hidden()), FALSE) &&
        identical(shiny::isolate(env_b$progress()), 50) &&
        grepl("3/6", shiny::isolate(env_b$text()), fixed = TRUE) &&
        grepl("Beta document", shiny::isolate(env_b$text()), fixed = TRUE) &&
        identical(shiny::isolate(env_b$hidden()), FALSE)
    },
    sessions = list(session_a, session_b)
  ))

  expect_identical(shiny::isolate(env_a$progress()), 25)
  expect_identical(shiny::isolate(env_b$progress()), 50)
  expect_match(shiny::isolate(env_a$text()), "Alpha document", fixed = TRUE)
  expect_match(shiny::isolate(env_b$text()), "Beta document", fixed = TRUE)

  # Each session only receives its own worker updates.
  expect_false(grepl(
    "Beta document",
    shiny::isolate(env_a$text()),
    fixed = TRUE
  ))
  expect_false(grepl(
    "Alpha document",
    shiny::isolate(env_b$text()),
    fixed = TRUE
  ))
})


test_that("AsyncInterruptor interrupts a mirai worker loop", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("nanonext")

  kwallm_test_start_mirai_daemons(n = 1L)

  interrupter <- AsyncInterruptor$new()
  withr::defer(interrupter$destroy())
  ready_path <- tempfile("kwallm-interrupt-ready-")
  withr::defer(unlink(ready_path), testthat::teardown_env())

  worker <- mirai::mirai(
    {
      writeLines("ready", ready_path)
      repeat {
        interrupter$execInterrupts()
        Sys.sleep(0.02)
      }
    },
    interrupter = interrupter,
    ready_path = ready_path
  )

  expect_true(spin_until(
    function() file.exists(ready_path),
    timeout = 5
  ))
  interrupter$interrupt("cancelled by test")

  err <- tryCatch(worker[], error = function(e) e)
  expect_s3_class(err, "miraiError")
  expect_match(conditionMessage(err), "cancelled by test", fixed = TRUE)
})


test_that("AsyncProgress keeps cumulative progress under rapid worker updates", {
  testthat::skip_if_not_installed("mirai")
  testthat::skip_if_not_installed("nanonext")

  shiny_ns <- asNamespace("shiny")
  old_progress <- get("Progress", envir = shiny_ns)
  progress_state <- new.env(parent = emptyenv())
  progress_state$entries <- list()
  progress_state$closed <- FALSE

  fake_progress <- R6::R6Class(
    "FakeProgress",
    public = list(
      initialize = function(session = NULL) {
        invisible(session)
      },
      set = function(value = NULL, message = NULL, detail = NULL) {
        progress_state$entries <- c(
          progress_state$entries,
          list(list(
            value = value,
            message = message,
            detail = detail
          ))
        )
        invisible(NULL)
      },
      close = function() {
        progress_state$closed <- TRUE
        invisible(NULL)
      }
    )
  )

  unlockBinding("Progress", shiny_ns)
  assign("Progress", fake_progress, envir = shiny_ns)
  lockBinding("Progress", shiny_ns)

  withr::defer({
    unlockBinding("Progress", shiny_ns)
    assign("Progress", old_progress, envir = shiny_ns)
    lockBinding("Progress", shiny_ns)
  })

  kwallm_test_start_mirai_daemons(n = 1L)

  shiny::testServer(
    function(input, output, session) {
      async_progress <- AsyncProgress$new(
        message = "Detectie",
        detail = "0/4",
        session = session,
        throttle = 0
      )

      list(async_progress = async_progress)
    },
    {
      worker <- mirai::mirai(
        {
          progress$inc(0.25, detail = "1/4", message = "Loading")
          progress$inc(0.25, detail = "2/4", message = "Loading")
          TRUE
        },
        progress = async_progress
      )

      expect_true(isTRUE(worker[]))
      expect_true(spin_until(
        function() {
          if (!length(progress_state$entries)) {
            return(FALSE)
          }

          latest <- tail(progress_state$entries, 1)[[1]]
          identical(latest$message, "Loading") &&
            identical(latest$detail, "2/4") &&
            isTRUE(all.equal(latest$value, 0.5))
        },
        session = session
      ))

      latest <- tail(progress_state$entries, 1)[[1]]
      expect_identical(latest$message, "Loading")
      expect_identical(latest$detail, "2/4")
      expect_equal(latest$value, 0.5)

      async_progress$close()
      expect_true(progress_state$closed)
    }
  )
})
