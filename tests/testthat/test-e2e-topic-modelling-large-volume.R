library(shinytest2)

build_large_volume_topic_texts <- function(n = 3000) {
  templates <- c(
    paste(
      "The invoice had duplicate charges, the refund was slow,",
      "and parcel tracking stayed incorrect."
    ),
    paste(
      "Support replied late, the help desk answer was generic,",
      "and the app login flow was confusing."
    ),
    paste(
      "Product quality felt unreliable because a part arrived damaged,",
      "while the brand also emphasized eco packaging."
    ),
    paste(
      "Delivery was fast, yet the dashboard and tracking page became confusing",
      "when the courier changed the schedule."
    ),
    paste(
      "The packaging sounded recyclable,",
      "but the invoice and refund policy were hard to understand."
    ),
    paste(
      "Customer support solved the issue eventually,",
      "but the replacement item still had a quality defect."
    )
  )

  vapply(
    seq_len(n),
    function(i) {
      template <- templates[[(i - 1L) %% length(templates) + 1L]]
      paste0(
        "Document ",
        sprintf("%04d", i),
        ": ",
        template,
        " Context note ",
        i,
        " compared similar experiences across teams."
      )
    },
    character(1)
  )
}


test_that("{shinytest2} large-volume topic modelling launches app and uses async workers", {
  temp_txt <- tempfile(fileext = ".txt")
  texts <- build_large_volume_topic_texts(3000)
  writeLines(texts, temp_txt, useBytes = TRUE)
  on.exit(unlink(temp_txt), add = TRUE)

  log_file <- here::here("logs", paste0(format(Sys.Date(), "%Y-%m-%d"), ".log"))
  initial_log_lines <- if (file.exists(log_file)) {
    readLines(log_file, warn = FALSE)
  } else {
    character()
  }

  app <- AppDriver$new(
    name = "topic modelling - large volume",
    height = 1400,
    width = 2400,
    load_timeout = 60000,
    seed = 123,
    options = list(
      kwallm.test_async = TRUE,
      kwallm.test_fake_llm = TRUE,
      topic_modelling__number_of_batches_limit = 200
    )
  )
  on.exit(app$stop(), add = TRUE)

  wait_for_text_upload_input(app)
  app$upload_file(`text_upload-text_file` = temp_txt)
  app$wait_for_value(
    export = "text_management-texts__document_text",
    timeout = 30000
  )
  uploaded_texts <- app$get_value(
    export = "text_management-texts__document_text"
  )
  expect_identical(length(uploaded_texts), 3000L)

  app$set_inputs(
    `research_background-research_background` = "Large-volume topic-modelling e2e test."
  )
  app$set_inputs(`mode-mode` = "Topic extraction")

  app$wait_for_js(
    "!!document.getElementById('model-main_model') && !!document.getElementById('model-large_model')",
    timeout = 30000
  )
  app$set_inputs(`model-main_model` = "kwallm-fake-main-1024")
  app$set_inputs(`model-large_model` = "kwallm-fake-reducer-320")

  app$set_inputs(`assign_multiple_categories_toggle-toggle` = "Yes")
  app$set_inputs(`human_in_the_loop_toggle-toggle` = "No")
  app$set_inputs(`write_paragraphs_toggle-toggle` = "No")
  app$set_inputs(`interrater_toggle-interrater_reliability` = "No")

  app$wait_for_js(
    "var btn = document.getElementById('processing-process'); !!btn && !btn.disabled;",
    timeout = 60000
  )

  app$click("processing-process")
  app$wait_for_value(
    export = "processing-success",
    timeout = 240000
  )

  expect_true(isTRUE(app$get_value(export = "processing-success")))

  results <- app$get_value(export = "processing-results_table")
  expect_identical(nrow(results), 3000L)
  expect_identical(sort(results$text), sort(texts))
  expect_true("analysis_unit_id" %in% names(results))
  topic_columns <- names(results)[vapply(results, is.logical, logical(1))]
  expect_true(length(topic_columns) >= 1)
  expect_true(all(rowSums(results[topic_columns]) > 0))

  deadline <- Sys.time() + 15
  new_log_lines <- character()

  repeat {
    all_log_lines <- if (file.exists(log_file)) {
      readLines(log_file, warn = FALSE)
    } else {
      character()
    }

    if (length(all_log_lines) > length(initial_log_lines)) {
      new_log_lines <- all_log_lines[
        seq.int(length(initial_log_lines) + 1, length(all_log_lines))
      ]
    } else {
      new_log_lines <- character()
    }

    if (
      any(grepl("\\[async\\].*Topic generation: n_batches=", new_log_lines)) &&
        any(grepl(
          "\\[async\\].*Topic reduction complete: n_input=",
          new_log_lines
        ))
    ) {
      break
    }

    if (Sys.time() >= deadline) {
      break
    }

    Sys.sleep(0.5)
  }

  expect_true(file.exists(log_file))

  topic_generation_log <- new_log_lines[
    grepl("\\[async\\].*Topic generation: n_batches=", new_log_lines)
  ]
  topic_reduction_log <- new_log_lines[
    grepl("\\[async\\].*Topic reduction complete: n_input=", new_log_lines)
  ]

  expect_true(length(topic_generation_log) >= 1)
  expect_true(length(topic_reduction_log) >= 1)

  generation_match <- stringr::str_match(
    utils::tail(topic_generation_log, 1),
    "n_batches=(\\d+), n_candidates=(\\d+)"
  )
  reduction_match <- stringr::str_match(
    utils::tail(topic_reduction_log, 1),
    "n_input=(\\d+), n_output=(\\d+), iterations=(\\d+)"
  )

  expect_true(as.integer(generation_match[, 2]) >= 100)
  expect_true(as.integer(generation_match[, 3]) >= 200)
  expect_true(as.integer(reduction_match[, 2]) >= 200)
  expect_true(as.integer(reduction_match[, 3]) >= 3)
  expect_true(as.integer(reduction_match[, 4]) >= 2)
})
