library(testthat)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

source(here::here("R", "result_model.R"), local = TRUE)
source(here::here("R", "result_builders.R"), local = TRUE)
source(here::here("R", "result_serializers.R"), local = TRUE)
source(here::here("R", "utils_processing_helpers.R"), local = TRUE)

.render_test_models <- function() {
  list(
    main = list(
      parameters = list(model = "test-model"),
      url = "https://api.example.com/v1/chat/completions"
    ),
    large = list(
      parameters = list(model = "test-large-model"),
      url = "https://api.example.com/v1/chat/completions"
    )
  )
}

.report_render_env <- function(parent = environment()) {
  list2env(analysis_result_report_globals(), parent = parent)
}

.report_language_from_path <- function(report_path) {
  if (grepl("_en\\.Rmd$", basename(report_path))) {
    return("en")
  }

  "nl"
}

.make_smoke_texts_df <- function(
  document_text,
  preprocessed = document_text,
  source_document_id = seq_along(document_text),
  source_document_text = document_text,
  analysis_unit_id = match(preprocessed, unique(preprocessed))
) {
  data.frame(
    source_document_id = as.integer(source_document_id),
    document_id = seq_along(document_text),
    source_document_text = as.character(source_document_text),
    document_text = as.character(document_text),
    preprocessed = as.character(preprocessed),
    analysis_unit_id = as.integer(analysis_unit_id),
    stringsAsFactors = FALSE
  )
}

.build_smoke_analysis_result <- function(
  report_path,
  by_column_name = NULL,
  by_column_lookup = NULL
) {
  nm <- basename(report_path)
  language <- .report_language_from_path(report_path)

  irr_kappa <- list(
    subjects = 10,
    raters = 2,
    irr.name = "Kappa",
    stat.name = "z",
    statistic = 1.0,
    p.value = 0.5,
    value = 0.4
  )

  irr_t <- list(
    subjects = 10,
    estimate = 0.0,
    statistic = 0.0,
    p.value = 1.0,
    parameter = 9,
    conf.low = -1,
    conf.high = 1,
    method = "Paired t-test",
    alternative = "two.sided",
    llm_mean = 20,
    llm_sd = 1,
    user_mean = 20,
    user_sd = 1,
    sensitivity_sentence = ""
  )

  if (grepl("Markeren", nm, fixed = TRUE)) {
    texts_df <- .make_smoke_texts_df(document_text = "A long text")
    results_table <- data.frame(
      analysis_unit_id = texts_df$analysis_unit_id,
      chunk_id = 1L,
      chunk_index = 1L,
      text = "A long text",
      chunk_text = "A long text",
      code = "Code 1",
      marked_text = "long",
      stringsAsFactors = FALSE
    )
    paragraph_entries <- list(list(
      topic = "Code 1",
      paragraph = 'Summary with "long".',
      texts = "A **long** text",
      analysis_unit_ids = 1L,
      prompt_fits = TRUE
    ))

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      paragraph_entries = paragraph_entries,
      uuid = paste0("smoke-", nm),
      mode = "Markeren",
      research_background = "",
      style_prompt = NULL,
      irr_result = NULL,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      codes = "Code 1",
      assign_multiple_categories = FALSE,
      human_in_the_loop = FALSE,
      write_paragraphs = TRUE,
      stage_prompt_previews = list(
        marking = "prompt",
        paragraph_generation = "paragraph prompt"
      )
    ))
  }

  if (grepl("Scoren", nm, fixed = TRUE)) {
    texts_df <- .make_smoke_texts_df(document_text = c("Text 1", "Text 2"))
    results_table <- data.frame(
      text = c("Text 1", "Text 2"),
      result = c(10, 20),
      stringsAsFactors = FALSE
    )

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      uuid = paste0("smoke-", nm),
      mode = "Scoren",
      research_background = "",
      style_prompt = NULL,
      irr_result = irr_t,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      scoring_characteristic = "test characteristic",
      write_paragraphs = FALSE,
      stage_prompt_previews = list(scoring = "prompt")
    ))
  }

  if (grepl("Onderwerpextractie", nm, fixed = TRUE)) {
    texts_df <- .make_smoke_texts_df(document_text = c("Text 1", "Text 2"))
    results_table <- data.frame(
      text = c("Text 1", "Text 2"),
      result = c("Topic A", "Topic B"),
      stringsAsFactors = FALSE
    )
    paragraph_entries <- list(list(
      topic = "Topic A",
      paragraph = 'Summary with "Text 1".',
      texts = "Text 1",
      analysis_unit_ids = 1L,
      prompt_fits = TRUE
    ))

    return(build_analysis_result(
      texts_df = texts_df,
      results_table = results_table,
      paragraph_entries = paragraph_entries,
      uuid = paste0("smoke-", nm),
      mode = "Onderwerpextractie",
      research_background = "",
      style_prompt = NULL,
      irr_result = irr_kappa,
      language = language,
      by_column_name = by_column_name,
      by_column_lookup = by_column_lookup,
      models = .render_test_models(),
      topics = c("Topic A", "Topic B"),
      exclusive_topics = character(),
      assign_multiple_categories = FALSE,
      human_in_the_loop = FALSE,
      write_paragraphs = TRUE,
      context_window = list(
        batch_size = 5,
        draws = 2,
        n_batches = 2,
        n_tokens_context_window = 1000
      ),
      stage_prompt_previews = list(
        topic_candidate_generation = "candidate prompt",
        topic_reduction = "reduction prompt",
        topic_assignment = "assignment prompt",
        paragraph_generation = "paragraph prompt"
      ),
      candidate_topics = c("Topic A", "Topic B"),
      reduced_topics = c("Topic A", "Topic B"),
      topics_were_edited = FALSE
    ))
  }

  texts_df <- .make_smoke_texts_df(document_text = c("Text 1", "Text 2"))
  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("A", "B"),
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(list(
    topic = "A",
    paragraph = 'Summary with "Text 1".',
    texts = "Text 1",
    analysis_unit_ids = 1L,
    prompt_fits = TRUE
  ))

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = paste0("smoke-", nm),
    mode = "Categorisatie",
    research_background = "",
    style_prompt = NULL,
    irr_result = irr_kappa,
    language = language,
    by_column_name = by_column_name,
    by_column_lookup = by_column_lookup,
    models = .render_test_models(),
    categories = c("A", "B"),
    exclusive_categories = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(
      categorization = "prompt",
      paragraph_generation = "paragraph prompt"
    )
  )
}

.build_marking_escape_analysis_result <- function(language) {
  texts_df <- .make_smoke_texts_df(document_text = "A long text")
  results_table <- data.frame(
    analysis_unit_id = 1L,
    chunk_id = 1L,
    chunk_index = 1L,
    text = "A long text",
    chunk_text = "A long text",
    code = "Code 1",
    marked_text = "long",
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(list(
    topic = "Code 1",
    paragraph = 'Summary with "alpha".',
    texts = "<kwallm-unsafe-tag>boom</kwallm-unsafe-tag> **alpha**",
    analysis_unit_ids = 1L,
    prompt_fits = TRUE
  ))

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = paste0("escape-", language),
    mode = "Markeren",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = language,
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .render_test_models(),
    codes = "Code 1",
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(
      marking = "prompt",
      paragraph_generation = "paragraph prompt"
    )
  )
}

.build_marking_overflow_analysis_result <- function(language) {
  texts_df <- .make_smoke_texts_df(document_text = "A long text")
  results_table <- data.frame(
    analysis_unit_id = 1L,
    chunk_id = 1L,
    chunk_index = 1L,
    text = "A long text",
    chunk_text = "A long text",
    code = "Code 1",
    marked_text = "long",
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(list(
    topic = "Code 1",
    paragraph = "",
    texts = "A supporting excerpt with **long** highlighted.",
    analysis_unit_ids = 1L,
    prompt_fits = FALSE
  ))

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = paste0("overflow-", language),
    mode = "Markeren",
    research_background = "",
    style_prompt = NULL,
    irr_result = NULL,
    language = language,
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .render_test_models(),
    codes = "Code 1",
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    stage_prompt_previews = list(
      marking = "prompt",
      paragraph_generation = "paragraph prompt"
    )
  )
}

.build_topic_report_regression_analysis_result <- function(language) {
  irr_kappa <- list(
    subjects = 10,
    raters = 2,
    irr.name = "Kappa",
    stat.name = "z",
    statistic = 1.0,
    p.value = 0.5,
    value = 0.4
  )

  texts_df <- .make_smoke_texts_df(document_text = c("Text 1", "Text 2"))
  results_table <- data.frame(
    text = c("Text 1", "Text 2"),
    result = c("Topic A", "Topic B"),
    stringsAsFactors = FALSE
  )
  paragraph_entries <- list(list(
    topic = "Topic A",
    paragraph = 'Summary with "Text 1".',
    texts = "Text 1",
    analysis_unit_ids = 1L,
    prompt_fits = FALSE
  ))
  reduced_topics <- c("Topic A", "Topic B")
  attr(reduced_topics, "reduction_summary") <- list(
    not_applicable_requested = TRUE,
    auto_added_not_applicable = TRUE,
    not_applicable_check_performed = TRUE,
    reduction_iterations = 2L
  )

  build_analysis_result(
    texts_df = texts_df,
    results_table = results_table,
    paragraph_entries = paragraph_entries,
    uuid = paste0("topic-regression-", language),
    mode = "Onderwerpextractie",
    research_background = "",
    style_prompt = NULL,
    irr_result = irr_kappa,
    language = language,
    by_column_name = NULL,
    by_column_lookup = NULL,
    models = .render_test_models(),
    topics = c("Topic A", "Topic B"),
    exclusive_topics = character(),
    assign_multiple_categories = FALSE,
    human_in_the_loop = FALSE,
    write_paragraphs = TRUE,
    context_window = list(
      batch_size = 25,
      draws = 2,
      n_batches = 2,
      n_tokens_context_window = 1000
    ),
    stage_prompt_previews = list(
      topic_candidate_generation = "candidate prompt",
      topic_reduction = "reduction prompt",
      topic_assignment = "assignment prompt",
      paragraph_generation = "paragraph prompt"
    ),
    candidate_topics = c("Topic A", "Topic B"),
    reduced_topics = reduced_topics,
    topics_were_edited = TRUE
  )
}

test_that("report templates render (smoke)", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")

  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  report_dir <- here::here("R")
  reports <- list.files(
    report_dir,
    pattern = "^report_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(reports) > 0)

  out_dir <- withr::local_tempdir()

  withr::with_dir(here::here(), {
    for (report_path in reports) {
      out_file <- file.path(
        out_dir,
        paste0(tools::file_path_sans_ext(basename(report_path)), ".html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_smoke_analysis_result(report_path)
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)
    }
  })
})

test_that("Topic reports render updated batching wording and provenance details", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  expected_strings <- list(
    en = c(
      "of up to 25 texts",
      "upload order",
      "Topic list provenance",
      "edited manually before topic assignment",
      "Unknown/not applicable",
      "The prompt for the summary of this topic did"
    ),
    nl = c(
      "van maximaal 25 teksten",
      "uploadvolgorde",
      "Herkomst van de onderwerpenlijst",
      "handmatig aangepast",
      "Onbekend/niet van toepassing",
      "De prompt voor de samenvatting van dit onderwerp paste"
    )
  )
  legacy_strings <- list(
    en = c(
      "randomly drawn groups of up to 5 texts",
      "The prompt for the summary of this category did"
    ),
    nl = c(
      "willekeurig getrokken groepen van maximaal 5 teksten",
      "De prompt voor de samenvatting van deze categorie paste"
    )
  )

  out_dir <- withr::local_tempdir()
  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Onderwerpextractie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      language <- .report_language_from_path(report_path)
      out_file <- file.path(
        out_dir,
        paste0(
          "topic-regression-",
          tools::file_path_sans_ext(basename(report_path)),
          ".html"
        )
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_topic_report_regression_analysis_result(
              language
            )
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
      for (expected in expected_strings[[language]]) {
        expect_match(html, expected, fixed = TRUE)
      }
      for (legacy in legacy_strings[[language]]) {
        expect_false(grepl(legacy, html, fixed = TRUE))
      }
    }
  })
})


test_that("overflow warning renders even without supporting texts", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  expected_warnings <- list(
    en = "The prompt for the summary of this topic did",
    nl = "De prompt voor de samenvatting van dit onderwerp paste"
  )

  out_dir <- withr::local_tempdir()
  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Onderwerpextractie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      language <- .report_language_from_path(report_path)

      texts_df <- .make_smoke_texts_df(
        document_text = c("Text 1", "Text 2")
      )
      results_table <- data.frame(
        text = c("Text 1", "Text 2"),
        result = c("Topic A", "Topic B"),
        stringsAsFactors = FALSE
      )
      # Paragraph with prompt_fits = FALSE and NO supporting texts
      paragraph_entries <- list(list(
        topic = "Topic A",
        paragraph = "",
        texts = character(0),
        analysis_unit_ids = integer(0),
        prompt_fits = FALSE
      ))

      ar <- build_analysis_result(
        texts_df = texts_df,
        results_table = results_table,
        paragraph_entries = paragraph_entries,
        uuid = paste0("overflow-no-sources-", language),
        mode = "Onderwerpextractie",
        research_background = "",
        style_prompt = NULL,
        irr_result = NULL,
        language = language,
        by_column_name = NULL,
        by_column_lookup = NULL,
        models = .render_test_models(),
        topics = c("Topic A", "Topic B"),
        exclusive_topics = character(),
        assign_multiple_categories = FALSE,
        human_in_the_loop = FALSE,
        write_paragraphs = TRUE,
        context_window = list(
          batch_size = 25,
          draws = 2,
          n_batches = 2,
          n_tokens_context_window = 1000
        ),
        stage_prompt_previews = list(
          topic_candidate_generation = "prompt",
          topic_reduction = "prompt",
          topic_assignment = "prompt",
          paragraph_generation = "prompt"
        ),
        candidate_topics = c("Topic A", "Topic B"),
        reduced_topics = c("Topic A", "Topic B")
      )

      out_file <- file.path(
        out_dir,
        paste0("overflow-no-sources-", basename(report_path), ".html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(analysis_result = ar),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
      expect_match(
        html,
        expected_warnings[[language]],
        fixed = TRUE,
        label = paste0(
          "overflow warning in ",
          basename(report_path),
          " (no supporting texts)"
        )
      )
    }
  })
})


test_that("Categorisatie report renders with by_column_* set", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  irr_kappa <- list(
    subjects = 10,
    raters = 2,
    irr.name = "Kappa",
    stat.name = "z",
    statistic = 1.0,
    p.value = 0.5,
    value = 0.4
  )

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Categorisatie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      out_file <- file.path(
        out_dir,
        paste0(tools::file_path_sans_ext(basename(report_path)), ".html")
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_smoke_analysis_result(
              report_path,
              by_column_name = "group",
              by_column_lookup = data.frame(
                source_document_id = c(1L, 2L),
                by_value = c("G1", "G2"),
                stringsAsFactors = FALSE
              )
            )
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render with by_column failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      expect_true(file.exists(out_file))
      expect_true(file.info(out_file)$size > 0)
    }
  })
})

test_that("Categorisatie reports use correct category wording, not topic/subject", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  out_dir <- withr::local_tempdir()

  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Categorisatie_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  expected_multi <- list(
    en = c(
      "one or more of the following",
      "categories were assigned",
      "per category"
    ),
    nl = c(
      "of meer van de volgende",
      "zijn toegewezen",
      "per categorie"
    )
  )
  forbidden_multi <- list(
    en = c("subjects were added", "per topic"),
    nl = c("onderwerpen zijn toegevoegd", "per onderwerp")
  )

  expected_single <- list(
    en = c("assign one of the following", "per category"),
    nl = c("in te delen in \u00e9\u00e9n van de volgende", "per categorie")
  )

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      language <- .report_language_from_path(report_path)

      # --- Multi-label variant ---
      texts_df <- .make_smoke_texts_df(
        document_text = c("Text 1", "Text 2")
      )
      results_table <- data.frame(
        text = c("Text 1", "Text 2"),
        A = c(TRUE, FALSE),
        B = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      )
      paragraph_entries <- list(list(
        topic = "A",
        paragraph = 'Summary with "Text 1" & details.',
        texts = "Text 1",
        analysis_unit_ids = 1L,
        prompt_fits = TRUE
      ))

      ar_multi <- build_analysis_result(
        texts_df = texts_df,
        results_table = results_table,
        paragraph_entries = paragraph_entries,
        uuid = paste0("cat-multi-", language),
        mode = "Categorisatie",
        research_background = "",
        style_prompt = NULL,
        language = language,
        models = .render_test_models(),
        categories = c("A", "B"),
        assign_multiple_categories = TRUE,
        write_paragraphs = TRUE,
        stage_prompt_previews = list(
          categorization = "prompt",
          paragraph_generation = "paragraph prompt"
        )
      )

      out_multi <- file.path(
        out_dir,
        paste0("cat-multi-", basename(report_path), ".html")
      )
      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_multi,
          intermediates_dir = out_dir,
          params = list(analysis_result = ar_multi),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )
      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for multi-label ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html_multi <- gsub(
        "\\s+",
        " ",
        paste(readLines(out_multi, warn = FALSE), collapse = " ")
      )
      for (s in expected_multi[[language]]) {
        expect_true(grepl(s, html_multi, fixed = TRUE))
      }
      for (s in forbidden_multi[[language]]) {
        expect_false(grepl(s, html_multi, fixed = TRUE))
      }

      # No double-escaping: &amp;amp; or &amp;lt; must not appear
      expect_false(grepl("&amp;amp;", html_multi, fixed = TRUE))
      expect_false(grepl("&amp;lt;", html_multi, fixed = TRUE))
      # The properly-escaped & from the paragraph text must be present
      expect_true(grepl("&amp;", html_multi, fixed = TRUE))
      ar_single <- .build_smoke_analysis_result(report_path)
      out_single <- file.path(
        out_dir,
        paste0("cat-single-", basename(report_path), ".html")
      )
      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_single,
          intermediates_dir = out_dir,
          params = list(analysis_result = ar_single),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )
      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for single-label ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html_single <- gsub(
        "\\s+",
        " ",
        paste(readLines(out_single, warn = FALSE), collapse = " ")
      )
      for (s in expected_single[[language]]) {
        expect_true(grepl(s, html_single, fixed = TRUE))
      }
      for (s in forbidden_multi[[language]]) {
        expect_false(grepl(s, html_single, fixed = TRUE))
      }
    }
  })
})

test_that("Markeren reports escape supporting text HTML in paragraph accordions", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  out_dir <- withr::local_tempdir()
  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Markeren_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      language <- .report_language_from_path(report_path)
      out_file <- file.path(
        out_dir,
        paste0(
          "escape-",
          tools::file_path_sans_ext(basename(report_path)),
          ".html"
        )
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_marking_escape_analysis_result(language)
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
      expect_match(
        html,
        "&lt;kwallm-unsafe-tag&gt;boom&lt;/kwallm-unsafe-tag&gt;",
        fixed = TRUE
      )
      expect_false(grepl(
        "<kwallm-unsafe-tag>boom</kwallm-unsafe-tag>",
        html,
        fixed = TRUE
      ))
    }
  })
})

test_that("Markeren reports render paragraph overflow warnings", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not_installed("knitr")
  testthat::skip_if_not_installed("here")
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not(isTRUE(rmarkdown::pandoc_available()))

  expected_warning <- list(
    nl = "De prompt voor de samenvatting van deze code paste",
    en = "The prompt for the summary of this code did"
  )

  out_dir <- withr::local_tempdir()
  report_paths <- list.files(
    here::here("R"),
    pattern = "^report_Markeren_.*\\.Rmd$",
    full.names = TRUE
  )
  expect_true(length(report_paths) > 0)

  withr::with_dir(here::here(), {
    for (report_path in report_paths) {
      language <- .report_language_from_path(report_path)
      out_file <- file.path(
        out_dir,
        paste0(
          "overflow-",
          tools::file_path_sans_ext(basename(report_path)),
          ".html"
        )
      )

      res <- try(
        rmarkdown::render(
          input = report_path,
          output_file = out_file,
          params = list(
            analysis_result = .build_marking_overflow_analysis_result(language)
          ),
          quiet = TRUE,
          envir = .report_render_env(environment())
        ),
        silent = TRUE
      )

      if (inherits(res, "try-error")) {
        stop(paste0(
          "Render failed for ",
          basename(report_path),
          ": ",
          as.character(res)
        ))
      }

      html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
      expect_match(html, expected_warning[[language]], fixed = TRUE)
    }
  })
})
