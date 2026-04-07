#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_file_arg <- grep("^--file=", commandArgs(), value = TRUE)

if (length(script_file_arg) == 1) {
  script_path <- normalizePath(sub("^--file=", "", script_file_arg))
  repo_root <- dirname(dirname(script_path))
  setwd(repo_root)
}

shard_ids <- c(
  "unit",
  "integration",
  "e2e-categorization",
  "e2e-scoring",
  "e2e-marking",
  "e2e-topic",
  "e2e-misc"
)

usage <- paste(
  "Usage:",
  "  Rscript tests/run_test_shard.R --check",
  "  Rscript tests/run_test_shard.R --list-shards",
  "  Rscript tests/run_test_shard.R <shard-id>",
  sep = "\n"
)

classify_test_file <- function(file_name) {
  if (grepl("^test-integration-.*[.]R$", file_name)) {
    return("integration")
  }

  if (grepl("^test-e2e-categorization.*[.]R$", file_name)) {
    return("e2e-categorization")
  }

  if (grepl("^test-e2e-scoring[.]R$", file_name)) {
    return("e2e-scoring")
  }

  if (grepl("^test-e2e-marking[.]R$", file_name)) {
    return("e2e-marking")
  }

  if (grepl("^test-e2e-topic-modelling.*[.]R$", file_name)) {
    return("e2e-topic")
  }

  if (grepl("^test-e2e-", file_name)) {
    return("e2e-misc")
  }

  "unit"
}

collect_test_assignments <- function() {
  test_files <- sort(list.files(
    file.path("tests", "testthat"),
    pattern = "^test-.*[.]R$",
    full.names = TRUE
  ))

  if (!length(test_files)) {
    stop("No test files found under tests/testthat.", call. = FALSE)
  }

  assignments <- data.frame(
    file = test_files,
    file_name = basename(test_files),
    shard = vapply(basename(test_files), classify_test_file, character(1)),
    stringsAsFactors = FALSE
  )

  unknown_shards <- setdiff(unique(assignments$shard), shard_ids)
  if (length(unknown_shards)) {
    stop(
      sprintf(
        "Unknown shard ids produced by classifier: %s",
        paste(unknown_shards, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  shard_counts <- table(factor(assignments$shard, levels = shard_ids))
  empty_shards <- names(shard_counts)[shard_counts == 0L]

  if (length(empty_shards)) {
    stop(
      sprintf(
        "Configured shard(s) matched no test files: %s",
        paste(empty_shards, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  assignments
}

print_assignment_summary <- function(assignments) {
  shard_counts <- table(factor(assignments$shard, levels = shard_ids))

  cat("Test shard summary:\n")
  for (shard_id in shard_ids) {
    cat(sprintf("- %s: %d file(s)\n", shard_id, shard_counts[[shard_id]]))
  }
}

run_shard <- function(assignments, shard_id) {
  shard_files <- assignments$file[assignments$shard == shard_id]

  if (!length(shard_files)) {
    stop(
      sprintf("Shard '%s' matched no test files.", shard_id),
      call. = FALSE
    )
  }

  cat(sprintf(
    "Running shard '%s' with %d test file(s):\n",
    shard_id,
    length(shard_files)
  ))
  cat(paste0("- ", basename(shard_files), "\n"), sep = "")

  for (file in shard_files) {
    testthat::test_file(file, stop_on_failure = TRUE)
  }
}

if (!length(args)) {
  stop(usage, call. = FALSE)
}

command <- args[[1]]
assignments <- collect_test_assignments()

if (identical(command, "--list-shards")) {
  cat(paste(shard_ids, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 0)
}

if (identical(command, "--check")) {
  print_assignment_summary(assignments)
  quit(save = "no", status = 0)
}

if (!(command %in% shard_ids)) {
  stop(
    sprintf("Unknown shard '%s'.\n\n%s", command, usage),
    call. = FALSE
  )
}

print_assignment_summary(assignments)
run_shard(assignments, command)
