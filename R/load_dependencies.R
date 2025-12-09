#' Load Dependencies
#'
#' This script handles dependency loading for all execution modes:
#' - "regular": Standard R environment with renv and reticulate
#' - "docker": Docker container with pre-installed packages
#' - "electron": Electron app with portable R and WinPython
#'
#' @param mode Character string: "regular", "docker", or "electron"
#'
load_dependencies <- function(mode = c("regular", "docker", "electron")) {
  mode <- match.arg(mode)

  cat("Loading dependencies for mode:", mode, "\n")


  # 1 Environment-specific setup ----------------------------------------------

  if (mode == "regular") {
    # renv package management
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv")
    }
    renv::restore()

    # Setup Python with reticulate & uv
    try({
      Sys.unsetenv("RETICULATE_PYTHON")
      reticulate:::uv_exec("sync")
      reticulate::use_virtualenv("./.venv")
    })
  }

  if (mode == "electron") {
    # Set library path explicitly to portable R library
    portable_lib <- file.path(dirname(R.home()), "library")
    .libPaths(portable_lib)
    print(paste("Using library path:", portable_lib))

    # Download portable WinPython
    try({
      url <- "https://github.com/winpython/winpython/releases/download/16.6.20250620final/Winpython64-3.12.10.1dot.zip"
      expected_sha256 <- "7a1f004aec39615977b2b245423a50115530d16af3418df77977186a555d0a40"
      zip_file <- "WinPython.zip"
      extract_dir <- "winpython"

      if (!file.exists(extract_dir)) {
        download.file(url, zip_file, mode = "wb")
        actual_sha256 <- digest::digest(file = zip_file, algo = "sha256")

        cat("WinPython: downloaded SHA-256:", actual_sha256, "\n")
        if (tolower(actual_sha256) != tolower(expected_sha256)) {
          stop("SHA-256 hash mismatch! File may be corrupted or tampered")
        }

        dir.create(extract_dir, showWarnings = FALSE)
        unzip(zip_file, exdir = extract_dir)
      }

      python_paths <- list.files(
        extract_dir,
        pattern = "python.exe$",
        recursive = TRUE,
        full.names = TRUE
      )

      # Filter out venv-related paths
      valid_python_paths <- python_paths[
        !grepl("venv|scripts|nt", tolower(python_paths))
      ]

      # Pick the first valid path (or throw an error if none found)
      if (length(valid_python_paths) == 0) {
        stop("No valid base python.exe found")
      }

      python_path <- valid_python_paths[1]

      if (is.na(python_path) || !file.exists(python_path)) {
        stop("WinPython: executable not found")
      }

      cat("WinPython: using Python at", python_path, "\n")
      Sys.setenv(UV_PYTHON = normalizePath(python_path))
    })
  }

  # Docker mode: no special environment setup needed (pre-installed)


  # 2 Load core packages ----------------------------------------------------

  library(tidyverse)
  library(tidyprompt)
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(htmltools)
  library(future)
  library(promises)


  # 3 Load R functions ------------------------------------------------------

  load_all <- function(except = c("R/load_dependencies.R")) {
    r_files <- list.files(
      path = "R",
      pattern = "\\.R$",
      full.names = TRUE
    )
    for (file in r_files) {
      if (file %in% except) {
        next
      }
      source(file)
    }
  }
  load_all()


  # 4 Done ------------------------------------------------------------------

  cli::cli_alert_success("Dependencies loaded for mode {mode}")

  invisible(mode)
}
