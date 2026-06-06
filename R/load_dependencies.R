# Load dependencies ------------------------------------------------------------

#' Load dependencies
#'
#' This script handles dependency loading for all execution modes:
#' - "regular": Standard R environment with renv and reticulate
#' - "docker": Docker container with pre-installed packages
#' - "electron": Electron app with portable R and WinPython
#'
#' @param mode Character string: "regular", "docker", or "electron"
#'
#' @return The mode used (invisible)
load_dependencies <- function(mode = c("regular", "docker", "electron")) {
  mode <- match.arg(mode)

  if (!requireNamespace("cli", quietly = TRUE)) {
    message("Installing 'cli' package...")
    install.packages("cli")
  }

  cli::cli_rule()
  cli::cli_h2("Loading dependencies")
  cli::cli_alert_info("Loading dependencies for mode {.emph {mode}}...")

  # 1 Environment-specific setup -----------------------------------------------

  if (mode == "regular") {
    cli::cli_rule()
    cli::cli_h2("Setting up R & Python environments")

    # renv package management
    if (!requireNamespace("renv", quietly = TRUE)) {
      install.packages("renv")
    }

    # Setup R with renv
    cli::cli_h3("R")
    cli::cli_alert_info("Executing `renv::restore()` to sync R packages...")
    renv::restore()

    # Setup Python with reticulate & uv
    cli::cli_h3("Python")
    cli::cli_alert_info(
      "Executing `uv python install` & `uv sync` to sync Python packages..."
    )
    tryCatch(
      {
        initialize_python_environment(
          sync_uv = TRUE,
          install_python = TRUE,
          force_reload = TRUE
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Python setup failed: {conditionMessage(e)}"
        )
        cli::cli_alert_warning(
          "Features that depend on Python (tokenization, semantic chunking, GLiNER) will not work."
        )
      }
    )
  }

  if (mode == "electron") {
    cli::cli_rule()
    cli::cli_h2("Setting up portable R & WinPython environments")

    # Set library path explicitly to portable R library
    portable_lib <- file.path(dirname(R.home()), "library")
    .libPaths(portable_lib)

    cli::cli_alert_info("Setting R library path to portable R library...")
    cli::cli_alert_info("Using R library path: {.path {portable_lib}}")

    # Download portable WinPython
    tryCatch(
      {
        url <- "https://github.com/winpython/winpython/releases/download/16.6.20250620final/Winpython64-3.12.10.1dot.zip"
        expected_sha256 <- "7a1f004aec39615977b2b245423a50115530d16af3418df77977186a555d0a40"
        zip_file <- "WinPython.zip"
        extract_dir <- "winpython"

        if (!file.exists(extract_dir)) {
          download.file(url, zip_file, mode = "wb")
          actual_sha256 <- digest::digest(file = zip_file, algo = "sha256")

          cli::cli_alert_info(
            "WinPython: download SHA-256:\n{.emph {actual_sha256}}"
          )

          if (tolower(actual_sha256) != tolower(expected_sha256)) {
            stop(
              "WinPython: downloaded file is corrupted/tampered with (SHA-256 mismatch)"
            )
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

        cli::cli_alert_info("WinPython: using Python at {.path {python_path}}")

        Sys.setenv(UV_PYTHON = normalizePath(python_path))
      },
      error = function(e) {
        cli::cli_alert_danger(
          "WinPython setup failed: {conditionMessage(e)}"
        )
        cli::cli_alert_warning(
          "Features that depend on Python (tokenization, semantic chunking, GLiNER) will not work."
        )
      }
    )
  }

  # Docker mode: no special environment setup needed (pre-installed)

  # 2 Load core packages -------------------------------------------------------

  # Note: generally functions from packages are & should be called with
  # `package::function()` for safety, but loading here for convenience
  # For 'shiny' & 'htmltools' functions we make an exception as they are used
  # extensively and using `::` everywhere would be overly verbose

  cli::cli_rule()
  cli::cli_h2("Loading core R packages...")

  library(tidyverse)
  library(tidyprompt)
  library(shiny)
  library(shinyjs)
  library(bslib)
  library(htmltools)
  library(mirai)
  library(mori)
  library(promises)

  # Make a fake call to 'jsonvalidate' to avoid `renv::status()` complaint
  # 'jsonvalidate' is used as suggested dependency of 'tidyprompt', but
  # not directly called in the code. We make a fake call here to avoid
  # `renv::status()` reporting it as unused
  try(
    {
      invisible(jsonvalidate::json_validate("...", schema = NULL))
    },
    silent = TRUE
  )

  cli::cli_alert_success("R packages loaded")

  # 3 Load R functions ---------------------------------------------------------

  cli::cli_rule()
  cli::cli_h2("Loading files in {.path R/} folder...")

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

  cli::cli_alert_success("R files loaded")

  # 4 Initialize logger --------------------------------------------------------

  # Make app mode available to other modules/logging
  options(app__mode = mode)

  tryCatch(
    {
      log_init(mode = mode)
      log_info(
        paste("Application starting in", mode, "mode"),
        component = "startup"
      )
      log_info(paste("R version:", R.version.string), component = "startup")
      cli::cli_alert_success("Logger initialized")
    },
    error = function(e) {
      cli::cli_alert_warning(paste("Logger init failed:", e$message))
    }
  )

  # 5 Done ---------------------------------------------------------------------

  cli::cli_rule()
  cli::cli_h2("Dependencies loaded")
  cli::cli_alert_success("All dependencies loaded for mode {.emph {mode}}")
  cli::cli_rule()

  invisible(mode)
}

.python_environment_state_default <- function() {
  list(
    initialized = FALSE,
    virtualenv = "./.venv",
    install_python_ran = FALSE,
    sync_ran = FALSE
  )
}


# Helpers for Python environment -----------------------------------------------

# Some helpers to keep track of whether we've already initialized the Python environment,
# run the install_python step, or run the sync_uv step, to avoid redundant calls

initialize_python_environment <- function(
  virtualenv = "./.venv",
  sync_uv = FALSE,
  install_python = FALSE,
  force_reload = FALSE
) {
  stopifnot(
    is.character(virtualenv) && length(virtualenv) == 1,
    is.logical(sync_uv) && length(sync_uv) == 1,
    is.logical(install_python) && length(install_python) == 1,
    is.logical(force_reload) && length(force_reload) == 1
  )

  # Force UTF-8 mode BEFORE Python can be initialized (must be before the
  # early return, so mirai workers that inherit cached state still get it).
  # Prevents codecs.lookup() from receiving a raw Windows code page integer.
  if (.Platform$OS.type == "windows") {
    Sys.setenv(PYTHONUTF8 = "1")
    Sys.setenv(PYTHONIOENCODING = "utf-8")
  }

  st <- .python_environment_state_get()

  if (
    !isTRUE(force_reload) &&
      isTRUE(st$initialized) &&
      identical(st$virtualenv, virtualenv) &&
      (!isTRUE(sync_uv) || isTRUE(st$sync_ran)) &&
      (!isTRUE(install_python) || isTRUE(st$install_python_ran))
  ) {
    return(invisible(st))
  }

  Sys.unsetenv("RETICULATE_PYTHON")

  if (isTRUE(install_python)) {
    reticulate:::uv_exec("python install")
  }

  if (isTRUE(sync_uv)) {
    reticulate:::uv_exec("sync")
  }

  suppressWarnings(reticulate::use_virtualenv(virtualenv))

  # Patch codecs.lookup(), codecs.encode(), and codecs.decode() to coerce
  # integer code-page numbers to strings. On Windows, some import chains pass
  # a raw code page int (e.g. 1252) instead of the string "cp1252", crashing
  # with either:
  #   TypeError: lookup() argument must be str, not int
  #   TypeError: encode() argument 'encoding' must be str, not int
  if (.Platform$OS.type == "windows") {
    tryCatch(
      reticulate::py_run_string(paste0(
        "import codecs as _codecs\n",
        "if not hasattr(_codecs, '_kwallm_patched'):\n",
        "    def _coerce_encoding(encoding):\n",
        "        return ('cp' + str(encoding)) if isinstance(encoding, int) else encoding\n",
        "    _orig_lookup = _codecs.lookup\n",
        "    def _safe_lookup(encoding):\n",
        "        return _orig_lookup(_coerce_encoding(encoding))\n",
        "    _codecs.lookup = _safe_lookup\n",
        "    _orig_encode = _codecs.encode\n",
        "    def _safe_encode(obj, encoding='utf-8', errors='strict'):\n",
        "        return _orig_encode(obj, _coerce_encoding(encoding), errors)\n",
        "    _codecs.encode = _safe_encode\n",
        "    _orig_decode = _codecs.decode\n",
        "    def _safe_decode(obj, encoding='utf-8', errors='strict'):\n",
        "        return _orig_decode(obj, _coerce_encoding(encoding), errors)\n",
        "    _codecs.decode = _safe_decode\n",
        "    _codecs._kwallm_patched = True\n"
      )),
      error = function(e) {
        warning(
          "codecs safety patch could not be applied: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  st$initialized <- TRUE
  st$virtualenv <- virtualenv
  st$install_python_ran <- isTRUE(st$install_python_ran) ||
    isTRUE(install_python)
  st$sync_ran <- isTRUE(st$sync_ran) || isTRUE(sync_uv)

  .python_environment_state_set(st)

  invisible(st)
}


#' Import a Python module with a safety net for the Windows codecs bug.
#'
#' On Windows, `codecs.lookup()` / `codecs.encode()` sometimes receive an
#' integer code-page number instead of a string, crashing with
#' `TypeError: lookup() argument must be str, not int` or
#' `TypeError: encode() argument 'encoding' must be str, not int`.
#' This wrapper catches that specific error, (re-)applies the codecs
#' monkey-patch, and retries once.
#'
#' @param module Module name passed to [reticulate::import()].
#' @param ... Further arguments forwarded to [reticulate::import()].
#' @return The imported Python module object.
safe_py_import <- function(module, ...) {
  tryCatch(
    reticulate::import(module, ...),
    error = function(first_err) {
      is_codecs_bug <- grepl(
        "(lookup|encode|decode).*must be str.*not int",
        first_err$message,
        ignore.case = TRUE
      )
      if (!is_codecs_bug) {
        stop(first_err)
      }

      # Re-apply the codecs patch (may have been lost or never applied)
      tryCatch(
        reticulate::py_run_string(paste0(
          "import codecs as _codecs\n",
          "def _coerce_encoding(encoding):\n",
          "    return ('cp' + str(encoding)) if isinstance(encoding, int) else encoding\n",
          "_orig_lookup = getattr(_codecs, '_orig_lookup', _codecs.lookup)\n",
          "def _safe_lookup(encoding):\n",
          "    return _orig_lookup(_coerce_encoding(encoding))\n",
          "_codecs.lookup = _safe_lookup\n",
          "_orig_encode = getattr(_codecs, '_orig_encode', _codecs.encode)\n",
          "def _safe_encode(obj, encoding='utf-8', errors='strict'):\n",
          "    return _orig_encode(obj, _coerce_encoding(encoding), errors)\n",
          "_codecs.encode = _safe_encode\n",
          "_orig_decode = getattr(_codecs, '_orig_decode', _codecs.decode)\n",
          "def _safe_decode(obj, encoding='utf-8', errors='strict'):\n",
          "    return _orig_decode(obj, _coerce_encoding(encoding), errors)\n",
          "_codecs.decode = _safe_decode\n",
          "_codecs._kwallm_patched = True\n"
        )),
        error = function(e) NULL
      )

      # Retry the import once
      reticulate::import(module, ...)
    }
  )
}

.python_environment_state_get <- function() {
  st <- getOption("kwallm__python_environment_state", NULL)
  if (is.null(st) || !is.list(st)) {
    st <- .python_environment_state_default()
    options(kwallm__python_environment_state = st)
  }

  defaults <- .python_environment_state_default()
  for (nm in names(defaults)) {
    if (is.null(st[[nm]])) st[[nm]] <- defaults[[nm]]
  }

  st
}

.python_environment_state_set <- function(st) {
  if (is.null(st) || !is.list(st)) {
    st <- .python_environment_state_default()
  }

  options(kwallm__python_environment_state = st)
  invisible(NULL)
}
