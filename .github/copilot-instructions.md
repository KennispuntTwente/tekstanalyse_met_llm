# Copilot instructions for KWALLM (tekstanalyse_met_llm)

This repo is a Shiny app (R) with an Electron desktop wrapper and Docker image. The app performs qualitative text analysis with LLMs. These notes capture the architecture, workflows, and conventions an AI assistant should follow when making changes.

## Big picture & architecture
- Entry points: `app.R` (local/dev), `Dockerfile-app.R` (container), `package-main.js` + `package-app.R` (Electron desktop).
- Core composition: UI/server orchestration lives in `R/module_core_main_ui_and_server.R`; individual features are Shiny modules in `R/*.R` following the `<feature>_ui()` / `<feature>_server()` pattern.
- Analysis modes: Categorization, Scoring, Topic extraction, Marking. Mode-specific workflows live in `R/analysis_*.R` and are orchestrated by `R/module_core_processing.R`.
- Reports: Mode-specific report templates live in `R/report_*.Rmd` (plus pre-rendered `.html` examples) and are used to generate user-facing HTML/Excel outputs.

## Data flow (from upload to results)
- Upload: `R/module_input_text_upload.R` reads `.txt`/`.csv`/`.xlsx`/`.sav`, handles sheet/column selection, and provides a reactive data frame.
- Preprocess & privacy: `R/module_input_text_management.R` and `R/module_misc_gliner_anonymization.R` handle anonymization (regex or GLiNER) before any LLM calls. Chunking and context sizing is handled by `R/module_misc_context_window.R`, `R/module_input_text_split.R`, and helpers in `R/utils_*`.
- Prompt & LLM: Prompts are built with `tidyprompt` inside `R/analysis_*.R` using helpers like `prompt_category`, `prompt_multi_category`, `prompt_score`, `assign_topics`. All calls go through `R/utils_send_prompt_with_retries.R` (respect the `send_prompt_with_retries__*` options; optional logging to `prompt_logs`).
- Processing & output: `R/module_core_processing.R` joins LLM outputs back to the original texts, optionally builds per-category/topic paragraphs, and passes data into `R/report_*` templates.

## Configuration (set in `app.R` / `Dockerfile-app.R`)
- LLM provider toggles/defaults: `llm_provider__can_configure_oai`, `llm_provider__default_oai_url`, `llm_provider__can_configure_ollama`, `llm_provider__default_ollama_url`.
- Retry/robustness for LLM calls: `send_prompt_with_retries__max_tries`, `send_prompt_with_retries__retry_delay_seconds`, `send_prompt_with_retries__max_interactions`, `send_prompt_with_retries__log_prompts`.
- Processing limits: `processing__max_texts` guards how many texts can be sent in one run.
- Language & UI: `language` ("en"/"nl"), `language__can_toggle`; translations live in `language/language.json` and are loaded via `R/module_config_language.R`.
- Privacy & anonymization: `anonymization__default` ("none" | "regex" | "gliner") and related availability toggles control what users can select.
- Topic extraction: `topic_modelling__*` governs topic modelling behaviour; context window sizing is currently hardcoded in helpers under `R/module_misc_context_window.R`.

## Async, errors, and testing
- Async: Long-running steps use `future()` / `future_promise()` (see `R/module_core_processing.R`); progress is written via `component_progress_bar.R`/`progress_bar_server(...).$async`, and cancellation is guarded by `ipc::AsyncInterruptor`.
- Error handling: Wrap expensive work in `tryCatch(...)` and surface issues with `app_error(...)` / `handle_detailed_error()` helpers in `R/utils_*` instead of `stop()`; keep the Shiny session responsive.
- Tests: `tests/testthat/test-shinytest2.R` drives full flows with {shinytest2}. Tests expect an OpenAI-compatible endpoint that answers `/models` and exposes a model id like `gpt-4.1-nano-2025-04-14`. Use `shiny::exportTestValues` when adding new critical reactives.

## Python integration (via `reticulate`)
- Tokenization: `R/utils_tokenizer.R` (or `R/tokenizer.R` in older branches) loads Python `tiktoken` via `reticulate` and a uv-managed venv (`reticulate:::uv_exec('sync')`, `reticulate::use_virtualenv('./.venv')`). Keep all Python deps declared in `pyproject.toml`.
- GLiNER & semchunk: Used for anonymization and semantic chunking (see `R/gliner_*`, `R/semchunk_load.R`). Docker and desktop builds warm relevant caches; when changing models, adjust these loaders rather than calling Python directly elsewhere.

## Build & run workflows
- R dev: Open `tekstanalyse_met_llm.Rproj`, run `renv::restore()` to install R packages, then run the app via `app.R`.
- Electron desktop: Scripts in `scripts/` provision portable R/Pandoc; `package-main.js` spawns `package-app.R` and opens the Shiny URL in an Electron window.
- Docker: `Dockerfile` uses a rocker build stage to restore `renv`, then copies the app into an Ubuntu runtime and runs `shiny::runApp` on port `3838`.

## When adding or changing features
- Follow the module pattern: create `<feature>_ui()` and `<feature>_server()` in a new `R/module_*` file; wire it into `R/module_core_main_ui_and_server.R` and, if relevant, into `R/module_core_processing.R`.
- For LLM interactions, always build prompts with `tidyprompt` in `R/analysis_*.R` and send via `send_prompt_with_retries()` from the utils layer; never call provider SDKs directly.
- Keep outputs mergeable with the original texts: if you attach extra metadata (e.g., paragraphs, topic summaries), store it as attributes on the results data frame as done in the existing analysis functions.
- Reuse existing `component_*.R` UI patterns (cards, header with tooltip, icon buttons, progress bar, yes/no toggle card) to keep the interface consistent.
- When in doubt about where a file belongs, consult `R/README.md` and match existing prefixes (`analysis_`, `component_`, `module_config_`, `module_core_`, `module_input_`, `module_misc_`, `module_toggle_`, `report_`, `style_`, `utils_`).
