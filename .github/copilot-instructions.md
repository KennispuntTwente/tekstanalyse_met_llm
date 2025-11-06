# Copilot instructions for KWALLM (tekstanalyse_met_llm)

This repo is a Shiny app (R) with an Electron desktop wrapper and Docker image. The app performs qualitative text analysis with LLMs. These notes capture the architecture, workflows, and conventions you should follow when making changes.

## Big picture
- Entry points: `app.R` (local/dev), `Dockerfile-app.R` (container), `package-main.js` + `package-app.R` (Electron desktop).
- Core app composition: UI/server is orchestrated in `R/main_ui_and_server.R`; individual modules live in `R/*.R` (pattern: `<feature>_ui` / `<feature>_server`).
- Modes (processing strategies): Categorization, Scoring, Topic extraction, Marking. The pipeline is implemented in `R/processing.R` with async execution via `future`/`promises` and progress via `R/progress_bar.R`.
- LLM access: Uses `tidyprompt`. Preconfigure models in `app.R` or let the user connect to OpenAI-compatible or Ollama in `R/llm_provider.R` (can ping `/models` and render a model dropdown).

## Data flow & key modules
- Upload: `R/text_upload.R` reads .txt/.csv/.xlsx/.sav, sheet/column selection, and an optional filter modal. Raw data → reactive data frame.
- Preprocess/privacy: `R/text_management.R` handles anonymization (regex or GLiNER) before any LLM calls. Chunking and context sizing is handled by `R/context_window.R` and `R/text_split.R`.
- Prompt/LLM calls: Prompts are built with `tidyprompt` (see prompt helpers like `prompt_category`, `prompt_multi_category`, `prompt_score`, `assign_topics`). Calls go through `R/send_prompt_with_retries.R` (respect `send_prompt_with_retries__*` options; optional prompt logging to `prompt_logs`).
- Results: `processing.R` joins LLM outputs back to original texts, optionally writes per-category/topic paragraphs, and generates an Excel + HTML report via `R/report_*` files.

## Configuration conventions (set in `app.R` or `Dockerfile-app.R`)
- LLM provider toggles and defaults: `llm_provider__can_configure_oai`, `llm_provider__default_oai_url`, `llm_provider__can_configure_ollama`, `llm_provider__default_ollama_url`.
- Retry/robustness for LLM calls: `send_prompt_with_retries__max_tries`, `send_prompt_with_retries__retry_delay_seconds`, `send_prompt_with_retries__max_interactions`, `send_prompt_with_retries__log_prompts`.
- Processing limits: `processing__max_texts`.
- Language & UI: `language` ("en"/"nl"), `language__can_toggle`; translations in `language/language.json` via `R/language.R`.
- Privacy: `anonymization__default` ("none"|"regex"|"gliner"), and per-method availability toggles.
- Topic extraction controls: `topic_modelling__*`; context window sizing is currently hardcoded in `R/context_window.R`.

## Async, testing, and error patterns
- Async: Long-running steps use `future()` / `future_promise()`; write progress via `progress_bar_server(...).$async` and guard stops with `ipc::AsyncInterruptor` in `processing.R`.
- Error handling: Wrap work inside `tryCatch(...)` and use `app_error(...)` or helpers like `handle_detailed_error()` to surface actionable messages to the UI; keep the pipeline non-blocking.
- Test conventions: `tests/testthat/test-shinytest2.R` drives full flows with {shinytest2}. Tests expect an OpenAI-compatible endpoint to answer `/models` and include a model id like `gpt-4.1-nano-2025-04-14`. Exported reactive values (e.g., via `shiny::exportTestValues`) are used for assertions.

## Python integration
- Tokenization: `R/tokenizer.R` loads Python `tiktoken` through `reticulate` and uv-managed venv (`reticulate:::uv_exec('sync')`, `reticulate::use_virtualenv('./.venv')`).
- GLiNER & semchunk: used for anonymization and semantic chunking (see `R/gliner_*`, `R/semchunk_load.R`). The Dockerfile warms caches; in desktop builds, the Electron launcher sets PATHs for portable R/Pandoc.

## Build/run workflows (where to look)
- R dev: open `tekstanalyse_met_llm.Rproj`, `renv::restore()` will install R deps. Run the app via `app.R`.
- Electron desktop: scripts under `scripts/` provision portable R/Pandoc; `package-main.js` spawns `package-app.R` and opens the Shiny URL.
- Docker: `Dockerfile` uses a rocker build stage to restore `renv`, then copies the app into an Ubuntu runtime and runs `shiny::runApp` on port 3838.

## When adding features
- Follow the module pattern: create `<feature>_ui()` and `<feature>_server()`; wire into `main_ui_and_server.R` and add mode branches or UI placement where relevant.
- For LLM interactions, construct prompts with `tidyprompt`, call via `send_prompt_with_retries`, and respect the relevant `options()` keys instead of hardcoding.
- Keep outputs mergeable with the original texts; if you attach extra metadata (like `paragraphs`), store it as attributes on the results data frame as done in `processing.R`.
