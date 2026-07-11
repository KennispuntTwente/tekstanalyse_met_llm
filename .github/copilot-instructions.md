# Copilot instructions for KWALLM (tekstanalyse_met_llm)

This repo is a Shiny app (R) with an Electron desktop wrapper and Docker image. The app performs qualitative text analysis with LLMs. These notes capture the architecture, workflows, and conventions an AI assistant should follow when making changes.

## Big picture & architecture
- Entry points: `app.R` (local/dev), `Dockerfile-app.R` (container), `package-main.js` + `package-app.R` (Electron desktop).
- Core composition: UI/server orchestration lives in `R/module_core_main_ui_and_server.R`; individual features are Shiny modules in `R/*.R` following the `<feature>_ui()` / `<feature>_server()` pattern.
- Analysis modes: Categorization, Scoring, Topic extraction, Marking. Mode-specific workflows live in `R/analysis_*.R` and are orchestrated by `R/module_core_processing.R`.
- Results: Analysis outputs are captured in an S7 `AnalysisResult` object (see `R/result_model.R`, `R/result_builders.R`). Reports, Excel exports, and download bundles are built from this typed object via `R/result_serializers.R`.
- Reports: Mode-specific report templates live in `R/report_*.Rmd` (plus pre-rendered `.html` examples) and receive `params$analysis_result` directly.

## Data flow (from upload to results)
- Upload: `R/module_input_text_upload.R` reads `.txt`/`.csv`/`.xlsx`/`.sav`, handles sheet/column selection, and provides a reactive data frame. `.txt` upload is BOM-aware (UTF-8, UTF-16LE/BE).
- Preprocess & privacy: `R/module_input_text_management.R` and `R/module_misc_gliner_anonymization.R` handle anonymization (regex or GLiNER) before any LLM calls. Processing must not start when GLiNER anonymization is requested but not yet completed. Chunking and context sizing is handled by `R/module_misc_context_window.R`, `R/module_input_text_split.R`, and helpers in `R/utils_*`.
- Prompt & LLM: Prompts are built with `tidyprompt` inside `R/analysis_*.R` using helpers like `prompt_category`, `prompt_multi_category`, `prompt_score`, `assign_topics`. All calls go through `R/utils_send_prompt_with_retries.R` (respect the `send_prompt_with_retries__*` options; optional logging to `prompt_logs`).
- Processing & output: `R/module_core_processing.R` joins LLM outputs back to the original texts, optionally builds per-category/topic paragraphs, and builds the `AnalysisResult` via `R/result_builders.R`.

## Configuration (set in `app.R` / `Dockerfile-app.R`)
- LLM provider toggles/defaults: `llm_provider__can_configure_oai`, `llm_provider__default_oai_url`, `llm_provider__can_configure_ollama`, `llm_provider__default_ollama_url`.
- Retry/robustness for LLM calls: `send_prompt_with_retries__max_tries`, `send_prompt_with_retries__retry_delay_seconds`, `send_prompt_with_retries__max_interactions`, `send_prompt_with_retries__log_prompts`, `send_prompt_with_retries__log_prompts_to_file`.
- Processing limits: `processing__max_texts` guards how many texts can be sent in one run. `marking__max_combinations` caps marking code combinations.
- Language & UI: `language` ("en"/"nl"), `language__can_toggle`; translations live in `language/language.json` and are loaded via `R/module_config_language.R`.
- Privacy & anonymization: `anonymization__default` ("none" | "regex" | "gliner") and related availability toggles control what users can select.
- Topic extraction: `topic_modelling__batch_size_default`, `topic_modelling__batch_size_limit`, `topic_modelling__number_of_batches_limit`, `topic_modelling__draws_default`, `topic_modelling__draws_limit`.
- Logging: `logger__level`, `logger__dir`, `logger__retention`.
- Paragraph summaries: `paragraph_streaming` controls live output;
  `paragraph_summary_strategy` selects recursive `"batch"` reduction or a
  context-sized random `"sample"`; `paragraph_summary_max_reduction_iterations`
  caps recursive summary reduction.

## Async workers (mirai) — critical section
All long-running work runs in `mirai::mirai()` daemon workers. Workers are separate R processes that do **not** share the Shiny session's environment.

### Daemon pool
- Entry points call `kwallm_ensure_mirai_daemons()` (from `R/utils_mirai_daemons.R`) at startup. This validates the existing pool with a lightweight probe and recycles stale sockets before creating new daemons.
- Default worker count: `min(detectCores(), KWALLM_N_ASYNC_WORKERS env, 2)`.
- Do **not** stop shared daemons in `session$onSessionEnded()`; that breaks multi-user/server processes.

### Worker bootstrap pattern
Every mirai call must use the bootstrap helpers from `R/utils_async_analysis_workers.R`:

```r
mirai::mirai(
  {
    kwallm_worker_bootstrap(
      task = "categorization",
      app_root = app_root,
      worker_options = worker_options,
      log_context = log_context
    )
    # ... worker body ...
  },
  .args = c(
    list(
      app_root = kwallm_worker_app_root(),
      worker_options = kwallm_worker_capture_options(),
      log_context = log_context,
      # ... task-specific data ...
    ),
    kwallm_worker_bootstrap_globals()
  )
)
```

Key functions:
- `kwallm_worker_bootstrap(task, app_root, worker_options, log_context)` — Sources the app code from `app_root`, applies captured options, and sets up logging inside the worker. This is the **only** way workers get access to app functions.
- `kwallm_worker_app_root()` — Returns the repo root path for `source()` inside the worker.
- `kwallm_worker_capture_options()` — Snapshots the explicit whitelist of runtime options (app mode, logger settings, prompt retry settings, topic/marking knobs, `paragraph_streaming`, `tidyprompt.*`, `kwallm.test_fake_llm`) for replay in the worker.
- `kwallm_worker_bootstrap_globals()` — Returns `list(kwallm_worker_bootstrap = <fn>)` for `.args`.

### Rules for worker code
1. **Never pass raw app functions in `.args`**. Workers get all functions by sourcing the app through `kwallm_worker_bootstrap()`. Only pass *data* (data frames, character vectors, option snapshots) and the bootstrap function itself.
2. **Never use unqualified operators** (`%||%`, `%>%`) in worker bodies without ensuring they are available after bootstrap. The bootstrap sources `load_dependencies.R` which loads packages, but pipe operators from specific packages must be in scope.
3. **Python in workers** — Workers that need Python (tokenizer, semchunk, GLiNER) must call `initialize_python_environment()` (available post-bootstrap). The Python venv state is not inherited across processes.
4. **Progress & cancellation** — Use serializable `progress_*$async` objects and the `interrupter$execInterrupts()` guard from `R/utils_nanonext_reactive_channel.R`. Do not pass full Shiny/session objects into workers.
5. **Guardrail test** — `tests/testthat/test-async-worker-bootstrap.R` statically verifies every production `mirai::mirai()` call site uses the bootstrap pattern. New call sites must pass this check.

### Worker call sites
| File | Tasks |
|------|-------|
| `R/module_core_processing.R` | categorization, scoring, topic_generation, topic_assignment, topic_reduction, marking |
| `R/module_input_text_split.R` | text_split |
| `R/module_input_marking_codes.R` | code_generation |
| `R/module_config_llm_provider.R` | llm_provider_models_fetch |
| `R/module_config_model.R` | model_provider_test |
| `R/module_misc_gliner_anonymization.R` | gliner |
| `R/module_misc_edit_topics.R` | topic_reduction |
| `R/component_llm_streaming.R` | streaming |

## Error handling
- Wrap expensive work in `tryCatch(...)` and surface issues with `app_error(...)` / `handle_detailed_error()` helpers in `R/utils_*` instead of `stop()`; keep the Shiny session responsive.

## Testing
- `tests/testthat/test-e2e-*.R` drive full end-to-end flows with {shinytest2}. Tests expect an OpenAI-compatible endpoint that answers `/models` and exposes a model id like `gpt-4.1-nano-2025-04-14`. Use `shiny::exportTestValues` when adding new critical reactives.
- `tests/testthat/test-async-worker-bootstrap.R` ensures all mirai call sites use the bootstrap pattern.
- `tests/testthat/test-async-worker-dependency-coverage.R` validates worker dependency completeness.
- Integration tests (`test-integration-*.R`) cover high-volume, paragraph-writing, and topic-modelling async flows.
- Module-level `testServer` tests use `mirai` stubs that evaluate with `parent = baseenv()` and inject required bindings explicitly; never rely on `parent.frame()` fallback because it masks missing worker objects.

## Python integration (via `reticulate`)
- Tokenization: `R/utils_tokenizer.R` loads Python `tiktoken` via `reticulate` and a uv-managed venv (`reticulate:::uv_exec('sync')`, `reticulate::use_virtualenv('./.venv')`). Keep all Python deps declared in `pyproject.toml`.
- GLiNER & semchunk: Used for anonymization and semantic chunking (see `R/utils_gliner.R`, `R/utils_semchunk.R`). Docker and desktop builds warm relevant caches; when changing models, adjust these loaders rather than calling Python directly elsewhere.

## Build & run workflows
- R dev: Open `KWALLM.Rproj`, run `renv::restore()` to install R packages, then run the app via `app.R`.
- Electron desktop: Scripts in `scripts/` provision portable R/Pandoc; `package-main.js` spawns `package-app.R` and opens the Shiny URL in an Electron window.
- Docker: `Dockerfile` uses a rocker build stage to restore `renv`, then copies the app into an Ubuntu runtime and runs `shiny::runApp` on port `3838`.

## When adding or changing features
- Follow the module pattern: create `<feature>_ui()` and `<feature>_server()` in a new `R/module_*` file; wire it into `R/module_core_main_ui_and_server.R` and, if relevant, into `R/module_core_processing.R`.
- For LLM interactions, always build prompts with `tidyprompt` in `R/analysis_*.R` and send via `send_prompt_with_retries()` from the utils layer; never call provider SDKs directly.
- Keep outputs flowing into the S7 `AnalysisResult`; use `R/result_builders.R` to construct and `R/result_serializers.R` to export. Do not bypass the typed result model.
- **Adding async work**: Follow the bootstrap pattern above. Add the new `mirai::mirai()` call with `kwallm_worker_bootstrap_globals()` in `.args`. Ensure the guardrail test in `test-async-worker-bootstrap.R` still passes.
- Reuse existing `component_*.R` UI patterns (cards, header with tooltip, icon buttons, progress bar, yes/no toggle card) to keep the interface consistent.
- When in doubt about where a file belongs, consult `R/README.md` and match existing prefixes (`analysis_`, `component_`, `module_config_`, `module_core_`, `module_input_`, `module_misc_`, `module_toggle_`, `report_`, `result_`, `style_`, `utils_`).
