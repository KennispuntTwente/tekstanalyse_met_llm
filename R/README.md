# R/ folder overview

Quick overview of the files & naming conventions used in this folder.

## Files

| Prefix | Description |
|--------|-------------|
| `analysis_` | Main analysis workflow implementations (categorization, marking, scoring, etc.) |
| `component_` | Reusable Shiny UI components (buttons, cards, modals, progress bars) |
| `module_config_` | Configuration modules (LLM provider, model, language, mode); user can use these to configure advanced options during use |
| `module_core_` | Core application modules (main UI/server, processing logic); these bring together the different modules to create a complete application & orchestrate the analysis workflow |
| `module_input_` | Input handling modules (text upload, categories, marking codes); user can use these to input data for analysis |
| `module_misc_` | Miscellaneous modules (anonymization, context window, edit topics) |
| `module_toggle_` | Toggle/switch option modules (interrater reliability, write paragraphs); user can use these to turn off/on simple options during use |
| `report_` | R Markdown report templates (`.Rmd`) for presenting analysis results to users |
| `result_` | S7 typed result model, builders, and serializers for export contracts |
| `style_` | CSS/JS styling and DataTable configurations |
| `utils_` | Utility/helper functions (tokenizer, error handling, API retries) |

### Special file

| File | Description |
|------|-------------|
| `load_dependencies.R` | Centralized dependency loading and environment setup; is called from 'app.R' (& variants) |
