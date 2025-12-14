# Sectioned (Horizontal) Layout View Plan

## Goal

The current UI in `R/module_core_main_ui_and_server.R` renders all “cards” (module UIs) in one long vertical stack. This works and should remain available.

Add an *additional* “horizontal/sectioned” view where the user navigates between a small number of **sections** (steps). Each section shows only a few existing cards at a time. The cards themselves remain functionally and visually the same; we only change *which subset is visible* and add navigation UI.

## High-level UX proposal

- A small **layout toggle** near the top of the page:
  - `Vertical` (current behaviour; default)
  - `Sections` (new guided / horizontal flow)
- In `Sections` view:
  - A **stepper** (section titles) at the top with a subtle **progress indicator**
  - A “Back / Next” control row
  - Optional: smooth left/right slide transition between sections (carousel-like)
  - Optional: click step titles to jump (keeps it non-blocking / flexible)

## Inventory: current “card stack”

Main UI currently mounts these module UIs in this order (inside one `div(class = "card-container", ...)`):

- `text_upload_ui("text_upload")`
- `text_split_ui("text_split")`
- `text_management_ui("text_management")`
- `research_background_ui("research_background")`
- `mode_ui("mode")`
- `categories_ui("categories")`
- `score_ui("scoring")`
- `marking_codes_ui("marking_codes")`
- `llm_provider_ui("llm_provider")`
- `model_ui("model")`
- `context_window_ui("context_window")`
- `assign_multiple_categories_toggle_ui("assign_multiple_categories_toggle")`
- `interrater_toggle_ui("interrater_toggle")`
- `human_in_the_loop_toggle_ui("human_in_the_loop_toggle")`
- `write_paragraphs_toggle_ui("write_paragraphs_toggle")`
- `processing_ui("processing")` (progress + start/cancel + downloads; not a card)

## Proposed sections (v1)

This mapping matches your requested first/second sections, and keeps later steps reasonably sized.

1. **Texts**
   - `text_upload_ui("text_upload")`
   - `text_management_ui("text_management")`
   - `text_split_ui("text_split")`

2. **Research & Mode**
   - `research_background_ui("research_background")`
   - `mode_ui("mode")`

3. **Analysis Inputs**
   - `categories_ui("categories")` (only renders in “Categorisatie”)
   - `score_ui("scoring")` (only renders in “Scoren”)
   - `marking_codes_ui("marking_codes")` (only renders in “Markeren”)
   - `assign_multiple_categories_toggle_ui("assign_multiple_categories_toggle")` (only renders in “Categorisatie”/“Onderwerpextractie”)

4. **LLM & Limits**
   - `llm_provider_ui("llm_provider")`
   - `model_ui("model")`
   - `context_window_ui("context_window")`

5. **Run Options**
   - `interrater_toggle_ui("interrater_toggle")`
   - `human_in_the_loop_toggle_ui("human_in_the_loop_toggle")`
   - `write_paragraphs_toggle_ui("write_paragraphs_toggle")`

6. **Run & Results**
   - `processing_ui("processing")`

Notes:
- Even if a section contains modules that sometimes don’t render (because the mode differs), the section itself can remain; it will just show fewer cards.
- If you later feel 6 steps is too many, Steps 5+6 can be merged.

## Implementation approach (recommended)

### 1) Keep vertical layout unchanged (existing code path)

Keep the current `div(class = "card-container", ...)` listing as the `Vertical` mode output.

### 2) Add a new “sectioned view” UI wrapper

Create a small “layout” component that:

- Stores state:
  - `layout_view` = `"vertical"` / `"sections"`
  - `section_index` = `1..N`
- Renders either:
  - the current vertical stack, or
  - the sectioned/stepper UI.

Suggested location:
- Option A (minimal files): implement directly inside `R/module_core_main_ui_and_server.R`
- Option B (cleaner): add `R/component_layout_sections.R` with:
  - `layout_toggle_ui(ns, selected)`
  - `sections_view_ui(section_index, lang, ...)`
  - `sections_definitions(lang)` returning a list describing steps + their UI content

### 3) Stepper + progress indicator

**Stepper** (titles):
- Visual: a row of “pills” or small buttons (Bootstrap 5), with:
  - active step highlighted
  - completed steps optionally showing a check mark
- Interaction:
  - click step to jump (optional)
  - “Back/Next” buttons for sequential navigation

**Progress bar**:
- A small Bootstrap progress bar beneath the stepper:
  - `% = (section_index - 1) / (N - 1) * 100`

### 4) “Carousel-like” slide transition (optional but matches request)

Use CSS flex + transform:

- viewport hides overflow
- track is a horizontal flex row
- change `transform: translateX(-100% * (section_index - 1))`
- CSS transition provides left/right animation

No extra packages required.

### 5) Preserve state across re-render

`output$main_ui <- renderUI({ ... })` depends on `lang()` and will re-render when language changes.

To avoid resetting the layout/step when UI re-renders:
- keep canonical state in server-side `reactiveVal()`s
- use those `reactiveVal()`s as `selected = ...` when rendering the layout toggle
- update the `reactiveVal()`s from `input$...` via `observeEvent(..., ignoreInit = TRUE)`

### 6) Scroll behaviour

When the user changes section:
- scroll back to top smoothly so the new section starts “at the right place”.

Implementation: `shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")` inside the `observeEvent()` that changes `section_index`.

## Concrete code sketch (pseudo)

### Server state

```r
layout_view <- reactiveVal("vertical")
section_index <- reactiveVal(1)

observeEvent(input$layout_view, {
  layout_view(input$layout_view)
  if (identical(input$layout_view, "sections")) section_index(1)
}, ignoreInit = TRUE)

observeEvent(input$next_section, {
  section_index(min(section_index() + 1, N))
  shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
})

observeEvent(input$prev_section, {
  section_index(max(section_index() - 1, 1))
  shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
})
```

### UI wrapper (inside `output$main_ui`)

```r
uiOutput("layout_stack")
```

```r
output$layout_stack <- renderUI({
  if (layout_view() == "vertical") {
    div(class = "card-container", ...)
  } else {
    sections_view_ui(section_index())
  }
})
```

### Sectioned view UI (sketch)

```r
div(
  class = "kwallm-sections",
  stepper_ui(section_index(), N, titles = ...),
  div(
    class = "kwallm-sections-viewport",
    div(
      class = "kwallm-sections-track",
      style = sprintf("transform: translateX(-%s%%);", (section_index()-1)*100),
      div(class="kwallm-section", step_1_ui),
      div(class="kwallm-section", step_2_ui),
      ...
    )
  ),
  nav_buttons_ui()
)
```

## Styling changes

Add to `R/style_css_js.R`:

- Stepper
  - `.kwallm-stepper` container (flex row, wrap on small screens)
  - `.kwallm-step` base, `.kwallm-step--active`, `.kwallm-step--done`
- Carousel
  - `.kwallm-sections-viewport { overflow: hidden; }`
  - `.kwallm-sections-track { display: flex; transition: transform 250ms ease; }`
  - `.kwallm-section { flex: 0 0 100%; }`

Ensure the inner content keeps the existing max-width:
- `.kwallm-section .card-container { max-width: 1000px; margin: 0 auto; }`

## Rollout steps (implementation checklist)

1. Add layout toggle UI (Vertical/Sections) to `R/module_core_main_ui_and_server.R`.
2. Refactor the current card list into a `vertical_view_ui()` helper (no functional change).
3. Implement `sections_view_ui()` that groups existing module UIs into the 6 steps above.
4. Add `section_index` state + Back/Next handlers, plus optional clickable stepper.
5. Add CSS in `R/style_css_js.R` for stepper + slide transition.
6. Manual QA:
   - switch Vertical ↔ Sections without breaking existing inputs
   - verify language toggle doesn’t reset layout unexpectedly
   - verify each mode shows the right “Analysis Inputs” cards
   - ensure processing still works and progress remains visible in Run/Results step

