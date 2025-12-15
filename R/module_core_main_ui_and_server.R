# This script defines the main UI + server for the Shiny app,
#   as well as the processing UI + server which is part of it
# Main UI + server combines the various modules into a single app
#   It combines the text upload, mode selection, model selection,
#   category/score input, and processing UI + server
# Processing UI + server is responsible for executing the chosen qualitative
#   analysis, updating about progress, making results availlable for download,
#   and stopping the app when done
# To run, see 'app.R'

## Main UI -----------------------------------------------------------
main_ui <- function() {
  bslib::page(
    theme = bs_theme(
      version = 5,
      bootswatch = "lux"
    ),
    shinyjs::useShinyjs(),
    css_js_head(),
    uiOutput("main_ui")
  )
}


# Main server ------------------------------------------------------
# Here we build the main server for the Shiny app

main_server <- function(
  preconfigured_main_models = NULL,
  preconfigured_large_models = NULL,
  azure_auth = FALSE,
  gliner_model = NULL
) {
  server <- function(input, output, session) {
    # Layout state -----------------------------------------------------------

    n_sections <- 5L
    current_section <- reactiveVal(1L)

    show_all_sections <- function() {
      for (i in seq_len(n_sections)) {
        shinyjs::show(paste0("kwallm_section_", i), anim = FALSE)
      }
    }

    show_single_section <- function(i, direction = c("none", "left", "right")) {
      direction <- match.arg(direction)

      for (j in seq_len(n_sections)) {
        id <- paste0("kwallm_section_", j)
        if (identical(j, i)) {
          shinyjs::show(id, anim = FALSE)
        } else {
          shinyjs::hide(id, anim = FALSE)
        }
      }

      if (!identical(direction, "none")) {
        anim_class <- if (direction == "left") {
          "kwallm-slide-in-left"
        } else {
          "kwallm-slide-in-right"
        }

        shinyjs::runjs(sprintf(
          paste0(
            "var el=document.getElementById('%s');",
            "if(el){",
            "el.classList.remove('kwallm-slide-in-left','kwallm-slide-in-right');",
            "void el.offsetWidth;",
            "el.classList.add('%s');",
            "}"
          ),
          paste0("kwallm_section_", i),
          anim_class
        ))
      }
    }

    update_section_nav_buttons <- function(i) {
      # Button visibility is now handled client-side in JavaScript for instant updates
      # See style_css_js.R kwallmUpdateNavButtons()
    }

    # Progress UI (only shown in "sections" mode) ----------------------------

    output$kwallm_sections_progress <- renderUI({
      req(lang())
      cur <- current_section()
      pct <- if (n_sections <= 1L) {
        100L
      } else {
        round((cur - 1L) / (n_sections - 1L) * 100L)
      }

      div(
        class = "kwallm-sections-progress",
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-1",
          tags$small(
            class = "text-muted",
            paste0(lang()$t("Sectie"), " ", cur, "/", n_sections)
          )
        ),
        div(
          class = "progress",
          div(
            class = "progress-bar",
            role = "progressbar",
            style = sprintf("width: %s%%;", pct),
            `aria-valuenow` = pct,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100
          )
        )
      )
    })

    # Layout toggle behaviour ------------------------------------------------

    observeEvent(input$kwallm_layout_view, {
      view <- input$kwallm_layout_view
      if (is.null(view) || !view %in% c("vertical", "sections")) {
        return()
      }

      if (identical(view, "vertical")) {
        shinyjs::hide("kwallm_sections_nav", anim = FALSE)
        show_all_sections()
        return()
      }

      # sections view
      shinyjs::show("kwallm_sections_nav", anim = FALSE)

      cur <- current_section()
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "kwallm_sections_step",
        selected = as.character(cur)
      )

      show_single_section(cur, direction = "none")
      update_section_nav_buttons(cur)
    }, ignoreInit = TRUE)

    # Section navigation behaviour ------------------------------------------

    observeEvent(input$kwallm_sections_step, {
      new <- suppressWarnings(as.integer(input$kwallm_sections_step))
      if (is.na(new) || new < 1L || new > n_sections) {
        return()
      }

      old <- current_section()
      current_section(new)

      if (!identical(input$kwallm_layout_view, "sections")) {
        return()
      }

      direction <- if (new > old) {
        "right"
      } else if (new < old) {
        "left"
      } else {
        "none"
      }

      show_single_section(new, direction = direction)
      update_section_nav_buttons(new)
    }, ignoreInit = TRUE)

    observeEvent(input$kwallm_sections_prev, {
      if (!identical(input$kwallm_layout_view, "sections")) {
        return()
      }
      cur <- current_section()
      if (cur <= 1L) {
        return()
      }
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "kwallm_sections_step",
        selected = as.character(cur - 1L)
      )
    }, ignoreInit = TRUE)

    observeEvent(input$kwallm_sections_next, {
      if (!identical(input$kwallm_layout_view, "sections")) {
        return()
      }
      cur <- current_section()
      if (cur >= n_sections) {
        return()
      }
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "kwallm_sections_step",
        selected = as.character(cur + 1L)
      )
    }, ignoreInit = TRUE)

    # Go to processing section button handler
    observeEvent(input$kwallm_goto_processing, {
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "kwallm_sections_step",
        selected = as.character(n_sections)
      )
    }, ignoreInit = TRUE)

    # UI ---------------------------------------------------------------
    output$main_ui <- renderUI({
      base_ui <- tagList(
        # Main header area with user/admin UI and title
        div(
          style = "margin-left: 0.5rem; margin-right: 0.5rem;",
          div(
            style = "
        max-width: 1000px;
        margin: 0 auto;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 20px 0;
        /* reserve space for 60px icon */
        min-height: 100px;
      ",

            # Left: user UI
            div(
              id = "kpt_logo",
              kpt_logo_ui("kpt_logo")
            ),

            # Middle: title
            div(
              id = "title",
              div(
                style = "display: flex; justify-content: center; align-items: center; gap: 10px;",
                img(
                  src = "www/logo.png",
                  style = "width: 3rem; height: 3rem"
                ),
                h1(
                  style = "margin: 0; text-align: center;",
                  strong("KWALLM")
                ),
                img(
                  src = "www/logo.png",
                  style = "width: 3rem; height: 3rem; transform: scaleX(-1);"
                )
              ),
              tags$hr(
                style = "border: none; border-top: 1px solid #ccc; width: 60%; margin: 10px auto;"
              ),
              h2(
                style = "margin: 0; text-align: center;",
                span(class = "title-part1", lang()$t("Tekstanalyse")),
                span(class = "title-break", " "), # this will turn into a line break on small screens
                span(class = "title-part2", lang()$t("met LLM"))
              )
            ),

            # Right: GitHub logo link
            div(
              id = "github_logo",
              github_logo_ui("github_logo")
            )
          ),

          # Info box under the title
          hr(),
          div(
            class = "d-flex justify-content-center align-items-center gap-3 flex-wrap",
            language_ui("language"),
            div(
              id = "kwallm_layout_controls",
              shinyWidgets::radioGroupButtons(
                inputId = "kwallm_layout_view",
                label = NULL,
                choices = stats::setNames(
                  c("vertical", "sections"),
                  c(
                    lang()$t("Verticaal"),
                    lang()$t("Secties")
                  )
                ),
                selected = if (isTRUE(getOption("shiny.testmode"))) "vertical" else "sections",
                size = "sm"
              )
            )
          ),
          div(
            style = "max-width: 1000px; margin: 10px auto 0 auto;",
            accordion(
              id = "info-accordion",
              open = FALSE,
              accordion_panel(
                title = lang()$t("Over deze app"),
                div(
                  style = "text-align: center;",
                  p(
                    HTML(paste0(
                      lang()$t(
                        "Deze app is ontwikkeld door <a href='https://www.kennispunttwente.nl' target='_blank'>Kennispunt Twente</a>,"
                      ),
                      lang()$t(
                        " voortkomend uit een samenwerkingstraject van de Kennispunt Twente en GGD Twente."
                      ),
                      lang()$t(
                        " Samen werken onze organisaties toepassingen met generatieve AI ten behoeve van de samenleving."
                      )
                    ))
                  ),
                  p(
                    HTML(paste0(
                      lang()$t(
                        "Kennispunt Twente is een non-profit organisatie voor data, inzicht, en kennis."
                      ),
                      lang()$t(" Ideeën of verbeterpunten voor de app? "),
                      lang()$t(
                        "<a href='https://github.com/kennispunttwente/tekstanalyse_met_llm/issues/new' target='_blank'>Open een issue in de GitHub-repository</a>."
                      ),
                      lang()$t(
                        " Geïnteresseerd in wat Kennispunt Twente voor jouw organisatie kan doen, bijvoorbeeld op gebied van generatieve AI?"
                      ),
                      lang()$t(
                        " Bezoek <a href='https://www.kennispunttwente.nl' target='_blank'>onze website</a>"
                      ),
                      lang()$t(
                        " of <a href=\"mailto:l.koning@kennispunttwente.nl,t.vandemerwe@kennispunttwente.nl?cc=info@kennispunttwente.nl\" target=\"_blank\">neem contact op met onze ontwikkelaars</a>."
                      )
                    ))
                  )
                )
              )
            )
          ),

          hr(),
          uiOutput("azure_auth_unauthorized_ui"),

          div(
            class = "card-container",
            div(
              id = "kwallm_sections_nav",
              class = "kwallm-sections-nav",
              style = "display: none;",
              shinyWidgets::radioGroupButtons(
                inputId = "kwallm_sections_step",
                label = NULL,
                choices = stats::setNames(
                  as.character(seq_len(n_sections)),
                  c(
                    paste0("1. ", lang()$t("Teksten")),
                    paste0("2. ", lang()$t("Onderzoek & modus")),
                    paste0("3. ", lang()$t("Analyse")),
                    paste0("4. ", lang()$t("LLM & context")),
                    paste0("5. ", lang()$t("Uitvoeren"))
                  )
                ),
                selected = "1",
                size = "sm",
                justified = TRUE
              ),
              uiOutput("kwallm_sections_progress"),
              div(
                class = "d-flex justify-content-between gap-2 mt-2",
                actionButton(
                  "kwallm_sections_prev",
                  lang()$t("Terug"),
                  class = "btn btn-outline-secondary btn-sm"
                ),
                actionButton(
                  "kwallm_sections_next",
                  lang()$t("Volgende"),
                  class = "btn btn-primary btn-sm"
                )
              ),
              # Floating processing status (visible when processing and not on section 5)
              div(
                id = "kwallm_floating_processing",
                class = "kwallm-floating-processing",
                style = "display: none;",
                tags$hr(style = "margin: 0.75rem 0;"),
                div(
                  class = "d-flex align-items-center gap-2",
                  div(
                    class = "flex-grow-1",
                    div(
                      id = "kwallm_floating_progress_bar",
                      class = "progress",
                      style = "height: 0.5rem;",
                      div(
                        id = "kwallm_floating_progress_fill",
                        class = "progress-bar",
                        role = "progressbar",
                        style = "width: 0%;",
                        `aria-valuenow` = "0",
                        `aria-valuemin` = "0",
                        `aria-valuemax` = "100"
                      )
                    )
                  ),
                  actionButton(
                    "kwallm_goto_processing",
                    lang()$t("Bekijk"),
                    class = "btn btn-outline-primary btn-sm"
                  )
                )
              )
            ),
            div(
              id = "kwallm_section_1",
              class = "kwallm-section",
              text_upload_ui("text_upload"),
              text_split_ui("text_split"),
              text_management_ui("text_management")
            ),
            div(
              id = "kwallm_section_2",
              class = "kwallm-section",
              research_background_ui("research_background"),
              mode_ui("mode")
            ),
            div(
              id = "kwallm_section_3",
              class = "kwallm-section",
              categories_ui("categories"),
              assign_multiple_categories_toggle_ui(
                "assign_multiple_categories_toggle"
              ),
              score_ui("scoring"),
              marking_codes_ui("marking_codes")
            ),
            div(
              id = "kwallm_section_4",
              class = "kwallm-section",
              llm_provider_ui("llm_provider"),
              model_ui("model"),
              context_window_ui("context_window")
            ),
            div(
              id = "kwallm_section_5",
              class = "kwallm-section",
              interrater_toggle_ui("interrater_toggle"),
              human_in_the_loop_toggle_ui("human_in_the_loop_toggle"),
              write_paragraphs_toggle_ui("write_paragraphs_toggle"),
              processing_ui("processing")
            ),

            div(style = "height: 75px;"),
          ),

          hr()
        ),

        # Footer
        div(
          style = "
            text-align: center;
            padding: 20px 0;
            background-color: #f8f9fa;
          ",
          a(
            href = "https://www.kennispunttwente.nl",
            target = "_blank",
            img(
              src = "www/kennispunttwente_logo.svg",
              alt = "Kennispunt Twente (logo)",
              style = "max-height: 60px;"
            )
          )
        )
      )
    })

    # 0 Authentication -----------------------------------------------
    # When deploying to server, you could implement, e.g.,
    #   Azure AD authentication here
    # See for example R/azure_auth.R

    if (azure_auth) {
      user_info <- get_azure_auth(session, output)
      if (is.null(user_info)) return()
    }

    # 1 Text management ----------------------------------------------
    # Text upload
    raw_texts <- text_upload_server("text_upload", processing, lang)

    # Split texts
    split_texts <- text_split_server(
      "text_split",
      processing = processing,
      raw_texts = raw_texts,
      lang = lang
    )

    # Pre-process texts, show table
    texts <- text_management_server(
      id = "text_management",
      processing = processing,
      raw_texts = split_texts,
      lang = lang,
      gliner_model = gliner_model
    )

    # Obtain research background
    research_background <- research_background_server(
      "research_background",
      processing = processing,
      lang = lang
    )

    # Manage context window, chunking
    context_window <- context_window_server(
      "context_window",
      mode = mode,
      models = models,
      categories = categories,
      scoring_characteristic = scoring_characteristic,
      codes = marking_codes,
      research_background = research_background,
      assign_multiple_categories = assign_multiple_categories_toggle,
      texts = texts,
      processing = processing,
      lang = lang
    )

    # 2 Mode management ----------------------------------------------
    # Obtain mode
    mode <- mode_server("mode", processing, lang)

    # Obtain toggle for assigning multiple categories
    assign_multiple_categories_toggle <- assign_multiple_categories_toggle_server(
      "assign_multiple_categories_toggle",
      processing,
      mode,
      lang
    )

    write_paragraphs_result <- write_paragraphs_toggle_server(
      "write_paragraphs_toggle",
      processing,
      mode,
      lang
    )

    # Extract both values from the result
    write_paragraphs_toggle <- write_paragraphs_result$write_paragraphs
    style_prompt <- write_paragraphs_result$style_prompt

    # Obtain toggle for interrater reliability
    interrater_reliability_toggle <- interrater_toggle_server(
      "interrater_toggle",
      processing = processing,
      mode = mode,
      lang = lang
    )

    # Obtain toggle for human-in-the-loop
    human_in_the_loop_toggle <- human_in_the_loop_toggle_server(
      "human_in_the_loop_toggle",
      processing,
      mode,
      lang
    )

    # 3 Model management ---------------------------------------------
    # Determine if we have preconfigured LLM providers or not
    # Are both preconfigured_llm_provider and preconfigured_main_models provided?
    has_preconfigured_llm_provider <- if (
      length(preconfigured_main_models) > 0 &&
        length(preconfigured_large_models) > 0
    ) {
      TRUE
    } else {
      FALSE
    }

    llm_provider_rv <- llm_provider_server(
      "llm_provider",
      processing = processing,
      has_preconfigured_llm_provider = has_preconfigured_llm_provider,
      lang = lang
    )

    models <- model_server(
      "model",
      processing = processing,
      mode = mode,
      llm_provider_rv = llm_provider_rv,
      lang = lang,
      preconfigured_llm_provider_model_main = preconfigured_main_models,
      preconfigured_llm_provider_model_large = preconfigured_large_models
    )

    # 4 Category & score fields --------------------------------------
    categories <- categories_server(
      "categories",
      mode = mode,
      processing = processing,
      lang = lang,
      assign_multiple_categories = assign_multiple_categories_toggle
    )
    scoring_characteristic <- score_server("scoring", mode, processing, lang)

    marking_codes <- marking_codes_server(
      id = "marking_codes",
      mode = mode,
      processing = processing,
      texts = texts,
      research_background = research_background,
      context_window = context_window,
      llm_provider_rv = llm_provider_rv,
      models = models,
      lang = lang
    )

    # 5 Processing ---------------------------------------------------
    processing <- processing_server(
      id = "processing",
      mode = mode,
      interrater_reliability_toggle = interrater_reliability_toggle,
      texts = texts,
      llm_provider_rv = llm_provider_rv,
      models = models,
      categories = categories,
      scoring_characteristic = scoring_characteristic,
      codes = marking_codes,
      research_background = research_background,
      style_prompt = style_prompt,
      human_in_the_loop = human_in_the_loop_toggle,
      assign_multiple_categories = assign_multiple_categories_toggle,
      write_paragraphs = write_paragraphs_toggle,
      context_window = context_window,
      lang = lang
    )

    # Show/hide floating processing panel based on processing state and section
    observe({
      is_processing <- isTRUE(processing())
      is_sections_mode <- identical(input$kwallm_layout_view, "sections")
      not_on_last_section <- current_section() < n_sections

      show_floating <- is_processing && is_sections_mode && not_on_last_section

      session$sendCustomMessage("kwallm_floating_processing", list(show = show_floating))
    })

    # 6 Language -----------------------------------------------------
    lang <- language_server("language", processing)
  }

  return(server)
}
