# This script defines the main UI + server for the Shiny app,
#   as well as the processing UI + server which is part of it
# Main UI + server combines the various modules into a single app
#   It combines the text upload, mode selection, model selection,
#   category/score input, and processing UI + server
# Processing UI + server is responsible for executing the chosen qualitative
#   analysis, updating about progress, making results availlable for download,
#   and stopping the app when done
# To run, see 'app.R'

##### Main UI #####

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


#### Main server ####

# Here we build the main server for the Shiny app

main_server <- function(
  preconfigured_llm_provider = NULL,
  preconfigured_main_models = NULL,
  preconfigured_large_models = NULL,
  azure_auth = FALSE
) {
  server <- function(input, output, session) {
    #### UI ####

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
              h1(
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
          language_ui("language"),
          div(
            style = "
        max-width: 800px;
        margin: 10px auto 0 auto;
        padding: 15px 20px;
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        font-size: 0.9em;
        color: #495057;
        text-align: center;
      ",
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
            ),
            accordion(
              id = "team-accordion",
              open = FALSE,
              accordion_panel(
                lang()$t("Bekijk contactinformatie"),
                div(
                  style = "text-align: left;",
                  tags$ul(
                    tags$li(
                      HTML(
                        "Luka Koning (<a href='mailto:l.koning@kennispunttwente.nl'>l.koning@kennispunttwente.nl</a>)"
                      )
                    ),
                    tags$li(
                      HTML(
                        "Tjark van de Merwe (<a href='mailto:t.vandemerwe@kennispunttwente.nl'>t.vandemerwe@kennispunttwente.nl</a>)"
                      )
                    )
                  )
                )
              )
            )
          ),

          hr(),
          uiOutput("azure_auth_unauthorized_ui"),

          div(
            class = "card-container",

            text_upload_ui("text_upload"),
            text_management_ui("text_management"),
            research_background_ui("research_background"),
            mode_ui("mode"),
            categories_ui("categories"),
            score_ui("scoring"),
            llm_provider_ui("llm_provider"),
            model_ui("model"),
            context_window_ui("context_window"),
            assign_multiple_categories_toggle_ui(
              "assign_multiple_categories_toggle"
            ),
            interrater_toggle_ui("interrater_toggle"),
            human_in_the_loop_toggle_ui("human_in_the_loop_toggle"),
            write_paragraphs_toggle_ui("write_paragraphs_toggle"),
            processing_ui("processing"),

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

    #### 0 Authentication ####

    # When deploying to server, you could implement, e.g.,
    #   Azure AD authentication here
    # See for example R/azure_auth.R

    if (azure_auth) {
      user_info <- get_azure_auth(session, output)
      if (is.null(user_info)) return()
    }

    #### 1 Text management ####

    # Text upload
    raw_texts <- text_upload_server("text_upload", processing, lang)

    # Pre-process texts, show table
    texts <- text_management_server(
      "text_management",
      processing = processing,
      raw_texts = raw_texts,
      lang = lang
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
      research_background = research_background,
      assign_multiple_categories = assign_multiple_categories_toggle,
      texts = texts,
      processing = processing,
      lang = lang
    )

    #### 2 Mode management ####

    # Obtain mode
    mode <- mode_server("mode", processing, lang)

    # Obtain toggle for assigning multiple categories
    assign_multiple_categories_toggle <- assign_multiple_categories_toggle_server(
      "assign_multiple_categories_toggle",
      processing,
      mode,
      lang
    )

    write_paragraphs_toggle <- write_paragraphs_toggle_server(
      "write_paragraphs_toggle",
      processing,
      mode,
      lang
    )

    # Obtain toggle for interrater reliability
    interrater_reliability_toggle <- interrater_toggle_server(
      "interrater_toggle",
      processing,
      lang
    )

    # Obtain toggle for human-in-the-loop
    human_in_the_loop_toggle <- human_in_the_loop_toggle_server(
      "human_in_the_loop_toggle",
      processing,
      mode,
      lang
    )

    #### 3 Model management ####

    llm_provider_rv <- llm_provider_server(
      "llm_provider",
      processing = processing,
      preconfigured_llm_provider = preconfigured_llm_provider,
      preconfigured_main_models = preconfigured_main_models,
      preconfigured_large_models = preconfigured_large_models,
      lang = lang
    )

    models <- model_server(
      "model",
      processing = processing,
      mode = mode,
      llm_provider_rv = llm_provider_rv,
      lang = lang
    )

    #### 4 Category & score fields ####

    categories <- categories_server("categories", mode, processing, lang)
    scoring_characteristic <- score_server("scoring", mode, processing, lang)

    #### 5 Processing ####

    processing <- processing_server(
      "processing",
      mode,
      interrater_reliability_toggle,
      texts,
      llm_provider_rv = llm_provider_rv,
      models,
      categories,
      scoring_characteristic,
      research_background,
      human_in_the_loop = human_in_the_loop_toggle,
      assign_multiple_categories = assign_multiple_categories_toggle,
      write_paragraphs = write_paragraphs_toggle,
      context_window = context_window,
      lang = lang
    )

    #### 6 Language ####

    lang <- language_server("language", processing)
  }

  return(server)
}
