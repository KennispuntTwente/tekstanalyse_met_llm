# Module for showing logos for Kennispunt Twente and GitHub

#### 1 UI ####

kpt_logo_ui <- function(
  id,
  logo_src = "www/kennispunttwente_avatar.png"
) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(
        "
        @keyframes fadeIn {
          from { opacity: 0; transform: scale(0.9); }
          to { opacity: 1; transform: scale(1); }
        }

        .logo-wrapper {
          position: relative;
          display: inline-block;
          width: 40px;
          height: 40px;
          opacity: 0;
          animation: fadeIn 1s cubic-bezier(0.645, 0.045, 0.355, 1) 1;
          animation-fill-mode: forwards;
          transition: box-shadow 0.3s ease;
        }

        .logo-wrapper:hover {
          transform: translateY(-1px)
        }

        .logo-img {
          width: 100%;
          height: 100%;
          object-fit: contain;
          position: absolute;
          top: 0;
          left: 0;
          z-index: 1;
        }
        "
      ))
    ),
    tags$div(
      class = "d-flex justify-content-center gap-3",
      tags$a(
        href = "https://kennispunttwente.nl",
        target = "_blank",
        class = "logo-wrapper",
        img(
          src = logo_src,
          class = "logo-img",
          alt = "Kennispunt Twente (avatar)"
        ) |>
          bslib::tooltip(
            "Kennispunt Twente"
          )
      )
    )
  )
}

github_logo_ui <- function(
  id,
  logo_src = "www/github_avatar.png",
  repo_url = "https://github.com/tjarkvandemerwe/llm_open_antwoorden"
) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML(
        "
        @keyframes fadeIn {
          from { opacity: 0; transform: scale(0.9); }
          to { opacity: 1; transform: scale(1); }
        }

        .logo-wrapper {
          position: relative;
          display: inline-block;
          width: 40px;
          height: 40px;
          opacity: 0;
          animation: fadeIn 1s cubic-bezier(0.645, 0.045, 0.355, 1) 1;
          animation-fill-mode: forwards;
          transition: box-shadow 0.3s ease;
        }

        .logo-wrapper:hover {
          transform: translateY(-1px);
        }

        .logo-img {
          width: 100%;
          height: 100%;
          object-fit: contain;
          position: absolute;
          top: 0;
          left: 0;
          z-index: 1;
        }
        "
      ))
    ),
    tags$div(
      class = "d-flex justify-content-center gap-3",
      tags$a(
        href = repo_url,
        target = "_blank",
        class = "logo-wrapper",
        img(src = logo_src, class = "logo-img", alt = "GitHub (avatar)") |>
          bslib::tooltip("GitHub-repository")
      )
    )
  )
}

#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  shiny::addResourcePath("www", "www")

  ui <- bslib::page_fluid(
    useShinyjs(),
    kpt_logo_ui("logo1"),
    github_logo_ui("logo2")
  )

  shinyApp(ui, server)
}
