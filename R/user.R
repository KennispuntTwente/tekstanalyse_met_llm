# Dit is een placeholder-module die gebruikt zou kunnen worden om
#   info te tonen over de ingelogde gebruiker.
# Daarvoor moet de deployende organisatie de app configureren
#   met authenticatie (zie bijvoorbeeld: R/azure_auth.R)
# Deze module kan een icoon tonen met de login-status van de gebruiker,
#   en bijvoorbeeld uitgebreid worden om een modal te tonen met details
#   over het account van de gebruiker
# De module is daarmee niet essentieel voor het functioneren van de app
#   of het analyse-proces, maar meer optie voor mogelijke uitbreiding
#   bij het deployen in een organisatie

#### 1 UI ####

user_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Combined CSS: animations, styling
    tags$style(HTML(
      "
      /* Graceful fade-in with slight scale-up */
      @keyframes fadeIn {
        from {
          opacity: 0;
          transform: scale(0.9);
        }
        to {
          opacity: 1;
          transform: scale(1);
        }
      }

      /* Icon wrapper: initially hidden; animates over 1s with elegant easing */
      .icon-wrapper {
        position: relative;
        display: inline-block;
        width: 60px;
        height: 60px;

        /* start hidden */
        opacity: 0;
        /* 1s duration, smooth cubic-bezier easing */
        animation: fadeIn 1s cubic-bezier(0.645, 0.045, 0.355, 1) 1;
        animation-fill-mode: forwards;
      }

      .icon {
        font-size: 2.5rem;
        color: #343a40; /* Dark gray */
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 2;
      }

      .ring {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        border: 2px solid transparent;
        border-top: 2px solid;
        border-radius: 50%;
        opacity: 0;
        transform: rotate(0deg);
        transition: opacity 0.3s ease;
        z-index: 1;
      }
      .ring.logged-in {
        border-top-color: #28a745; /* Green */
      }
      .ring.not-logged-in {
        border-top-color: #dc3545; /* Red */
      }
      .icon-wrapper:hover .ring {
        opacity: 1;
        animation: spin 1s linear infinite;
      }
      @keyframes spin {
        from { transform: rotate(0deg); }
        to   { transform: rotate(360deg); }
      }

      .checkmark {
        position: absolute;
        top: -5px;
        right: -5px;
        border-radius: 50%;
        width: 18px;
        height: 18px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 0.75rem;
        opacity: 0;
        transition: opacity 0.3s ease;
        z-index: 3;
      }
      .checkmark.green {
        background-color: #28a745;
        color: white;
      }
      .checkmark.red {
        background-color: #dc3545;
        color: white;
      }
      .icon-wrapper:hover .checkmark {
        opacity: 1;
      }

      .tooltip .tooltip-inner {
        max-width: 300px;
        text-align: center;
      }
      "
    )),
    uiOutput(ns("user_icon"))
  )
}

#### 2 Server ####

user_server <- function(
  id,
  user_info = list(mail = "someone@example.com")
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$user_icon <- renderUI({
        is_logged_in <- !is.null(user_info$mail) && nzchar(user_info$mail)

        div(
          class = "d-flex justify-content-center gap-3",
          div(
            class = "icon-wrapper",
            div(
              class = paste(
                "ring",
                if (is_logged_in) "logged-in" else "not-logged-in"
              )
            ),
            div(
              class = "icon",
              bsicons::bs_icon("person-circle")
            ),
            div(
              class = paste(
                "checkmark",
                if (is_logged_in) "green" else "red"
              ),
              HTML(
                if (is_logged_in) {
                  "&check;" # ✓ if logged in
                } else {
                  "&times;" # × if not logged in
                }
              )
            )
          ) |>
            bslib::tooltip(
              HTML(
                paste0(
                  if (is_logged_in) {
                    paste0(
                      "Ingelogd als:<br>",
                      user_info$mail
                    )
                  } else {
                    "Niet ingelogd"
                  }
                )
              ),
              placement = "bottom"
            )
        )
      })
    }
  )
}


#### 3 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page_fluid(
    useShinyjs(),
    user_ui("user_module")
  )

  server <- function(input, output, session) {
    user_server("user_module")
  }

  shinyApp(ui, server)
}
