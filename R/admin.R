# Dit is een placeholder admin-module. Deze zou kunnen worden uitgebreid om binnen de app gebruikersbeheer te doen,
#   of bijvoorbeeld API-keys/tokens toe te kennen aan specifieke gebruikers binnen de app
# Deze module heeft geen functionaliteit; indien gewenst kan dit worden ingericht
#   door organisaties die de app deployen

#### 1 Functions ####

admin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns("admin_icon")),
    uiOutput(ns("admin_modal"))
  )
}

admin_server <- function(
  id,
  user_info = list(mail = "admin@example.com"),
  admin_emails = c("admin@example.com")
) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Check if user is admin
      is_admin <- reactive({
        !is.null(user_info$mail) && user_info$mail %in% admin_emails
      })

      # Render icon with admin status
      output$admin_icon <- renderUI({
        logged_in <- !is.null(user_info$mail) && nzchar(user_info$mail)
        admin_status <- is_admin()

        tagList(
          tags$style(HTML(
            "
            .icon-wrapper {
              position: relative;
              display: inline-block;
              width: 60px;
              height: 60px;
            }
            .icon {
              font-size: 2.5rem;
              color: #343a40; /* Dark gray */
              position: absolute;
              top: 50%;
              left: 50%;
              transform: translate(-50%, -50%);
              z-index: 2;
              /* NO transition on color anymore */
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
            .ring.admin {
              border-top-color: #28a745; /* Green */
            }
            .ring.not-admin {
              border-top-color: #dc3545; /* Red */
            }
            .icon-wrapper:hover .ring {
              opacity: 1;
              animation: spin 1s linear infinite;
            }
            @keyframes spin {
              from { transform: rotate(0deg); }
              to { transform: rotate(360deg); }
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
            "
          )),
          div(
            class = "d-flex justify-content-center gap-3",
            div(
              class = "icon-wrapper",
              div(
                class = paste(
                  "ring",
                  if (admin_status) "admin" else "not-admin"
                )
              ),
              actionLink(
                ns("admin_icon"),
                label = bsicons::bs_icon("shield-lock"), # always shield-lock
                class = "clickable-icon icon"
              ),
              div(
                class = paste(
                  "checkmark",
                  if (admin_status) "green" else "red"
                ),
                HTML(
                  if (admin_status) {
                    "&check;" # ✓ when admin
                  } else {
                    "&times;" # × when not admin
                  }
                )
              )
            ) |>
              bslib::tooltip(
                HTML(
                  if (logged_in && admin_status) {
                    "Admin"
                  } else {
                    "Niet admin"
                  }
                ),
                placement = "bottom"
              )
          )
        )
      })

      # Show modal when admin icon is clicked
      observeEvent(input$admin_icon, {
        if (is_admin()) {
          showModal(modalDialog(
            title = "Adminportaal",
            easyClose = TRUE
          ))
        }
      })

      # Render admin modal (functionality not implemented)
      output$admin_modal <- renderUI({
        req(is_admin())
        NULL
      })
    }
  )
}


#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(bslib)

  ui <- bslib::page_fluid(
    useShinyjs(),
    admin_ui("user_module")
  )

  server <- function(input, output, session) {
    admin_server("user_module")
  }

  shinyApp(ui, server)
}
