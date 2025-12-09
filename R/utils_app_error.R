# Function handle errors in the Shiny app
# Logs the error message with a timestamp
# Shows details about the error message in the modal, and stops the app if it's considered a
#   fatal error
# Also shows contact details to report the error

#### 1 Functions ####

app_error <- function(
  error,
  when = "unknown",
  fatal = FALSE,
  shiny_session = shiny::getDefaultReactiveDomain(),
  admin_name = getOption("app_admin_name", NULL),
  admin_email = getOption("app_admin_email", NULL),
  github_repo = "https://github.com/KennispuntTwente/KWALLM",
  lang = shiny.i18n::Translator$new(
    translation_json_path = "language/language.json"
  )
) {
  # Downgrade IPC error which occurs after interrupting process
  if (
    isTRUE(grepl(
      "Cannot pop from destroyed TextFileSource",
      try(conditionMessage(error), silent = TRUE)
    ))
  ) {
    fatal <- FALSE
  }

  error <- capture.output(print(error)) |> paste0(collapse = "\n")

  current_time <- Sys.time()
  formatted_time <- format(current_time, "%Y-%m-%d %H:%M:%S")
  log_message <- paste0(
    "Error: ",
    error,
    "\n",
    "When: ",
    when,
    "\n",
    "Time: ",
    formatted_time,
    "\n"
  )
  print(log_message)

  # Define subdirectory based on error type
  log_dir <- if (fatal) "app_errors/fatal" else "app_errors/nonfatal"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_file <- file.path(
    log_dir,
    paste0("error_", format(current_time, "%Y%m%d_%H%M%S"), ".log")
  )
  write(log_message, file = log_file, append = TRUE)

  if (is.null(shiny_session)) {
    stop(error)
  }

  if (fatal) {
    removeModal()

    body_encoded <- URLencode(paste0(
      lang$t("Ik kreeg zojuist deze foutmelding:"),
      "\n\n",
      log_message
    ))

    # Fallback if admin contact info is missing
    contact_info <- if (!is.null(admin_name) && !is.null(admin_email)) {
      email_subject <- URLencode(paste0(
        lang$t("Tekstanalyse-app-foutmelding: "),
        stringr::str_trunc(error, 50, ellipsis = "...")
      ))
      mailto_link <- paste0(
        "mailto:",
        admin_email,
        "?subject=",
        email_subject,
        "&body=",
        body_encoded
      )
      tagList(
        p(paste(
          lang$t("Neem contact op met"),
          admin_name,
          lang$t("als je deze foutmelding blijft zien.")
        )),
        p(tags$a(
          href = mailto_link,
          lang$t("Klik hier om een e-mail te sturen met de foutmelding."),
          target = "_blank"
        ))
      )
    } else {
      github_issue_link <- paste0(
        github_repo,
        "/issues/new?labels=bug&title=",
        URLencode(paste0(
          lang$t("Foutmelding: "),
          stringr::str_trunc(error, 50)
        )),
        "&body=",
        body_encoded
      )
      tagList(
        p(tags$a(
          href = github_issue_link,
          lang$t(
            "Klik hier deze foutmelding te rapporteren als GitHub issue. Bedankt!"
          ),
          target = "_blank"
        ))
      )
    }

    showModal(modalDialog(
      title = "Error",
      tagList(
        p(lang$t(
          "Er gebeurde iets onverwachts, waardoor de app is gestopt. Sorry!"
        )),
        hr(),
        pre(log_message),
        hr(),
        contact_info
      ),
      easyClose = FALSE,
      footer = NULL,
      size = "l"
    ))

    shiny_session$close()
  } else {
    showNotification(
      paste("Error:", error),
      type = "error",
      duration = NULL
    )
  }
}


#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)

  ui <- bslib::page(
    useShinyjs(),
    actionButton("trigger_error", "Trigger Error"),
    actionButton("trigger_fatal_error", "Trigger Fatal Error"),
    actionButton(
      "trigger_fatal_error_unexpected",
      "Trigger Unexpected Fatal Error"
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$trigger_error, {
      app_error("This is a non-fatal error.")
    })

    observeEvent(input$trigger_fatal_error, {
      app_error("This is a fatal error.", fatal = TRUE)
    })

    # Create unexpected error
    observeEvent(input$trigger_fatal_error_unexpected, {
      stop("This is an unexpected error.")
    })
  }

  app <- shinyApp(ui, server)
  shiny::runApp(app)
}
