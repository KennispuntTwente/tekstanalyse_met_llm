# Dit script bevat enkele voorbeeld-functies om Azure Authenticatie toe te voegen aan een Shiny app
#   Gebaseerd op https://cran.r-project.org/web/packages/Microsoft365R/vignettes/shiny.html
# Hiermee zou bijvoorbeeld kunnen worden aangesloten op de Microsoft Entra ID(/Azure AD) van een
#   organisatie. Dit kan gebruikt worden om toegang tot de app te beperken
#   tot specifieke gebruikers
# Met aanpassingen aan deze functies zou dit ook kunnen worden gebruikt om aan te sluiten
#   op een andere OAuth2 provider, zoals Google of GitHub
# Het is aan een deployende organisatie om de app te configureren met de juiste
#   instellingen hierbij. Gebruik van deze functies is op eigen risico,
#   controleer altijd bij de informatiebeveiliging van de organisatie of wordt voldaan
#   aan de nodige vereisten voor authenticatie en autorisatie!
# Een alternatieve manier van authenticatie toevoegen zou kunnen zijn om een een reverse-proxy
#   toe te voegen die het verkeer naar de app beheert, bijv., OAuth2-proxy (https://oauth2-proxy.github.io/oauth2-proxy/)

#### 1 Functions ####

# Function to add Azure Auth to a shiny UI
#   Adds some JavaScript to UI, which will redirect the user to the Azure login page
# Note: requires cachem memory object 'memory_authentication' to be defined in the global environment,
#   this will hold authentication states. See example below
with_azure_auth <- function(
  ui,
  # SamenTwente (Kennispunt Twente) tenant ID; replace with your tenant ID:
  tenant = "9391afd1-7129-4938-9e4d-633c688f93c0",
  # Application ID for the Azure app registration; this is the Azure CLI app ID:
  app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
) {
  function(req) {
    opts <- parseQueryString(req$QUERY_STRING)

    if (is.null(opts$code)) {
      # Generate a random state string
      state <- paste0(
        sample(c(letters, 0:9), 256, replace = TRUE),
        collapse = ""
      )
      memory_authentication$set(state, state)

      # Build the Azure auth URI
      auth_uri <- AzureAuth::build_authorization_uri(
        resource = c("https://graph.microsoft.com/.default", "openid"),
        tenant = tenant,
        app = app,
        redirect_uri = "http://localhost:8100",
        version = 2,
        state = state
      )

      # Return JS redirect
      redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
      tags$script(HTML(redir_js))
    } else {
      ui
    }
  }
}

# Function to check Azure authentication in Shiny server
#   Checks the authentication state and retrieves user info
#     Will return NULL if the user is not authorized, and display an error message
#       under output$azure_auth_unauthorized_ui
#     Returns the user info if the user is authorized
# Note: requires cachem memory object 'memory_authentication' to be defined in the global environment,
#   this will hold authentication states. See example below
get_azure_auth <- function(
  session,
  output,
  # SamenTwente (Kennispunt Twente) tenant ID; replace with your tenant ID:
  tenant = "9391afd1-7129-4938-9e4d-633c688f93c0",
  # Application ID for the Azure app registration; this is the Azure CLI app ID:
  app = "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
  # E-mail address to contact if there is an error:
  admin_email = "admin@example.com"
) {
  print("getting Azure authentication...")

  shinyjs::runjs(
    "
    $(document).ready(function() {
      if (window.location.search.includes('code=')) {
        const newUrl = window.location.origin + window.location.pathname;
        window.history.replaceState({}, document.title, newUrl);
      }
    });
    "
  )

  opts <- parseQueryString(isolate(session$clientData$url_search))
  if (is.null(opts$code)) return(NULL)

  if (!memory_authentication$exists(opts$state)) {
    output$azure_auth_unauthorized_ui <- renderUI({
      div(
        style = "padding: 20px; text-align: center;",
        h2("Er is een fout opgetreden."),
        p(paste0(
          "Neem contact op met de beheerder (",
          admin_email,
          ")."
        ))
      )
    })
    print("State does not match")
    return(NULL)
  }

  user_info <- tryCatch(
    {
      token <- AzureAuth::get_azure_token(
        resource = c("https://graph.microsoft.com/.default", "openid"),
        tenant = tenant,
        app = app,
        auth_type = "authorization_code",
        authorize_args = list(redirect_uri = "http://localhost:8100"),
        version = 2,
        use_cache = FALSE,
        auth_code = opts$code
      )

      if (
        token$tenant != tenant ||
          token$client$client_id != app
      ) {
        stop("Unauthorized tenant/app")
      }

      httr::GET(
        "https://graph.microsoft.com/v1.0/me",
        httr::add_headers(
          Authorization = paste("Bearer", token$credentials$access_token)
        )
      ) |>
        httr::content(as = "parsed")
    },
    error = function(e) NULL
  )

  if (is.null(user_info)) {
    output$azure_auth_unauthorized_ui <- renderUI({
      div(
        style = "padding: 20px; text-align: center;",
        h2("Er is een fout opgetreden."),
        p(paste0(
          "Neem contact op met de beheerder (",
          admin_email,
          ")."
        ))
      )
    })
    print("Azure user info could not be retrieved")
    return(NULL)
  }

  email <- tolower(user_info$mail)
  user_info$mail <- email
  if (
    is.null(email) ||
      !is.character(email) ||
      email == ""
  ) {
    output$azure_auth_unauthorized_ui <- renderUI({
      div(
        style = "padding: 20px; text-align: center;",
        h2("Je bent niet geautoriseerd om deze applicatie te gebruiken."),
        p(paste0(
          "Neem contact op met de beheerder (",
          admin_email,
          ")."
        )),
        p(
          "Je bent ingelogd met het e-mailadres: ",
          email,
          a(
            "(klik hier om uit te loggen).",
            href = "https://login.microsoftonline.com/common/oauth2/logout",
            style = "color: #FF0033; text-decoration: underline;"
          )
        ),
        hr()
      )
    })
    return(NULL)
  }

  return(user_info)
}


#### 2 Example/development usage ####

if (FALSE) {
  library(shiny)
  library(shinyjs)
  library(AzureAuth)

  memory_authentication <- cachem::cache_mem(max_age = 300)

  ui <- with_azure_auth(
    ui = div(
      shinyjs::useShinyjs(),
      uiOutput("azure_auth_unauthorized_ui"),
      uiOutput("azure_auth_user_info_ui")
    )
  )

  server <- function(input, output, session) {
    user_info <- get_azure_auth(session, output)

    output$azure_auth_user_info_ui <- renderUI({
      req(user_info)
      div(
        style = "padding: 20px; text-align: center;",
        h2("Welkom!"),
        p(paste0("Je bent ingelogd als: ", user_info$mail))
      )
    })
  }

  shinyApp(ui, server)
}
