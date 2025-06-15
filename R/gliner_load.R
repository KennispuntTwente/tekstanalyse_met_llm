# Function to load the GLiNER model in a virtual environment;
#   installs Python, sets up venv, downloads model, loads model, tests model, returns model

# Allows to load Python & then interrupt R without fatal R crash:
Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

gliner_load_model <- function(
  venv_name = "portable-gliner-venv",
  python_version = "3.12.10",
  model_name = "urchade/gliner_multi_pii-v1",
  use_system_python = FALSE,
  docker_env = NULL,
  test_model = FALSE
) {
  stopifnot(
    is.character(venv_name),
    length(venv_name) == 1,
    is.character(python_version),
    length(python_version) == 1,
    is.character(model_name),
    length(model_name) == 1,
    is.logical(use_system_python),
    length(use_system_python) == 1,
    is.null(docker_env) || (is.logical(docker_env) && length(docker_env) == 1)
  )

  # Auto-detect if running in Docker via env var
  if (is.null(docker_env)) {
    docker_env <- identical(tolower(Sys.getenv("IS_DOCKER")), "true")
    cli::cli_alert_info(paste0(
      "Docker environment auto-detected: ",
      docker_env
    ))
  }

  #### 1 Load/create virtual environment ####

  cli::cli_alert_info(paste0(
    "Loading/creating virtual environment (",
    venv_name,
    ") for GLiNER model..."
  ))

  if (docker_env) {
    cli::cli_alert_info(
      "Running inside Docker: assuming Python + GLiNER already installed"
    )
    reticulate::use_virtualenv("/opt/gliner-venv", required = TRUE)
  } else {
    Sys.setenv("RETICULATE_VIRTUALENV_ROOT" = getwd())

    if (!reticulate::virtualenv_exists(venv_name)) {
      python_exec <- if (use_system_python) {
        cli::cli_alert_info("Using system-installed Python at /usr/bin/python3")
        "/usr/bin/python3"
      } else {
        cli::cli_alert_info("Installing Python with pyenv...")
        reticulate::install_python(version = python_version)
        python_version
      }

      reticulate::virtualenv_create(
        envname = venv_name,
        python = python_exec
      )
    }

    if (!reticulate::virtualenv_exists(venv_name)) {
      stop(
        "Virtual environment '",
        venv_name,
        "' does not exist; cannot load GLiNER model"
      )
    }

    reticulate::use_virtualenv(venv_name, required = TRUE)

    #### 2 Install gliner if needed ####

    cli::cli_alert_info("Checking/installing 'gliner' Python package...")

    available_packages <- reticulate::py_list_packages(envname = venv_name)
    if (!"gliner" %in% available_packages$package) {
      reticulate::py_install(envname = venv_name, packages = "gliner")
    }

    if (
      !"gliner" %in% reticulate::py_list_packages(envname = venv_name)$package
    ) {
      stop(
        "Package 'gliner' is not installed in virtual environment '",
        venv_name,
        "'"
      )
    }
  }

  #### 3 Load gliner Python module ####

  gliner <- reticulate::import("gliner")

  #### 4 Load model ####

  cli::cli_alert_info(paste0(
    "Loading/downloading GLiNER model ('",
    model_name,
    "')..."
  ))

  Sys.setenv(HF_HUB_DISABLE_SYMLINKS = "1")
  cache_dir <- if (docker_env) "/opt/hf-cache" else
    reticulate::virtualenv_python(venv_name) |> dirname() |> dirname()

  cli::cli_alert_info(paste0(
    "Using model cache directory: ",
    cache_dir
  ))

  model <- gliner$GLiNER$from_pretrained(
    model_name,
    cache_dir = cache_dir
  )

  #### 5 Test model ####

  if (test_model) {
    cli::cli_alert_info("Testing GLiNER model...")

    test_result <- tryCatch(
      model$predict_entities(
        text = paste0(
          "My name is Luka Koning,",
          " I live on 5th avenue street in London.",
          " I work at Kennispunt Twente",
          " sometimes I visit the University of Twente.",
          " Lets go for a walk at 5th avenue street today! btw, my name is Kangorowits Wakka Wakka"
        ),
        labels = c("person", "address", "employer")
      ),
      error = function(e) stop("Error testing GLiNER model: ", e$message)
    )
  }

  #### 6 Return model ####

  cli::cli_alert_success("GLiNER model loaded successfully!")
  return(model)
}
