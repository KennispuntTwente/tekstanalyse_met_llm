#### 1 Load venv ####

load_gliner_model <- function(
  venv_name = "kwallm__venv7",
  python_version = "3.12.10",
  model_name = "urchade/gliner_multi_pii-v1"
) {
  stopifnot(
    is.character(venv_name),
    length(venv_name) == 1,
    is.character(python_version),
    length(python_version) == 1,
    is.character(model_name),
    length(model_name) == 1
  )

  #### 1 Load/create virtual environment ####

  cli::cli_alert_info(paste0(
    "Loading/creating virtual environment (",
    venv_name,
    ") for GLiNER model..."
  ))

  if (!reticulate::virtualenv_exists(venv_name)) {
    python_version <- "3.12.10"

    # Install Python if not already installed
    reticulate::install_python(
      version = python_version
    )

    # Create virtual environment
    reticulate::virtualenv_create(
      envname = venv_name,
      python = python_version
    )
  }
  if (!reticulate::virtualenv_exists(venv_name)) {
    stop(
      "Virtual environment '",
      venv_name,
      "' does not exist; cannot load GLiNER model"
    )
  }
  reticulate::use_virtualenv(venv_name)

  #### 2 Load gliner ####

  cli::cli_alert_info("Loading/installing 'gliner' Python package...")

  available_packages <- reticulate::py_list_packages(envname = venv_name)
  if (!"gliner" %in% available_packages$package) {
    reticulate::py_install(
      envname = venv_name,
      packages = "gliner"
    )
  }
  available_packages <- reticulate::py_list_packages(envname = venv_name)
  if (
    !"gliner" %in% reticulate::py_list_packages(envname = venv_name)$package
  ) {
    stop(
      "Package 'gliner' is not installed in virtual environment '",
      venv_name,
      "'; cannot load GLiNER model"
    )
  }
  gliner <- reticulate::import("gliner")

  #### 3 (Down)load model ####

  cli::cli_alert_info(paste0(
    "Loading/downloading GLiNER model ('",
    model_name,
    "')..."
  ))

  # Disable symlinks to avoid issues with copying files in some environments
  Sys.setenv(HF_HUB_DISABLE_SYMLINKS = "1")

  # (Down)load the GLiNER model from Hugging Face
  model <- gliner$GLiNER$from_pretrained(
    "urchade/gliner_multi_pii-v1",
    cache_dir = reticulate::virtualenv_python(venv_name) |> dirname()
  )

  #### 4 Test the model ####

  cli::cli_alert_info("Testing GLiNER model...")

  test_result <- tryCatch(
    {
      model$predict_entities(
        text = paste0(
          "My name is Luka Koning,",
          " I live on 5th avenue street in London.",
          " I work at Kennispunt Twente",
          " sometimes I visit the University of Twente"
        ),
        labels = c("person", "address", "employer")
      )
    },
    error = function(e) {
      stop("Error testing GLiNER model: ", e$message)
    }
  )

  cli::cli_alert_success("GLiNER model loaded successfully!")

  return(model)
}

model <- load_gliner_model(
  venv_name = "kwallm__venv7",
  python_version = "3.12.10",
  model_name = "urchade/gliner_multi_pii-v1"
)
