# Function to load the GLiNER model in a virtual environment;
#   installs Python, sets up venv, downloads model, loads model, tests model, returns model

# Allows to load Python & then interrupt R without fatal R crash:
Sys.setenv(FOR_DISABLE_CONSOLE_CTRL_HANDLER = "1")

gliner_load_model <- function(
  model_name = "urchade/gliner_multi_pii-v1",
  test_model = FALSE,
  queue = NULL
) {
  stopifnot(
    is.character(model_name) && length(model_name) == 1,
    is.logical(test_model) && length(test_model) == 1,
    is.null(queue) || inherits(queue, "Queue")
  )

  # Helper to print messages to the console + queue if needed
  print_message <- async_message_printer(
    queue = queue,
    reactive_value_name = "gliner_load_message"
  )

  ##### 1 Load Python, GLiNER ####

  print_message("Loading Python and GLiNER module...")

  Sys.unsetenv("RETICULATE_PYTHON")
  reticulate:::uv_exec("sync")
  reticulate::use_virtualenv("./.venv")
  gliner <- reticulate::import("gliner")

  #### 4 Load model ####

  Sys.setenv(HF_HUB_DISABLE_SYMLINKS = "1")
  cache_dir <- "./.venv"

  print_message(paste0(
    "Loading GLiNER model (model_name: '",
    model_name,
    "'",
    ", cache_dir: '",
    cache_dir,
    "')..."
  ))

  model <- gliner$GLiNER$from_pretrained(
    model_name,
    cache_dir = cache_dir
  )

  #### 5 Test model ####

  if (test_model) {
    print_message("Testing GLiNER model...")

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

  print_message("GLiNER model loaded successfully!", type = "success")
  return(model)
}
