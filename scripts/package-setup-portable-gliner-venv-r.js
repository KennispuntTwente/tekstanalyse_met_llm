// This script ensures portable-gliner-venv exists by calling the R function
// which (also) sets up the virtual environment (while loading the model)
// Had issues in GH actions environment, so it was replaced by a nodeJS script
// which now does the virtual environment setup (see: package-setup-portable-gliner-venv.js)

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const VENV_DIR = path.resolve(__dirname, "../portable-gliner-venv");

function tryLoadGlinerFromR() {
  console.log("[INFO] Attempting to load gliner model using R...");

  // CI uses the system Python that’s already on ubuntu-latest
  const rScript = `
    Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3") # force system python
    source("R/gliner_load.R")
    gliner_load_model(use_system_python = TRUE)
  `;

  const result = spawnSync(
    "Rscript",
    ["--vanilla", "-e", rScript],
    { cwd: path.resolve(__dirname, ".."), encoding: "utf8" }
  );

  // Always dump R’s stdout/stderr to the Action log
  if (result.stdout) process.stdout.write(result.stdout);
  if (result.stderr) process.stderr.write(result.stderr);

  // Propagate R’s exit status so CI fails immediately
  if (result.status !== 0) {
    console.error(`[ERROR] Rscript exited with code ${result.status}`);
    process.exit(result.status);
  }
}


function ensureGlinerVenvExists() {
  if (!fs.existsSync(VENV_DIR)) {
    console.warn(
      "[WARN] portable-gliner-venv not found. Attempting to generate it using R..."
    );
    tryLoadGlinerFromR();

    if (!fs.existsSync(VENV_DIR)) {
      console.error(
        "[ERROR] portable-gliner-venv still not found after running R script.\n" +
        "Please check R/gliner_load.R and gliner_load_model()."
      );
      process.exit(1);
    }
  } else {
    console.log("[INFO] portable-gliner-venv already exists. No action needed.");
  }
}

ensureGlinerVenvExists();

