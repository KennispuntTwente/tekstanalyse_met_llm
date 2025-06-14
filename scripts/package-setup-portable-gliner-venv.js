// This script ensures portable-gliner-venv exists by calling the R setup if needed

const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");

const VENV_DIR = path.resolve(__dirname, "../portable-gliner-venv");

function tryLoadGlinerFromR() {
  console.log("[INFO] Attempting to load gliner model using R...");
  const rScript = `
    source("R/gliner_load.R")
    gliner_load_model()
  `;

  const result = spawnSync("Rscript", ["-e", rScript], {
    stdio: "inherit",
    cwd: path.resolve(__dirname, ".."),
  });

  if (result.error) {
    console.error("[ERROR] Failed to run Rscript:", result.error.message);
    process.exit(1);
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

