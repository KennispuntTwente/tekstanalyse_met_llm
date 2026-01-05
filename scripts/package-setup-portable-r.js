// This script sets up a portable R environment on Windows
// Used to include in the Electron app
// Note that it copies R packages from the renv library, so it is required
//    to first have the renv library setup through 'renv::restore()' before
//    running this script

const { execSync } = require("child_process");
const { existsSync, mkdirSync, writeFileSync, copyFileSync, readdirSync, statSync } = require("fs");
const https = require("https");
const path = require("path");
const fs = require("fs");

const R_VERSION = "4.5.1";
const R_SHORT_VERSION = R_VERSION.match(/^(\d+\.\d+)/)[1];
const R_INSTALLER_URL = `https://cran.r-project.org/bin/windows/base/old/${R_VERSION}/R-${R_VERSION}-win.exe`;
const INSTALLER_PATH = path.resolve(__dirname, "../R-" + R_VERSION + ".exe");
const R_DIR = path.resolve(__dirname, "../portable-r");
const RENVSOURCE_DIR = path.resolve(__dirname, `../renv/library/windows/R-${R_SHORT_VERSION}/x86_64-w64-mingw32`);
const RENVTARGET_DIR = path.join(R_DIR, "library");

// Simple recursive copy
function copyRecursive(src, dest) {
  if (!existsSync(dest)) mkdirSync(dest, { recursive: true });

  for (const item of readdirSync(src)) {
    const srcPath = path.join(src, item);
    const destPath = path.join(dest, item);
    const stat = statSync(srcPath);

    if (stat.isDirectory()) {
      copyRecursive(srcPath, destPath);
    } else {
      copyFileSync(srcPath, destPath);
    }
  }
}

function downloadFile(url, dest, cb) {
  const file = fs.createWriteStream(dest);

  function request(url) {
    https.get(url, (response) => {
      if (response.statusCode === 302 || response.statusCode === 301) {
        request(response.headers.location);
      } else if (response.statusCode !== 200) {
        cb(new Error(`Download failed: ${response.statusCode}`));
      } else {
        response.pipe(file);
        file.on("finish", () => {
          file.close(cb);
        });
      }
    }).on("error", cb);
  }

  request(url);
}

function writeRProfile() {
  const siteFile = path.join(R_DIR, "etc", "Rprofile.site");
  const content = `
  .libPaths(file.path(getwd(), "renv/library/R-" + paste(R.version$major, R.version$minor, sep = ".")))
  Sys.setenv(R_ENVIRON_USER = "")
  `;
  writeFileSync(siteFile, content);
}

function writeLauncher() {
  const launcher = `
  @echo off
  set R_HOME=%~dp0portable-r
  set PATH=%R_HOME%\\bin;%PATH%
  %R_HOME%\\bin\\Rscript.exe %~dp0your_script.R %*
  `;
  writeFileSync(path.resolve(__dirname, "../run.bat"), launcher);
}

async function setupR() {
  if (!existsSync(INSTALLER_PATH)) {
    console.log("[INFO] Downloading R installer...");
    await new Promise((resolve) =>
      downloadFile(R_INSTALLER_URL, INSTALLER_PATH, resolve)
    );
  }

  console.log("[INFO] Installing R...");
  execSync(`"${INSTALLER_PATH}" /VERYSILENT /DIR="${R_DIR}" /NOICONS /NODESKTOPICON /SUPPRESSMSGBOXES`, {
    stdio: "inherit",
  });

  console.log("[INFO] Cleaning up installer...");
  fs.unlinkSync(INSTALLER_PATH);

  console.log("[INFO] Configuring R...");
  // writeRProfile();
  // writeLauncher();

  if (existsSync(RENVSOURCE_DIR)) {
    console.log("Copying renv library...");
    copyRecursive(RENVSOURCE_DIR, RENVTARGET_DIR);
  } else {
    console.warn(`[WARNING] renv library not found at: ${RENVSOURCE_DIR}`);
    console.warn(`Please ensure you have installed the library via 'renv::restore()' before running this script!`);
  }

  console.log("[SUCCESS] R is set up for portable use!");
}

setupR();
