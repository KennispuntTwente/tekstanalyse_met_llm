// This script sets up Pandoc for portable use on Windows
// Used to include in the Electron app

const { execSync } = require("child_process");
const { existsSync, mkdirSync, createWriteStream, rmSync } = require("fs");
const fs = require("fs");
const https = require("https");
const path = require("path");
const unzip = require("unzipper");

const PANDOC_VERSION = "3.1.11.1";
const PANDOC_ZIP_URL = `https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-windows-x86_64.zip`;

const DOWNLOAD_PATH = path.resolve(__dirname, `../pandoc-${PANDOC_VERSION}.zip`);
const EXTRACT_DIR = path.resolve(__dirname, "../portable-pandoc");

function downloadFile(url, dest, cb) {
  const file = createWriteStream(dest);

  function request(url) {
    https.get(url, (response) => {
      if ([301, 302].includes(response.statusCode)) {
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

async function extractZip(zipPath, targetDir) {
  await fs.createReadStream(zipPath)
    .pipe(unzip.Extract({ path: targetDir }))
    .promise();
}

function writeLauncher() {
  const launcher = `
  @echo off
  set PANDOC_HOME=%~dp0portable-pandoc
  set PATH=%PANDOC_HOME%;%PATH%
  %PANDOC_HOME%\\pandoc.exe %*
  `;
  require("fs").writeFileSync(path.resolve(__dirname, "../run-pandoc.bat"), launcher);
}

async function setupPandoc() {
  if (!existsSync(DOWNLOAD_PATH)) {
    console.log("[INFO] Downloading Pandoc...");
    await new Promise((resolve) => downloadFile(PANDOC_ZIP_URL, DOWNLOAD_PATH, resolve));
  }

  if (!existsSync(EXTRACT_DIR)) {
    console.log("[INFO] Extracting Pandoc...");
    await extractZip(DOWNLOAD_PATH, EXTRACT_DIR);
  }

  console.log("[INFO] Cleaning up installer...");
  rmSync(DOWNLOAD_PATH);

  // console.log("Creating launcher...");
  // writeLauncher();

  console.log("[SUCCESS] Pandoc is set up for portable use.");
}

setupPandoc().catch(console.error);
