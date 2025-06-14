// This script checks if portable R, Pandoc, and Gliner venv folders exist,
// and if not, runs the setup scripts to create them

const fs = require('fs');
const { execSync } = require('child_process');

function runIfMissing(folder, command) {
  if (!fs.existsSync(folder)) {
    console.log(`Folder "${folder}" not found. Running "${command}"...`);
    execSync(command, { stdio: 'inherit' });
  } else {
    console.log(`Folder "${folder}" already exists. Skipping "${command}".`);
  }
}

runIfMissing('portable-r', 'node scripts/package-setup-portable-r.js');
runIfMissing('portable-pandoc', 'node scripts/package-setup-portable-pandoc.js');
runIfMissing('portable-gliner-venv', 'node scripts/package-setup-portable-gliner-venv-js.js');
