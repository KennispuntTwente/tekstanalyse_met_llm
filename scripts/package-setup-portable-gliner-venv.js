#!/usr/bin/env node
/**
 * Bootstrap a GLiNER venv + download model.
 *
 *   npm run setup-gliner            # uses pyenv if available
 *   npm run setup-gliner -- --use-system-python
 *
 * Prereqs:  npm i commander execa tmp
 */

import { program } from 'commander';
import { execaSync } from 'execa';
import { tmpdir } from 'os';
import { join, resolve } from 'path';
import { writeFileSync, existsSync } from 'fs';
import { dirSync } from 'tmp';

// ────────────────── helpers ────────────────────────────────────────────────
const run = (cmd, opts = {}) =>
  execaSync(cmd, { stdio: 'inherit', shell: true, ...opts });

const runSilent = (cmd) =>
  execaSync(cmd, { stdio: 'pipe', shell: true });

const log = (msg) => console.log(`[gliner-setup] ${msg}`);

// ────────────────── CLI ────────────────────────────────────────────────────
program
  .option('-n, --venv-name <name>',   'virtualenv name',          'portable-gliner-venv')
  .option('-p, --python-version <v>', 'Python version via pyenv', '3.12.10')
  .option('-m, --model-name <id>',    'Hugging Face model id',    'urchade/gliner_multi_pii-v1')
  .option('--use-system-python',      'skip pyenv and use system python', false)
  .parse();

const opt = program.opts();

// ────────────────── 1. choose Python interpreter ───────────────────────────
let pythonBin = process.platform === 'win32' ? 'python' : 'python3';
const wantPyenv = !opt.useSystemPython;
let havePyenv = false;

if (wantPyenv) {
  try {
    runSilent('pyenv --version');
    havePyenv = true;
  } catch {
    log('pyenv not found → falling back to system Python');
  }
}

if (havePyenv) {
  const versions = runSilent('pyenv versions --bare').stdout.split('\n');
  if (!versions.includes(opt.pythonVersion)) {
    log(`Installing Python ${opt.pythonVersion} with pyenv…`);
    run(`pyenv install -s ${opt.pythonVersion}`);
  }
  const root = runSilent('pyenv root').stdout.trim();
  pythonBin = join(root, 'versions', opt.pythonVersion, 'bin', 'python');
}

log(`Using interpreter: ${pythonBin}`);

// ────────────────── 2. create / update venv ────────────────────────────────
const venvPath = resolve(opt.venvName);
const binDir = process.platform === 'win32' ? 'Scripts' : 'bin';
const venvPython = join(
  venvPath,
  binDir,
  process.platform === 'win32' ? 'python.exe' : 'python'
);

if (!existsSync(join(venvPath, binDir))) {
  log(`Creating virtual-env ${opt.venvName}…`);
  run(`${pythonBin} -m venv "${venvPath}"`);
}

// Always upgrade pip & install gliner via “python -m pip …” so pip.exe isn't locked
run(`"${venvPython}" -m pip install --upgrade pip`);
run(`"${venvPython}" -m pip install --upgrade gliner`);

// ────────────────── 3. determine cache directory ───────────────────────────
const isDocker = (process.env.IS_DOCKER || '').toLowerCase() === 'true';
const cacheDir = isDocker
  ? '/opt/hf-cache'            // match the R helper’s hard-coded Docker path
  : venvPath;

log(`Model cache will live in: ${cacheDir}`);

// ────────────────── 4. download model ──────────────────────────────────────
const pyCode = `
from gliner import GLiNER
import os, sys
os.environ["HF_HUB_DISABLE_SYMLINKS"] = "1"
try:
    GLiNER.from_pretrained(
        "${opt.modelName}",
        cache_dir=r"${cacheDir.replace(/\\\\/g, '\\\\\\\\')}"
    )
    print("GLiNER model downloaded and ready.")
except Exception as e:
    print("Error downloading model:", e, file=sys.stderr)
    sys.exit(1)
`;

const tmp = dirSync({ tmpdir: tmpdir(), unsafeCleanup: true });
const pyFile = join(tmp.name, 'init_gliner.py');
writeFileSync(pyFile, pyCode);
run(`"${venvPython}" "${pyFile}"`);
tmp.removeCallback();
