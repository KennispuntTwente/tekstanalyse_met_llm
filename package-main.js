const { app, BrowserWindow } = require('electron');
const { spawn, spawnSync } = require('child_process');
const waitOn = require('wait-on');
const path = require('path');
const net = require('net');
const fs = require('fs');
const http = require('http');
const treeKill = require('tree-kill');
const { shell } = require('electron');

const logFilePath = path.join(__dirname, 'app.log');
const logStream = fs.createWriteStream(logFilePath, { flags: 'a' });
const log = (...args) => {
  const message = args.join(' ') + '\n';
  logStream.write(message);
  console.log(message);
};

log('App starting...');

let splashWin;
let mainWin;
let shinyProcess = null;
let isQuitting = false;
let killInProgress = false;

function isAlive(pid) {
  if (!pid) return false;
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

function killProcessTreeWindows(pid, reason) {
  return new Promise((resolve) => {
    if (!pid) return resolve();
    log(`Attempting Windows taskkill for PID ${pid} (${reason})...`);
    const child = spawn('taskkill', ['/PID', String(pid), '/T', '/F'], { shell: true });
    child.on('error', (err) => {
      log(`[WARNING] taskkill failed to start: ${err.message}`);
      resolve();
    });
    child.on('exit', (code) => {
      log(`taskkill exited with code ${code} (reason: ${reason})`);
      resolve();
    });
  });
}

function killProcessTreePosix(pid, reason) {
  return new Promise((resolve) => {
    if (!pid) return resolve();
    log(`Attempting tree-kill for PID ${pid} (${reason})...`);
    treeKill(pid, 'SIGTERM', (err) => {
      if (err) log(`[WARNING] tree-kill SIGTERM failed: ${err.message}`);
      resolve();
    });
  });
}

async function killShinyProcessTree(reason) {
  if (killInProgress) return;
  killInProgress = true;

  try {
    if (!shinyProcess || !shinyProcess.pid) return;
    const pid = shinyProcess.pid;

    if (!isAlive(pid)) {
      log(`Shiny PID ${pid} already not alive (${reason}).`);
      return;
    }

    log(`Killing Shiny process tree (PID ${pid})... (${reason})`);

    if (process.platform === 'win32') {
      await killProcessTreeWindows(pid, reason);
    } else {
      await killProcessTreePosix(pid, reason);
    }

    // Fallback: if still alive after a short grace period, force kill.
    await new Promise((r) => setTimeout(r, 1200));
    if (isAlive(pid)) {
      log(`[WARNING] Shiny PID ${pid} still alive; forcing termination (${reason}).`);
      if (process.platform === 'win32') {
        await killProcessTreeWindows(pid, `${reason}:force`);
      } else {
        await new Promise((resolve) => {
          treeKill(pid, 'SIGKILL', (err) => {
            if (err) log(`[WARNING] tree-kill SIGKILL failed: ${err.message}`);
            resolve();
          });
        });
      }
    }
  } finally {
    killInProgress = false;
  }
}

function getFreePort(defaultPort = 21471) {
  return new Promise((resolve) => {
    const server = net.createServer();
    server.once('error', (err) => {
      log(`[WARNING] Could not find a free port (${err.message}), falling back to default: ${defaultPort}`);
      resolve(defaultPort);
    });
    server.listen(0, () => {
      const port = server.address().port;
      server.close(() => resolve(port));
    });
  });
}

async function launchShinyApp(port) {
  return new Promise((resolve, reject) => {
    const rHome = path.join(__dirname, 'portable-r');
    const rExe = path.join(rHome, 'bin', 'Rscript.exe');
    const pandocDir = path.join(__dirname, 'portable-pandoc', 'pandoc-3.1.11.1');
    const pandocExe = path.join(pandocDir, 'pandoc.exe');
    const rScriptPath = path.join(__dirname, 'package-app.R');

    // Check if Rscript exists
    if (!fs.existsSync(rExe)) {
      const msg = `[ERROR] Rscript not found at: ${rExe}`;
      log(msg);
      return reject(new Error(msg));
    }

    // Log warning if Pandoc is missing
    if (!fs.existsSync(pandocExe)) {
      log(`[WARNING] Pandoc not found at: ${pandocExe}`);
    }

    const env = {
      ...process.env,
      R_HOME: rHome,
      PATH: `${pandocDir};${process.env.PATH}`,
      R_PROFILE_USER: '',
      R_ENVIRON_USER: '',
      SHINY_PORT: port.toString()
    };

    shinyProcess = spawn(rExe, ['--no-init-file', '--no-site-file', rScriptPath, port.toString()], {
      cwd: __dirname,
      env,
      shell: false
    });

    shinyProcess.stdout.on('data', (data) => {
      log(data.toString());
    });

    shinyProcess.stderr.on('data', (data) => {
      log(data.toString());
    });

    shinyProcess.on('error', (err) => {
      log('Failed to start Shiny process:', err);
      reject(err);
    });

    shinyProcess.on('exit', (code) => {
      log(`Shiny process exited with code ${code}`);
      // If Shiny dies unexpectedly, close the UI to avoid a stuck Electron window.
      if (!isQuitting) {
        try {
          if (mainWin && !mainWin.isDestroyed()) mainWin.close();
          if (splashWin && !splashWin.isDestroyed()) splashWin.close();
        } catch (e) {
          log(`[WARNING] Failed to close windows after Shiny exit: ${e.message}`);
        }
        app.quit();
      }
    });

    resolve();
  });
}

async function waitForShiny(port) {
  const TIMEOUT_MS = 90 * 1000; // 90 seconds
  return new Promise((resolve, reject) => {
    waitOn({ resources: [`http://127.0.0.1:${port}`], timeout: TIMEOUT_MS }, (err) => {
      if (err) reject(new Error(`Shiny did not become ready within ${TIMEOUT_MS / 1000}s: ${err.message}`));
      else resolve();
    });
  });
}

function createSplashWindow() {
  splashWin = new BrowserWindow({
    width: 600,
    height: 400,
    autoHideMenuBar: true,
    resizable: false,
    alwaysOnTop: false,
    title: 'KWALLM: Text analysis with LLM',
    icon: path.join(__dirname, 'www', 'icon.ico'),
    webPreferences: {
      contextIsolation: true
    }
  });

  const splashHTML = `
    <html>
      <head>
        <style>
          body {
            font-family: 'Segoe UI', sans-serif;
            background: #f4f4f4;
            padding: 20px;
          }
          h2 {
            color: #444;
          }
          #logOutput {
            height: 280px;
            overflow-y: auto;
            background: #1e1e1e;
            color: #dcdcdc;
            padding: 10px;
            border-radius: 8px;
            font-family: monospace;
            white-space: pre-wrap;
            box-shadow: inset 0 0 5px #ccc;
          }
        </style>
      </head>
      <body>
        <h1>App is loading...</h1>
        <p>This should take no longer than 30 seconds.<br>
        (The first time may take longer, as it needs to install Python.)<br>
        Is it not working? View the log file:<br><br>
        <code>${logFilePath}</code></p>
      </body>
    </html>
  `;

  splashWin.loadURL(`data:text/html;charset=utf-8,${encodeURIComponent(splashHTML)}`);
}

function createMainWindow(port) {
  mainWin = new BrowserWindow({
    width: 1000,
    height: 1200,
    minWidth: 600,
    minHeight: 600,
    autoHideMenuBar: true,
    resizable: true,
    alwaysOnTop: false,
    title: 'KWALLM: Text analysis with LLM',
    icon: path.join(__dirname, 'www', 'icon.ico'),
    webPreferences: {
      contextIsolation: true
    }
  });

  mainWin.loadURL(`http://127.0.0.1:${port}`);
  mainWin.setMenuBarVisibility(false);
  mainWin.removeMenu();
  mainWin.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });
  //mainWin.webContents.openDevTools();

  // Ensure that closing the window triggers a quit (and cleanup) on all platforms.
  mainWin.on('close', () => {
    if (!isQuitting) {
      isQuitting = true;
      app.quit();
    }
  });
}

app.whenReady().then(async () => {
  try {
    const port = await getFreePort();
    createSplashWindow();
    await launchShinyApp(port);
    await waitForShiny(port);
    splashWin.close();
    createMainWindow(port);
    log("Shiny is ready, window created.");
  } catch (err) {
    console.error("Startup failed:", err);
    if (splashWin) splashWin.close();
    const failWin = new BrowserWindow({ width: 600, height: 400 });
    failWin.loadURL('data:text/html,<h1>Error</h1><pre>' + err + '</pre>');
  }
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

app.on('before-quit', (event) => {
  // Make sure we terminate R + all its children before exiting Electron.
  if (killInProgress) return;
  isQuitting = true;
  event.preventDefault();

  killShinyProcessTree('before-quit')
    .catch((err) => log(`[WARNING] killShinyProcessTree failed: ${err.message}`))
    .finally(() => {
      log('Exiting Electron after cleanup.');
      app.exit(0);
    });
});

// Best-effort cleanup when Electron receives termination signals.
// Note: `exit` cannot run async work, so we use spawnSync on Windows.
function syncKillOnExit(reason) {
  try {
    if (shinyProcess && shinyProcess.pid && isAlive(shinyProcess.pid)) {
      const pid = shinyProcess.pid;
      log(`syncKillOnExit: ${reason}, PID ${pid}`);
      if (process.platform === 'win32') {
        spawnSync('taskkill', ['/PID', String(pid), '/T', '/F'], { shell: true });
      } else {
        treeKill(pid, 'SIGKILL');
      }
    }
  } catch (e) {
    // Ignore errors during forced shutdown.
  }
}

process.on('SIGINT', () => {
  isQuitting = true;
  killShinyProcessTree('SIGINT').finally(() => app.exit(0));
});
process.on('SIGTERM', () => {
  isQuitting = true;
  killShinyProcessTree('SIGTERM').finally(() => app.exit(0));
});
process.on('exit', () => syncKillOnExit('process-exit'));
