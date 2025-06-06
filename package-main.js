const { app, BrowserWindow } = require('electron');
const { spawn } = require('child_process');
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
    });

    resolve();
  });
}

async function waitForShiny(port) {
  return new Promise((resolve, reject) => {
    waitOn({ resources: [`http://127.0.0.1:${port}`] }, (err) => {
      if (err) reject(err);
      else resolve();
    });
  });
}

function createSplashWindow() {
  splashWin = new BrowserWindow({
    width: 600,
    height: 420,
    autoHideMenuBar: true,
    resizable: false,
    alwaysOnTop: false,
    title: 'Text analysis with LLM',
    icon: path.join(__dirname, 'www', 'icon.ico'),
    webPreferences: {
      contextIsolation: true,
      preload: path.join(__dirname, 'splash-preload.js')
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
        <h1>App is loading...</h2>
        <p>This should take no longer than 30 seconds.<br>
        Is it not working? View the log file:<br><br>
        <code>${logFilePath}</code></p>
      </body>
    </html>
  `;

  splashWin.loadURL(`data:text/html;charset=utf-8,${encodeURIComponent(splashHTML)}`);
}

function createMainWindow(port) {
  mainWin = new BrowserWindow({
    width: 800,
    height: 1000,
    minWidth: 500,
    minHeight: 500,
    autoHideMenuBar: true,
    resizable: true,
    alwaysOnTop: false,
    title: 'Text analysis with LLM',
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

app.on('before-quit', () => {
  if (shinyProcess && shinyProcess.pid) {
    log('Killing Shiny process...');
    treeKill(shinyProcess.pid, 'SIGTERM', (err) => {
      if (err) console.error('Failed to kill process tree:', err);
      else log('Shiny process tree killed');
    });
  }
});
