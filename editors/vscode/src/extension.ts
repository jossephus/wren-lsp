import { workspace, ExtensionContext, window, ProgressLocation } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import * as path from "path";
import * as os from "os";
import * as fs from "fs";
import * as https from "https";

let client: LanguageClient;

const REPO = "jossephus/wren-lsp";

const LSP_VERSION: string = require("../package.json").lspVersion || "latest";

interface PlatformInfo {
  platform: string;
  asset: string;
}

function getPlatformInfo(): PlatformInfo | null {
  const platform = os.platform();
  const arch = os.arch();

  const platformMap: Record<string, string> = {
    "linux-x64": "wren-lsp-linux-x86_64",
    "linux-arm64": "wren-lsp-linux-aarch64",
    "darwin-x64": "wren-lsp-macos-x86_64",
    "darwin-arm64": "wren-lsp-macos-aarch64",
    "win32-x64": "wren-lsp-windows-x86_64.exe",
    "win32-arm64": "wren-lsp-windows-aarch64.exe",
  };

  const key = `${platform}-${arch}`;
  const asset = platformMap[key];

  if (!asset) {
    return null;
  }

  return { platform: key, asset };
}

function getBinaryName(): string {
  return os.platform() === "win32" ? "wren-lsp.exe" : "wren-lsp";
}

async function downloadFile(
  url: string,
  dest: string,
  onProgress?: (downloaded: number, total: number) => void
): Promise<void> {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);
    https.get(
      url,
      { headers: { "User-Agent": "wren-lsp-vscode" } },
      (response) => {
        if (response.statusCode === 302 || response.statusCode === 301) {
          downloadFile(response.headers.location!, dest, onProgress)
            .then(resolve)
            .catch(reject);
          return;
        }
        if (response.statusCode !== 200) {
          reject(new Error(`Download failed: ${response.statusCode}`));
          return;
        }

        const total = parseInt(response.headers["content-length"] || "0", 10);
        let downloaded = 0;

        response.on("data", (chunk: Buffer) => {
          downloaded += chunk.length;
          if (onProgress && total > 0) {
            onProgress(downloaded, total);
          }
        });

        response.pipe(file);
        file.on("finish", () => {
          file.close();
          resolve();
        });
      }
    ).on("error", reject);
  });
}

async function getRelease(version: string): Promise<any> {
  const url =
    version === "latest"
      ? `https://api.github.com/repos/${REPO}/releases/latest`
      : `https://api.github.com/repos/${REPO}/releases/tags/${version}`;

  return new Promise((resolve, reject) => {
    https
      .get(url, { headers: { "User-Agent": "wren-lsp-vscode" } }, (response) => {
        let data = "";
        response.on("data", (chunk) => {
          data += chunk;
        });
        response.on("end", () => {
          try {
            resolve(JSON.parse(data));
          } catch (e) {
            reject(e);
          }
        });
      })
      .on("error", reject);
  });
}

function cleanupOldVersions(
  globalStoragePath: string,
  currentVersion: string
): void {
  try {
    if (!fs.existsSync(globalStoragePath)) {
      return;
    }

    const entries = fs.readdirSync(globalStoragePath);
    for (const entry of entries) {
      const entryPath = path.join(globalStoragePath, entry);
      const stat = fs.statSync(entryPath);

      if (stat.isDirectory() && entry !== currentVersion) {
        if (/^v\d+\.\d+/.test(entry) || /^\d+\.\d+/.test(entry)) {
          fs.rmSync(entryPath, { recursive: true, force: true });
          console.log(`Cleaned up old version: ${entry}`);
        }
      }
    }
  } catch (error) {
    console.error("Failed to cleanup old versions:", error);
  }
}

async function ensureServerBinary(
  context: ExtensionContext
): Promise<string | null> {
  const config = workspace.getConfiguration("wren");
  const customPath = config.get<string>("serverPath");

  if (customPath && customPath !== "wren-lsp") {
    return customPath;
  }

  const platformInfo = getPlatformInfo();
  if (!platformInfo) {
    return getBinaryName();
  }

  const binaryName = getBinaryName();
  const versionDir = path.join(context.globalStoragePath, LSP_VERSION);
  const binaryPath = path.join(versionDir, binaryName);

  if (fs.existsSync(binaryPath)) {
    return binaryPath;
  }

  window.showInformationMessage(
    `wren-lsp ${LSP_VERSION} not found. Downloading from GitHub releases...`
  );

  try {
    await window.withProgress(
      {
        location: ProgressLocation.Notification,
        title: `Downloading wren-lsp ${LSP_VERSION}`,
        cancellable: false,
      },
      async (progress) => {
        if (!fs.existsSync(versionDir)) {
          fs.mkdirSync(versionDir, { recursive: true });
        }

        const release = await getRelease(LSP_VERSION);

        if (!release.assets) {
          throw new Error(
            `Release ${LSP_VERSION} not found or has no assets`
          );
        }

        const asset = release.assets.find(
          (a: any) => a.name === platformInfo.asset
        );

        if (!asset) {
          throw new Error(
            `Binary not found for platform: ${platformInfo.platform}`
          );
        }

        await downloadFile(asset.browser_download_url, binaryPath, (
          downloaded,
          total
        ) => {
          const percent = Math.round((downloaded / total) * 100);
          progress.report({ increment: percent, message: `${percent}%` });
        });

        if (os.platform() !== "win32") {
          fs.chmodSync(binaryPath, 0o755);
        }
      }
    );

    cleanupOldVersions(context.globalStoragePath, LSP_VERSION);

    window.showInformationMessage(
      `wren-lsp ${LSP_VERSION} downloaded successfully!`
    );
    return binaryPath;
  } catch (error) {
    window.showErrorMessage(
      `Failed to download wren-lsp ${LSP_VERSION}: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return getBinaryName();
  }
}

export async function activate(context: ExtensionContext) {
  const serverPath = await ensureServerBinary(context);

  if (!serverPath) {
    window.showErrorMessage(
      "Failed to start wren-lsp: no server binary available"
    );
    return;
  }

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "wren" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.wren"),
    },
  };

  client = new LanguageClient(
    "wrenLsp",
    "Wren Language Server",
    serverOptions,
    clientOptions
  );

  await client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
