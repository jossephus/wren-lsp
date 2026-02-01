import { workspace, ExtensionContext, window, ProgressLocation, commands } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import * as path from "path";
import * as os from "os";
import * as fs from "fs";
import * as https from "https";

let client: LanguageClient | null = null;
let downloadInProgress: Promise<string> | null = null;

const REPO = "jossephus/wren-lsp";
const MAX_REDIRECTS = 5;
const REQUEST_TIMEOUT_MS = 60000;
const ALLOWED_HOSTS = ["github.com", "objects.githubusercontent.com", "github-releases.githubusercontent.com"];

interface PlatformInfo {
  platform: string;
  asset: string;
}

interface GitHubAsset {
  name: string;
  browser_download_url: string;
  size?: number;
}

interface GitHubRelease {
  tag_name: string;
  assets: GitHubAsset[];
}

interface DownloadOptions {
  onProgress?: (downloaded: number, total: number) => void;
  maxRedirects?: number;
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

function isAllowedHost(urlString: string): boolean {
  try {
    const url = new URL(urlString);
    return url.protocol === "https:" && ALLOWED_HOSTS.some(host => url.host === host || url.host.endsWith(`.${host}`));
  } catch {
    return false;
  }
}

function cleanupFile(filePath: string): void {
  try {
    if (fs.existsSync(filePath)) {
      fs.unlinkSync(filePath);
    }
  } catch {
    // Ignore cleanup errors
  }
}

async function downloadFile(
  url: string,
  dest: string,
  options: DownloadOptions = {}
): Promise<void> {
  const { onProgress, maxRedirects = MAX_REDIRECTS } = options;

  if (maxRedirects <= 0) {
    throw new Error("Too many redirects");
  }

  if (!isAllowedHost(url)) {
    throw new Error(`Untrusted download host: ${url}`);
  }

  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);
    let resolved = false;

    const cleanup = (error: Error) => {
      if (resolved) return;
      resolved = true;
      file.destroy();
      cleanupFile(dest);
      reject(error);
    };

    file.on("error", cleanup);

    const req = https.get(
      url,
      { headers: { "User-Agent": "wren-lsp-vscode" } },
      (response) => {
        if (response.statusCode === 302 || response.statusCode === 301) {
          file.destroy();
          cleanupFile(dest);
          response.resume();

          const location = response.headers.location;
          if (!location) {
            reject(new Error("Redirect without location header"));
            return;
          }

          const resolvedUrl = new URL(location, url).toString();

          downloadFile(resolvedUrl, dest, { onProgress, maxRedirects: maxRedirects - 1 })
            .then(resolve)
            .catch(reject);
          return;
        }

        if (response.statusCode !== 200) {
          cleanup(new Error(`Download failed with status ${response.statusCode}`));
          return;
        }

        response.on("error", cleanup);

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
          if (resolved) return;
          resolved = true;
          file.close();
          resolve();
        });
      }
    );

    req.on("error", cleanup);
    req.setTimeout(REQUEST_TIMEOUT_MS, () => {
      req.destroy();
      cleanup(new Error("Request timed out"));
    });
  });
}

async function getLatestRelease(): Promise<GitHubRelease> {
  const url = `https://api.github.com/repos/${REPO}/releases/latest`;

  return new Promise((resolve, reject) => {
    const req = https.get(
      url,
      { headers: { "User-Agent": "wren-lsp-vscode" } },
      (response) => {
        let data = "";

        response.on("data", (chunk) => {
          data += chunk;
        });

        response.on("error", reject);

        response.on("end", () => {
          if (response.statusCode !== 200) {
            let message = `GitHub API request failed with status ${response.statusCode}`;
            try {
              const parsed = JSON.parse(data);
              if (parsed.message) {
                message += `: ${parsed.message}`;
              }
            } catch {
              // Ignore parse errors
            }
            reject(new Error(message));
            return;
          }

          try {
            const release = JSON.parse(data);
            if (!release.tag_name || !Array.isArray(release.assets)) {
              reject(new Error("Invalid release format from GitHub API"));
              return;
            }
            resolve(release as GitHubRelease);
          } catch (e) {
            reject(new Error("Failed to parse GitHub API response"));
          }
        });
      }
    );

    req.on("error", reject);
    req.setTimeout(REQUEST_TIMEOUT_MS, () => {
      req.destroy();
      reject(new Error("GitHub API request timed out"));
    });
  });
}

function getInstalledVersion(globalStoragePath: string): string | null {
  const versionFile = path.join(globalStoragePath, "version.txt");
  try {
    if (fs.existsSync(versionFile)) {
      return fs.readFileSync(versionFile, "utf-8").trim();
    }
  } catch {
    // Ignore read errors
  }
  return null;
}

function saveInstalledVersion(globalStoragePath: string, version: string): void {
  const versionFile = path.join(globalStoragePath, "version.txt");
  try {
    fs.writeFileSync(versionFile, version, "utf-8");
  } catch {
    // Ignore write errors
  }
}

function isBinaryInstalled(globalStoragePath: string): boolean {
  const binaryPath = path.join(globalStoragePath, getBinaryName());
  return fs.existsSync(binaryPath);
}

async function downloadServer(
  globalStoragePath: string,
  release: GitHubRelease,
  platformInfo: PlatformInfo
): Promise<string> {
  const binaryName = getBinaryName();
  const binaryPath = path.join(globalStoragePath, binaryName);
  const tempPath = `${binaryPath}.tmp`;

  if (!fs.existsSync(globalStoragePath)) {
    fs.mkdirSync(globalStoragePath, { recursive: true });
  }

  const asset = release.assets.find((a) => a.name === platformInfo.asset);

  if (!asset) {
    throw new Error(`Binary not found for platform: ${platformInfo.platform}`);
  }

  cleanupFile(tempPath);

  await window.withProgress(
    {
      location: ProgressLocation.Notification,
      title: `Downloading wren-lsp ${release.tag_name}`,
      cancellable: false,
    },
    async (progress) => {
      let lastPercent = 0;
      await downloadFile(asset.browser_download_url, tempPath, {
        onProgress: (downloaded, total) => {
          const currentPercent = Math.round((downloaded / total) * 100);
          const delta = Math.max(0, currentPercent - lastPercent);
          progress.report({ increment: delta, message: `${currentPercent}%` });
          lastPercent = currentPercent;
        },
      });

      if (os.platform() !== "win32") {
        fs.chmodSync(tempPath, 0o755);
      }

      fs.renameSync(tempPath, binaryPath);
    }
  );

  saveInstalledVersion(globalStoragePath, release.tag_name);
  return binaryPath;
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
    window.showWarningMessage(
      "Your platform is not supported for automatic download. Set 'wren.serverPath' to a manually installed wren-lsp binary."
    );
    return null;
  }

  const binaryPath = path.join(context.globalStoragePath, getBinaryName());

  if (fs.existsSync(binaryPath)) {
    return binaryPath;
  }

  if (downloadInProgress) {
    return downloadInProgress;
  }

  window.showInformationMessage(
    "wren-lsp not found. Downloading from GitHub releases..."
  );

  downloadInProgress = (async () => {
    try {
      const release = await getLatestRelease();

      const result = await downloadServer(
        context.globalStoragePath,
        release,
        platformInfo
      );

      window.showInformationMessage(
        `wren-lsp ${release.tag_name} downloaded successfully!`
      );
      return result;
    } finally {
      downloadInProgress = null;
    }
  })();

  try {
    return await downloadInProgress;
  } catch (error) {
    window.showErrorMessage(
      `Failed to download wren-lsp: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return null;
  }
}

async function updateServer(context: ExtensionContext): Promise<void> {
  const platformInfo = getPlatformInfo();
  if (!platformInfo) {
    window.showErrorMessage(
      "Your platform is not supported for automatic updates."
    );
    return;
  }

  if (downloadInProgress) {
    window.showWarningMessage("A download is already in progress.");
    return;
  }

  try {
    const release = await getLatestRelease();

    const installedVersion = getInstalledVersion(context.globalStoragePath);
    const binaryExists = isBinaryInstalled(context.globalStoragePath);

    if (installedVersion === release.tag_name && binaryExists) {
      window.showInformationMessage(
        `wren-lsp is already up to date (${installedVersion}).`
      );
      return;
    }

    const action = installedVersion && binaryExists
      ? `Update from ${installedVersion} to ${release.tag_name}?`
      : `Download wren-lsp ${release.tag_name}?`;

    const choice = await window.showInformationMessage(
      action,
      "Yes",
      "No"
    );

    if (choice !== "Yes") {
      return;
    }

    if (client) {
      await client.stop();
      client = null;
    }

    downloadInProgress = downloadServer(context.globalStoragePath, release, platformInfo);

    try {
      await downloadInProgress;
      window.showInformationMessage(
        `wren-lsp updated to ${release.tag_name}. Restart VS Code to use the new version.`
      );
    } finally {
      downloadInProgress = null;
    }
  } catch (error) {
    window.showErrorMessage(
      `Failed to update wren-lsp: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
  }
}

export async function activate(context: ExtensionContext) {
  context.subscriptions.push(
    commands.registerCommand("wren.updateServer", () => updateServer(context))
  );

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
