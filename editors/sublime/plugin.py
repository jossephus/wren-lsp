"""
LSP client registration for Wren Language Server.
"""

import os
import shutil
import json
import urllib.request
import platform as plat
import threading
from pathlib import Path

import sublime
import sublime_plugin

from LSP.plugin import AbstractPlugin, register_plugin, unregister_plugin
from LSP.plugin.core.typing import Optional

REPO = "jossephus/wren-lsp"
REQUEST_TIMEOUT = 30


def get_binary_name() -> str:
    return "wren-lsp.exe" if plat.system().lower() == "windows" else "wren-lsp"


def get_storage_path() -> Path:
    return Path(sublime.cache_path()) / "WrenLSP"


def get_binary_path() -> Path:
    return get_storage_path() / get_binary_name()


def get_installed_version() -> Optional[str]:
    version_file = get_storage_path() / "version.txt"
    try:
        if version_file.exists():
            return version_file.read_text().strip()
    except Exception as e:
        print(f"WrenLSP: failed reading version.txt: {e}")
    return None


def save_installed_version(version: str) -> None:
    version_file = get_storage_path() / "version.txt"
    version_file.parent.mkdir(parents=True, exist_ok=True)
    version_file.write_text(version)


def get_platform_asset() -> Optional[str]:
    system = plat.system().lower()
    machine = plat.machine().lower()
    platform_map = {
        ("linux", "x86_64"): "wren-lsp-linux-x86_64",
        ("linux", "amd64"): "wren-lsp-linux-x86_64",
        ("linux", "aarch64"): "wren-lsp-linux-aarch64",
        ("linux", "arm64"): "wren-lsp-linux-aarch64",
        ("darwin", "x86_64"): "wren-lsp-macos-x86_64",
        ("darwin", "amd64"): "wren-lsp-macos-x86_64",
        ("darwin", "aarch64"): "wren-lsp-macos-aarch64",
        ("darwin", "arm64"): "wren-lsp-macos-aarch64",
        ("windows", "x86_64"): "wren-lsp-windows-x86_64.exe",
        ("windows", "amd64"): "wren-lsp-windows-x86_64.exe",
        ("windows", "aarch64"): "wren-lsp-windows-aarch64.exe",
        ("windows", "arm64"): "wren-lsp-windows-aarch64.exe",
    }
    return platform_map.get((system, machine))


def fetch_latest_release() -> dict:
    url = f"https://api.github.com/repos/{REPO}/releases/latest"
    req = urllib.request.Request(url)
    req.add_header("User-Agent", "wren-lsp-sublime")
    req.add_header("Accept", "application/vnd.github.v3+json")
    with urllib.request.urlopen(req, timeout=REQUEST_TIMEOUT) as response:
        return json.loads(response.read().decode('utf-8'))


def download_binary(download_url: str, binary_path: Path) -> None:
    temp_path = binary_path.with_suffix('.tmp')
    req = urllib.request.Request(download_url)
    req.add_header("User-Agent", "wren-lsp-sublime")
    with urllib.request.urlopen(req, timeout=REQUEST_TIMEOUT) as response:
        temp_path.parent.mkdir(parents=True, exist_ok=True)
        with open(temp_path, 'wb') as f:
            while chunk := response.read(8192):
                f.write(chunk)
    if binary_path.exists():
        binary_path.unlink()
    temp_path.rename(binary_path)
    if plat.system().lower() != 'windows':
        binary_path.chmod(0o755)


class WrenLsp(AbstractPlugin):

    @classmethod
    def name(cls) -> str:
        return "wren-lsp"

    @classmethod
    def basedir(cls) -> str:
        return os.path.dirname(__file__)

    @classmethod
    def server_version(cls) -> str:
        return get_installed_version() or "unknown"

    @classmethod
    def current_server_version(cls) -> Optional[str]:
        return get_installed_version()

    @classmethod
    def needs_update_or_installation(cls) -> bool:
        if get_binary_path().exists():
            return False
        if shutil.which("wren-lsp"):
            return False
        return True

    @classmethod
    def install_or_update(cls) -> None:
        asset_name = get_platform_asset()
        if not asset_name:
            raise Exception(f"Unsupported platform: {plat.system()}-{plat.machine()}")

        release = fetch_latest_release()
        version = release.get('tag_name') or "unknown"
        if not release.get('assets'):
            raise Exception("Latest release has no assets")

        download_url = None
        for asset in release['assets']:
            if asset['name'] == asset_name:
                download_url = asset['browser_download_url']
                break

        if not download_url:
            raise Exception(f"No binary for platform: {plat.system()}-{plat.machine()}")

        download_binary(download_url, get_binary_path())
        save_installed_version(version)

    @classmethod
    def additional_variables(cls) -> Optional[dict]:
        binary_path = get_binary_path()
        if binary_path.exists():
            return {"binary_path": str(binary_path)}
        wren_lsp_in_path = shutil.which("wren-lsp")
        if wren_lsp_in_path:
            return {"binary_path": wren_lsp_in_path}
        return {"binary_path": str(binary_path)}


class WrenLspUpdateCommand(sublime_plugin.ApplicationCommand):

    def run(self) -> None:
        thread = threading.Thread(target=self._update, daemon=True)
        thread.start()

    def _update(self) -> None:
        try:
            sublime.set_timeout(
                lambda: sublime.status_message("WrenLSP: Checking for updates..."), 0
            )
            installed = get_installed_version()
            release = fetch_latest_release()
            latest = release.get('tag_name')

            if installed == latest:
                sublime.set_timeout(
                    lambda: sublime.message_dialog(f"WrenLSP: Already up to date ({latest})"), 0
                )
                return

            WrenLsp.install_or_update()
            msg = f"WrenLSP: Updated from {installed or 'unknown'} to {latest}. Restart LSP to apply."
            sublime.set_timeout(lambda: sublime.message_dialog(msg), 0)
        except Exception as e:
            sublime.set_timeout(
                lambda: sublime.error_message(f"WrenLSP: Failed to update: {e}"), 0
            )


def plugin_loaded() -> None:
    register_plugin(WrenLsp)


def plugin_unloaded() -> None:
    unregister_plugin(WrenLsp)
