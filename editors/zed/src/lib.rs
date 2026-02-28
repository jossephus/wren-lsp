use std::fs;
use zed_extension_api::{self as zed, process, LanguageServerId, Result};

struct WrenExtension {
    cached_binary_path: Option<String>,
    has_checked_for_update: bool,
}

const REPO: &str = "jossephus/wren-lsp";
const VERSION_FILE: &str = "version.txt";

impl WrenExtension {
    fn binary_name(platform: zed::Os) -> &'static str {
        match platform {
            zed::Os::Windows => "wren-lsp.exe",
            zed::Os::Mac | zed::Os::Linux => "wren-lsp",
        }
    }

    fn read_installed_version() -> Option<String> {
        let version = fs::read_to_string(VERSION_FILE).ok()?;
        let version = version.trim();
        if version.is_empty() {
            None
        } else {
            Some(version.to_string())
        }
    }

    fn write_installed_version(version: &str) {
        let _ = fs::write(VERSION_FILE, version);
    }

    fn asset_name(platform: zed::Os, arch: zed::Architecture) -> Result<String> {
        Ok(format!(
            "wren-lsp-{os}-{arch}",
            os = match platform {
                zed::Os::Mac => "macos",
                zed::Os::Linux => "linux",
                zed::Os::Windows => "windows",
            },
            arch = match arch {
                zed::Architecture::Aarch64 => "aarch64",
                zed::Architecture::X8664 => "x86_64",
                zed::Architecture::X86 => return Err("x86 (32-bit) is not supported".into()),
            },
        ) + if matches!(platform, zed::Os::Windows) {
            ".exe"
        } else {
            ""
        })
    }

    fn latest_release() -> Result<zed::GithubRelease> {
        zed::latest_github_release(
            REPO,
            zed::GithubReleaseOptions {
                require_assets: true,
                pre_release: false,
            },
        )
    }

    fn install_release_binary(
        platform: zed::Os,
        arch: zed::Architecture,
        binary_path: &str,
        release: &zed::GithubRelease,
    ) -> Result<()> {
        let asset_name = Self::asset_name(platform, arch)?;
        let asset = release
            .assets
            .iter()
            .find(|a| a.name == asset_name)
            .ok_or_else(|| format!("no asset found matching {asset_name}"))?;

        let tmp_path = format!("{binary_path}.tmp");
        let _ = fs::remove_file(&tmp_path);

        zed::download_file(
            &asset.download_url,
            &tmp_path,
            zed::DownloadedFileType::Uncompressed,
        )
        .map_err(|e| format!("failed to download file: {e}"))?;

        if !matches!(platform, zed::Os::Windows) {
            zed::make_file_executable(&tmp_path)?;
        }

        fs::rename(&tmp_path, binary_path).map_err(|e| format!("failed to install binary: {e}"))?;
        Ok(())
    }

    fn is_system_binary_available(platform: zed::Os) -> bool {
        let binary_name = Self::binary_name(platform);
        let mut command = process::Command::new(binary_name).arg("--version");
        command
            .output()
            .is_ok_and(|output| output.status == Some(0))
    }

    fn try_update_existing_binary(
        &mut self,
        language_server_id: &LanguageServerId,
        platform: zed::Os,
        arch: zed::Architecture,
        binary_path: &str,
    ) {
        if self.has_checked_for_update {
            return;
        }

        self.has_checked_for_update = true;
        zed::set_language_server_installation_status(
            language_server_id,
            &zed::LanguageServerInstallationStatus::CheckingForUpdate,
        );

        let release = match Self::latest_release() {
            Ok(release) => release,
            Err(_) => return,
        };

        if Self::read_installed_version().as_deref() == Some(release.version.as_str()) {
            return;
        }

        zed::set_language_server_installation_status(
            language_server_id,
            &zed::LanguageServerInstallationStatus::Downloading,
        );

        if Self::install_release_binary(platform, arch, binary_path, &release).is_ok() {
            Self::write_installed_version(&release.version);
        }
    }

    fn language_server_binary_path(
        &mut self,
        language_server_id: &LanguageServerId,
    ) -> Result<String> {
        let (platform, arch) = zed::current_platform();
        let managed_binary_path = Self::binary_name(platform).to_string();

        if let Some(path) = self.cached_binary_path.clone() {
            if fs::metadata(&path).is_ok_and(|m| m.is_file()) {
                self.try_update_existing_binary(language_server_id, platform, arch, &path);
                return Ok(path);
            }
        }

        if fs::metadata(&managed_binary_path).is_ok_and(|m| m.is_file()) {
            self.cached_binary_path = Some(managed_binary_path.clone());
            self.try_update_existing_binary(
                language_server_id,
                platform,
                arch,
                &managed_binary_path,
            );
            return Ok(managed_binary_path);
        }

        if Self::is_system_binary_available(platform) {
            self.cached_binary_path = None;
            self.has_checked_for_update = false;
            return Ok(Self::binary_name(platform).to_string());
        }

        zed::set_language_server_installation_status(
            language_server_id,
            &zed::LanguageServerInstallationStatus::CheckingForUpdate,
        );

        let release = Self::latest_release()?;

        zed::set_language_server_installation_status(
            language_server_id,
            &zed::LanguageServerInstallationStatus::Downloading,
        );

        Self::install_release_binary(platform, arch, &managed_binary_path, &release)?;
        Self::write_installed_version(&release.version);

        self.cached_binary_path = Some(managed_binary_path.clone());
        self.has_checked_for_update = true;
        Ok(managed_binary_path)
    }
}

impl zed::Extension for WrenExtension {
    fn new() -> Self {
        WrenExtension {
            cached_binary_path: None,
            has_checked_for_update: false,
        }
    }

    fn language_server_command(
        &mut self,
        language_server_id: &LanguageServerId,
        _worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let binary_path = self.language_server_binary_path(language_server_id)?;

        Ok(zed::Command {
            command: binary_path,
            args: vec![],
            env: vec![],
        })
    }
}

zed::register_extension!(WrenExtension);
