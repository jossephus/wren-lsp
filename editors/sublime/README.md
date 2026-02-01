# Wren Language Support for Sublime Text

This package provides language support for [Wren](https://wren.io/) via [wren-lsp](https://github.com/jossephus/wren-lsp).

## Features

- Syntax highlighting
- Diagnostics (syntax + semantic checks)
- Code completion
- Hover information
- Go to definition
- Find references
- Rename symbol
- Semantic tokens
- Document symbols (outline)
- Signature help
- Document highlights
- Code actions (quick fixes)
- Workspace symbols
- Folding ranges
- Selection range
- Inlay hints (type hints)

## Requirements

- [Sublime Text 4](https://www.sublimetext.com/) (build 4000+)
- [LSP](https://packagecontrol.io/packages/LSP) package (install via Package Control)
- [wren-lsp](https://github.com/jossephus/wren-lsp) binary

## Installation

### Via Package Control (Recommended - when published)

1. Open Command Palette (`Cmd/Ctrl+Shift+P`)
2. Type "Package Control: Install Package"
3. Search for "Wren LSP"

### Manual Installation

1. Download/clone this repository
2. Copy/move the folder to your Sublime Text Packages directory:
   - macOS: `~/Library/Application Support/Sublime Text/Packages/`
   - Linux: `~/.config/sublime-text/Packages/`
   - Windows: `%APPDATA%/Sublime Text/Packages/`

### LSP Binary

The extension will attempt to auto-download the appropriate `wren-lsp` binary for your platform from GitHub releases. Alternatively, you can:

1. Download manually from [releases](https://github.com/jossephus/wren-lsp/releases)
2. Place it in your PATH, or
3. Set the path in settings (see below)

## Configuration

Access settings via: `Preferences > Package Settings > LSP-wren-lsp > Settings`

**Available settings:**

| Setting | Description | Default |
|---------|-------------|---------|
| `command` | Path to wren-lsp executable | `["wren-lsp"]` |
| `enabled` | Enable/disable the language server | `true` |
| `auto_download` | Auto-download LSP binary from GitHub | `true` |

**Example custom configuration:**

```json
{
    "command": ["/usr/local/bin/wren-lsp"],
    "enabled": true,
    "auto_download": true
}
```

## Usage

1. Open a `.wren` file
2. LSP will automatically start the language server
3. Use LSP features via:
   - Command Palette (`Cmd/Ctrl+Shift+P`)
   - Right-click context menu
   - Keyboard shortcuts (see LSP package documentation)

## Building

To create a `.sublime-package` file for distribution:

```bash
# From the editors/sublime directory
zip -r WrenLSP.sublime-package . -x "*.git*" -x "*.DS_Store" -x "README.md"
```

Install via: `Preferences > Browse Packages...` (place in `Installed Packages/` folder)

## Troubleshooting

**LSP not starting?**
- Check the Sublime Text console (`View > Show Console`) for errors
- Ensure `wren-lsp` is in your PATH or set the correct path in settings
- Try disabling auto-download if behind a proxy

**Syntax highlighting issues?**
- Ensure no other Wren syntax packages are conflicting
- Check the scope in status bar (should show `source.wren`)

## License

MIT
