# Wren for Zed

Wren language support for the [Zed](https://zed.dev) editor, powered by [wren-lsp](https://github.com/jossephus/wren-lsp).

## Features

- Syntax highlighting via [tree-sitter-wren](https://github.com/jossephus/tree-sitter-wren)
- Bracket matching and auto-indentation
- Code outline
- LSP support via wren-lsp (auto-downloaded from GitHub releases if its not globally available):
  - Diagnostics
  - Go to definition
  - Completions
  - Hover information

## Installation
### As a Dev Extension

1. Clone this repository
2. In Zed, run `zed: install dev extension` and select the `editors/zed` directory

The extension uses `wren-lsp` from your system `PATH` if available; otherwise it downloads a managed binary.

## Supported Platforms

- macOS (Apple Silicon & Intel)
- Linux (x86_64 & aarch64)
- Windows (x86_64 & aarch64)
