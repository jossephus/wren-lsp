# Wren Language Support for VS Code

This extension provides language support for [Wren](https://wren.io/) via [wren-lsp](https://github.com/jossephus/wren-lsp).

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

You need to have `wren-lsp` installed and available in your PATH, or configure the path via settings.

### Installing wren-lsp

Download from [releases](https://github.com/jossephus/wren-lsp/releases) or build from source.

## Extension Settings

- `wren.serverPath`: Path to the wren-lsp executable (default: `wren-lsp`)
- `wren.trace.server`: Traces communication between VS Code and the language server

## Building

```bash
npm install
npm run compile
npm run package  # creates .vsix file
```

Install the `.vsix` file via: Extensions → ⋯ → Install from VSIX...
