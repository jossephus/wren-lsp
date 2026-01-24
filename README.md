# LSP for Wren

WIP LSP implementation for Wren.

## Features

- [x] Diagnostics (syntax + basic semantic checks)
- [x] Completion (identifiers, members, builtins, class methods)
- [x] Hover
- [x] Go to definition
- [x] References
- [x] Rename
- [x] Semantic tokens

## Roadmap

- [ ] Document symbols (outline)
- [ ] Signature help
- [ ] Document highlights
- [ ] Code actions (quick fixes)
- [ ] Workspace symbols
- [ ] Folding ranges
- [ ] Selection range
- [ ] Inlay hints
- [ ] Cross-module navigation

## Releases

Download the matching binary from GitHub Releases:

- `wren-lsp-linux-x86_64`
- `wren-lsp-linux-aarch64`
- `wren-lsp-macos-x86_64`
- `wren-lsp-macos-aarch64`
- `wren-lsp-windows-x86_64.exe`
- `wren-lsp-windows-aarch64.exe`

Make the binary executable and put it on your `PATH`.

## Editor Setup

### Neovim

#### With lspconfig (recommended)

```lua
vim.filetype.add({
  extension = {
    wren = "wren",
  },
})

local lspconfig = require("lspconfig")
local configs = require("lspconfig.configs")

if not configs.wren_lsp then
  configs.wren_lsp = {
    default_config = {
      cmd = { "wren-lsp" },
      filetypes = { "wren" },
      root_dir = lspconfig.util.root_pattern(".git"),
    },
  }
end

lspconfig.wren_lsp.setup({})
```

#### Without lspconfig

```lua
vim.filetype.add({
  extension = {
    wren = "wren",
  },
})

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("wren", {}),
  pattern = "wren",
  callback = function()
    vim.lsp.start({
      name = "Wren LSP",
      cmd = { "wren-lsp" },
      root_dir = vim.loop.cwd(),
      flags = { exit_timeout = 1000 },
    })
  end,
})
```

### VS Code

*Coming soon*

### Helix

Add this to `~/.config/helix/languages.toml`:

```toml
[language-server.wren-lsp]
command = "wren-lsp"

[[language]]
name = "wren"
scope = "text.wren"
file-types = ["wren"]
comment-token = "//"
language-servers = ["wren-lsp"]
```
