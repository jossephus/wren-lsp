# LSP for Wren

WIP LSP implementation for Wren.

### Try It

This is pretty much work in progress, but if you want to try it out, try to use the executables from releases and configure it according to your favorite editor.

#### Neovim

```lua

vim.filetype.add({
  extension = {
    wren = "wren"
  }
})



vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("wren", {}),
	pattern = "wren",
	callback = function()
		vim.lsp.start({
			name = "Wren LSP",
			cmd = { "path-to-wren-lsp" },
			root_dir = vim.loop.cwd(),
			flags = { exit_timeout = 1000 },
		})
	end,


```

#### Helix

add this to ~/.config/helix/languages.toml

```toml

[language-server.wren-lsp]
command = "path-to-wren-lsp"

[[language]]
name = "wren"
scope = "text.wren"
file-types = ["wren"]
comment-token = "//"
language-servers = ["wren-lsp"]

```
