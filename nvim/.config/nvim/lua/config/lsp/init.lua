local options = require("config.lsp.options")

-- Overriding default signs
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- Adding borders for signature help and hover popup
vim.cmd([[autocmd ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.cmd([[autocmd ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, options)
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.hover, options)
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = {
    source = "always", -- Or "if_many"
    prefix = "●", -- Could be '●', '▎', 'x'
  },
  signs = true,
  update_in_insert = true,
})

-- Show diagnostics on cursor hold
-- vim.cmd("autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics(require('config.lsp.options'))")
