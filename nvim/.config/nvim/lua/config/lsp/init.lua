local options = require("config.lsp.options")

-- Updating diagnostics. See: https://github.com/neovim/nvim-lspconfig/wiki/UI-customization
vim.lsp.handlers["textDocument/publishDiagnostics"] = function(_, _, params, client_id, _)
  local config = {
    -- your config
    underline = true,
    update_in_insert = true,
    signs = true,
    virtual_text = { spacing = 4, prefix = "●" },
    -- virtual_text = false,
    severity_sort = true,
  }
  local uri = params.uri
  local bufnr = vim.uri_to_bufnr(uri)

  if not bufnr then
    return
  end

  local diagnostics = params.diagnostics
  print(vim.inspect(diagnostics))

  for i, v in ipairs(diagnostics) do
    if v.source then
      diagnostics[i].message = string.format("%s: %s", v.source, v.message)
    end
  end

  vim.lsp.diagnostic.save(diagnostics, bufnr, client_id)

  if not vim.api.nvim_buf_is_loaded(bufnr) then
    return
  end

  vim.lsp.diagnostic.display(diagnostics, bufnr, client_id, config)
end

-- Overriding default signs
local signs = { Error = " ", Warning = " ", Hint = " ", Information = " " }

for type, icon in pairs(signs) do
  local hl = "LspDiagnosticsSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- Adding borders for signature help and hover popup
vim.cmd([[autocmd ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.cmd([[autocmd ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, options)
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.hover, options)

-- Show diagnostics on cursor hold
-- vim.cmd("autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics(require('config.lsp.options'))")
