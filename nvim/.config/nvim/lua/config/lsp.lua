-- Updating diagnostics. See: https://github.com/folke/dot/blob/master/config/nvim/lua/config/lsp/diagnostics.lua
vim.lsp.handlers["textDocument/publishDiagnostics"] =
  vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  {
    underline = true,
    update_in_insert = false,
    -- virtual_text = {spacing = 4, prefix = "●"},
    virtual_text = false,
    severity_sort = true
  }
)
