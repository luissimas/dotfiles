local util = require("vim.lsp.util")

-- Updating diagnostics. See: https://github.com/folke/dot/blob/master/config/nvim/lua/config/lsp/diagnostics.lua
vim.lsp.handlers["textDocument/publishDiagnostics"] =
  vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics,
  {
    underline = true,
    update_in_insert = true,
    signs = true,
    -- virtual_text = {spacing = 4, prefix = "●"},
    virtual_text = false,
    severity_sort = true
  }
)

local signs = {Error = " ", Warning = " ", Hint = " ", Information = " "}

for type, icon in pairs(signs) do
  local hl = "LspDiagnosticsSign" .. type
  vim.fn.sign_define(hl, {text = icon, texthl = hl, numhl = ""})
end

-- vim.lsp.handlers["textDocument/hover"] =
--   vim.lsp.with(
--   vim.lsp.handlers.hover,
--   {
--     border = "single"
--   }
-- )

-- Show diagnostics on cursor hold
vim.cmd("autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()")
