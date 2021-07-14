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
    severity_sort = false
  }
)

--vim.lsp.handlers["textDocument/hover"] = function(_, method, result, _, _, config)
--  if not (result and result.contents) then
--    return
--  end

--  config = config or {}
--  config.focus_id = method

--  print(vim.inspect(result))

--  local markdown_lines = util.convert_input_to_markdown_lines(result.contents)
--  markdown_lines = util.trim_empty_lines(markdown_lines)

--  if vim.tbl_isempty(markdown_lines) then
--    return
--  end

--  --return open_split_preview(markdown_lines, "markdown", config)
--  return util.open_floating_preview(markdown_lines, "markdown", config)
--end

function open_split_preview(contents, syntax, opts)
  vim.validate {
    contents = {contents, "t"},
    syntax = {syntax, "s", true},
    opts = {opts, "t", true}
  }

  opts = opts or {}
  opts.wrap = opts.wrap ~= false -- wrapping by default
  opts.stylize_markdown = opts.stylize_markdown ~= false
  opts.close_events = opts.close_events or {"CursorMoved", "CursorMovedI", "BufHidden", "InsertCharPre"}

  local bufnr = vim.api.nvim_get_current_buf()

  vim.cmd("split")

  local split_bufnr = vim.api.nvim_create_buf(false, true)
  local split_winnr = vim.api.nvim_get_current_win()

  local do_stylize = syntax == "markdown" and opts.stylize_markdown

  if do_stylize then
    -- applies the syntax and sets the lines to the buffer
    contents = util.stylize_markdown(split_bufnr, contents, opts)
  else
    if syntax then
      vim.api.nvim_buf_set_option(split_bufnr, "syntax", syntax)
    end
    vim.api.nvim_buf_set_lines(split_bufnr, 0, -1, true, contents)
  end

  vim.api.nvim_win_set_buf(split_winnr, split_bufnr)

  -- disable folding
  vim.api.nvim_win_set_option(split_winnr, "foldenable", false)
  -- soft wrapping
  vim.api.nvim_win_set_option(split_winnr, "wrap", opts.wrap)

  vim.api.nvim_buf_set_option(split_bufnr, "modifiable", false)
  vim.api.nvim_buf_set_option(split_bufnr, "bufhidden", "wipe")
  vim.api.nvim_buf_set_keymap(split_bufnr, "n", "q", "<cmd>bdelete<cr>", {silent = true, noremap = true})

  -- save focus_id
  if opts.focus_id then
    vim.api.nvim_win_set_var(split_winnr, opts.focus_id, bufnr)
  end

  return split_winnr, split_bufnr
end

-- Show diagnostics on cursor hold
vim.cmd("autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()")
-- vim.cmd("autocmd CursorHold * Lspsaga show_line_diagnostics")
