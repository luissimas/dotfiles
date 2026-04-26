-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")
vim.api.nvim_create_autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    -- Skip toggle on floating windows. Makes it behave nicely for LSP hover pop ups.
    if vim.api.nvim_win_get_config(0).zindex then
      return
    end

    require("no-neck-pain").enable()
  end,
})

vim.api.nvim_create_autocmd({ "VimEnter", "FocusGained" }, {
  group = vim.api.nvim_create_augroup("luis_theme", { clear = true }),
  -- `nested` so the `:colorscheme` call inside `apply()` actually fires the
  -- `ColorScheme` autocmd below (and lualine's own listeners). Without this,
  -- nested autocmds are suppressed and the statusline keeps the stale palette.
  nested = true,
  callback = function()
    require("config.theme").apply()
  end,
})

-- Lualine reads its theme at setup time and doesn't fully re-detect on
-- `ColorScheme` alone, so re-run setup with `theme = "auto"` after every
-- colorscheme change to refresh the statusline palette.
vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("luis_theme_lualine", { clear = true }),
  callback = function()
    local ok, lualine = pcall(require, "lualine")
    if ok then
      lualine.setup({ options = { theme = "auto" } })
    end
  end,
})
