--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

local v = vim.g

-- Tokyonight
v.tokyonight_style = "night"
v.tokyonight_terminal_colors = true
v.tokyonight_italic_comments = true
v.tokyonight_italic_keywords = true
v.tokyonight_italic_functions = true
v.tokyonight_italic_variables = true
v.tokyonight_transparent = false
v.tokyonight_hide_inactive_statusline = true
v.tokyonight_dark_sidebar = false
v.tokyonight_dark_float = false
v.tokyonight_lualine_bold = false

v.tokyonight_colors = {
	bg_statusline = "bg",
}

vim.g.rose_pine_variant = "base"
vim.g.rose_pine_enable_italics = true
vim.g.rose_pine_disable_background = false

local M = {}

M.lualine_colorscheme = "rose-pine"
M.colorscheme = "rose-pine"

return M
