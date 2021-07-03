--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

-- Tokyonight
vim.g.tokyonight_style = "storm"
vim.g.tokyonight_terminal_colors = true
vim.g.tokyonight_italic_comments = true
vim.g.tokyonight_italic_keywords = true
vim.g.tokyonight_italic_functions = true
vim.g.tokyonight_italic_variables = true
vim.g.tokyonight_transparent = false
vim.g.tokyonight_hide_inactive_statusline = true
vim.g.tokyonight_dark_sidebar = true
vim.g.tokyonight_dark_float = true

-- Moonlight
vim.g.moonlight_italic_comments = true
vim.g.moonlight_italic_keywords = true
vim.g.moonlight_italic_functions = true
vim.g.moonlight_italic_variables = true
vim.g.moonlight_contrast = true
vim.g.moonlight_borders = false
vim.g.moonlight_disable_background = false

local M = {}

M.lualine_colorscheme = "onedark"
M.colorscheme = "onedark"

-- M.lualine_colorscheme = "tokyonight"
-- M.colorscheme = "tokyonight"

-- M.lualine_colorscheme = "moonlight"
-- M.colorscheme = "moonlight"

return M
