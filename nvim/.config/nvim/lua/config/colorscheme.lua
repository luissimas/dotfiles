--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

-- Gruvbox-flat
vim.g.gruvbox_flat_style = "dark"
vim.g.gruvbox_terminal_colors = true
vim.g.gruvbox_italic_comments = true
vim.g.gruvbox_italic_keywords = true
vim.g.gruvbox_italic_functions = true
vim.g.gruvbox_italic_variables = true
vim.g.gruvbox_transparent = false
vim.g.gruvbox_hide_inactive_statusline = true
vim.g.gruvbox_dark_sidebar = true
vim.g.gruvbox_dark_float = true

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

-- Material
vim.g.material_style = "deep ocean"
vim.g.material_italic_comments = true
vim.g.material_italic_keywords = true
vim.g.material_italic_functions = true
vim.g.material_italic_variables = true
vim.g.material_contrast = true
vim.g.material_borders = false
vim.g.material_disable_background = false

local M = {}

-- M.lualine_colorscheme = "gruvbox-flat"
-- M.colorscheme = "gruvbox-flat"

M.lualine_colorscheme = "tokyonight"
M.colorscheme = "tokyonight"

-- M.lualine_colorscheme = "material"
-- M.colorscheme = "material"

return M
