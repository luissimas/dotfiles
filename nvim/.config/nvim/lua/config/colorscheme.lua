--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

-- Gruvbox
vim.g.gruvbox_italic = 1
vim.g.gruvbox_contrast_dark = "hard"
vim.g.gruvbox_hls_cursor = "gray"
vim.g.gruvbox_sign_column = "bg0"

-- Material-nvim
vim.g.material_style = "palenight"
vim.g.material_italic_comments = true
vim.g.material_italic_keywords = true
vim.g.material_italic_functions = true
vim.g.material_italic_variables = true
vim.g.material_contrast = false
vim.g.material_borders = true
vim.g.material_disable_background = false

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

local M = {}

-- M.lualine_colorscheme = "material-nvim"
-- M.colorscheme = "material"

M.lualine_colorscheme = "tokyonight"
M.colorscheme = "tokyonight"

return M
