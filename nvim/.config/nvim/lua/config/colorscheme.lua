--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

local v = vim.g

v.tokyonight_style = "storm"
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

v.rose_pine_variant = "base"
v.rose_pine_enable_italics = true
v.rose_pine_disable_background = false

v.nord_contrast = true
v.nord_borders = false
v.nord_disable_background = false
v.nord_italic = true

v.zenflesh_darkness = "stark"

local M = {}

M.colorscheme = "base16-material-palenight"
M.lualine_colorscheme = "palenight"

-- M.colorscheme = "tokyonight"
-- M.lualine_colorscheme = "tokyonight"

-- M.colorscheme = "zenflesh"
-- M.lualine_colorscheme = "zenflesh"

return M
