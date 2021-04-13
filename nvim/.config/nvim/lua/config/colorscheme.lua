--            _                     _
--   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
--  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
-- | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
--  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|
--
--

-- Gruvbox config
vim.g.gruvbox_italic=1
vim.g.gruvbox_bold=1
vim.g.gruvbox_contrast_dark="hard"
vim.g.gruvbox_termcolors=16


local M = {}

M.colorscheme = 'onedark'

vim.cmd('colorscheme '..M.colorscheme)

return M
