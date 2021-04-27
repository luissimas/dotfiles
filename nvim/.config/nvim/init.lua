--                         _                              __ _
--   _ __   ___  _____   _(_)_ __ ___     ___ ___  _ __  / _(_) __ _
--  | '_ \ / _ \/ _ \ \ / / | '_ ` _ \   / __/ _ \| '_ \| |_| |/ _` |
--  | | | |  __/ (_) \ V /| | | | | | | | (_| (_) | | | |  _| | (_| |
--  |_| |_|\___|\___/ \_/ |_|_| |_| |_|  \___\___/|_| |_|_| |_|\__, |
--                                                             |___/
--  Github: https://github.com/luissimas
--
--
--
-- This is the main file init.lua, here we can source all the other config
--  files. The idea is to keep things more modular and extensible.
--

-- Plugins list
require('plugins')

-- General
require('config.sets')
require('config.autocommands')
require('config.keymaps')

-- Setting colorscheme
vim.cmd('colorscheme '..require('config.colorscheme').colorscheme)

-- Plugin-specific configs
require('plug.lspconfig')
require('plug.treesitter')
require('plug.telescope')
require('plug.lualine')
require('plug.colorizer')
require('plug.gitsigns')
require('plug.compe')
require('plug.ultisnips')
require('plug.tree')
require('plug.indent')
require('plug.barbar')
require('plug.pandoc')
require('plug.autopairs')
require('plug.toggleterm')
require('plug.neoscroll')
require('plug.icons')
require('plug.dashboard')
require('plug.closetag')
require('plug.dadbod')
require('plug.neogit')
require('plug.formatter')
require('plug.saga')
