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
--  This is the main file init.vim, here we can source all the other config
--  files. The idea is to keep things more modular and extensible.
--

-- Plugins list
require('plugins')

-- General neovim configuration via sets
require('config.sets')

-- Autocommands
require('config.autocommands')

-- All the keymaps
require('config.keymaps')


-- Plugin-specific configs
require('plug.lspconfig')
require('plug.treesitter')
require('plug.telescope')
require('plug.lualine')
require('plug.colorizer')
require('plug.gitsigns')
require('plug.completion')
require('plug.ultisnips')
require('plug.tree')
require('plug.indent')
require('plug.barbar')
