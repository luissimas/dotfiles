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

-- General
require("config.sets")
require("config.keymaps")

-- Plugins
require("plugins")

-- Setting colorscheme
vim.cmd("colorscheme " .. require("config.colorscheme").colorscheme)

-- Config for plugins TODO: Manage this better
require("plug.colorizer")
require("plug.compe")
require("plug.formatter")
require("plug.gitsigns")
require("plug.illuminate")
require("plug.lspconfig")
require("plug.lsp_signature")
require("plug.lspkind")
require("plug.lualine")
require("plug.pears")
require("plug.telescope")
require("plug.todocomments")
require("plug.tree")
require("plug.treesitter")
require("plug.trouble")
require("plug.ultisnips")
require("plug.webdevicons")
require("plug.whichkey")
require("plug.lsp_rooter")
require("plug.neogit")
require("plug.tmux")
require("plug.lspsaga")
