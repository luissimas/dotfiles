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
require("config.autocommands")
require("config.keymaps")

-- Plugins
require("plugins")

-- Setting colorscheme
vim.cmd("colorscheme " .. require("config.colorscheme").colorscheme)
