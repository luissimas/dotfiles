--
--            _
--   ___  ___| |_ ___
--  / __|/ _ \ __/ __|
--  \__ \  __/ |_\__ \
--  |___/\___|\__|___/
--
--


-- Spellcheck
vim.o.spelllang='pt,en,la'

-- General config
vim.o.number = true
vim.o.relativenumber = true
vim.o.encoding='UTF-8'
vim.o.smartindent = true
vim.o.autoindent = true
vim.o.linebreak = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.updatetime = 1000
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.scrolloff = 8
vim.o.hidden = true
vim.o.cmdheight = 1
vim.o.signcolumn = 'false'
--vim.o.colorcolumn = '80,120'
vim.o.termguicolors = true
vim.o.timeoutlen = 500

vim.cmd('set nowrap')

-- Tabs
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true

-- Clipboard
vim.o.clipboard = 'unnamedplus'

-- Netrw
vim.g.netrw_liststyle = 3
vim.g.netrw_banner = 0
