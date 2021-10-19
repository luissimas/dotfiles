--
--            _
--   ___  ___| |_ ___
--  / __|/ _ \ __/ __|
--  \__ \  __/ |_\__ \
--  |___/\___|\__|___/
--
--

local opt = vim.opt

-- Spellcheck
opt.spelllang = "pt,en,la"
opt.spelllang = "pt,en,la"

-- General config
opt.encoding = "utf-8"
opt.smartindent = true
opt.ignorecase = true
opt.smartcase = true
opt.linebreak = true
opt.splitbelow = true
opt.splitright = true
opt.updatetime = 500
opt.incsearch = true
opt.hlsearch = false
opt.inccommand = "nosplit"
opt.scrolloff = 8
opt.hidden = true
opt.termguicolors = true
opt.timeoutlen = 400
opt.cmdheight = 1
opt.showmode = false
opt.equalalways = false
-- opt.colorcolumn = "80,120"
opt.relativenumber = true
opt.number = true
opt.wrap = false
opt.signcolumn = "auto"

-- Tabs
opt.tabstop = 2
opt.shiftwidth = 2
opt.softtabstop = 2
opt.expandtab = true
opt.autoindent = true

-- Clipboard
opt.clipboard = "unnamedplus"

-- Folding
-- opt.foldmethod = "expr"
-- opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldenable = false

-- Making floating and popup windows transparent
opt.pumblend = 10
opt.winblend = 10

-- Conceal
opt.conceallevel = 2

-- Netrw
vim.g.netrw_liststyle = 3
vim.g.netrw_banner = 0
