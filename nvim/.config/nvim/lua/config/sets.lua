--
--            _
--   ___  ___| |_ ___
--  / __|/ _ \ __/ __|
--  \__ \  __/ |_\__ \
--  |___/\___|\__|___/
--
--

-- TODO: util to set options

-- Spellcheck
vim.o.spelllang = "pt,en,la"
vim.bo.spelllang = "pt,en,la"

-- General config
vim.o.encoding = "utf-8"
vim.o.smartindent = true
vim.o.autoindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.linebreak = true
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.updatetime = 500
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.inccommand = "nosplit"
vim.o.scrolloff = 8
vim.o.hidden = true
vim.o.cmdheight = 1
vim.o.termguicolors = true
vim.o.timeoutlen = 400

-- vim.wo.colorcolumn = "80,120"
vim.wo.relativenumber = true
vim.wo.number = true
vim.wo.wrap = false
vim.wo.signcolumn = "auto"

-- Tabs
vim.o.tabstop = 2
vim.bo.tabstop = 2
vim.o.shiftwidth = 2
vim.bo.shiftwidth = 2
vim.o.expandtab = true
vim.bo.expandtab = true

-- Clipboard
vim.o.clipboard = "unnamedplus"

-- Folding
-- vim.wo.foldmethod = "expr"
-- vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
vim.wo.foldenable = false

-- Making floating and popup windows transparent
vim.go.pumblend = 10
vim.go.winblend = 10

-- Conceal
vim.wo.conceallevel = 2

-- Netrw
vim.g.netrw_liststyle = 3
vim.g.netrw_banner = 0
