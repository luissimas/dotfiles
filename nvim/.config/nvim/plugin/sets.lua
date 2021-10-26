-- Tabs
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.smartindent = true

-- Number column
vim.o.relativenumber = true
vim.o.number = true
vim.o.numberwidth = 1

-- Search
vim.o.hlsearch = false
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true

-- Recover files
vim.o.swapfile = false
vim.o.undofile = true

-- Split orientation
vim.o.splitbelow = true
vim.o.splitright = true

-- Misc
vim.o.termguicolors = true -- enable true color support
vim.o.wrap = false -- no line wrap
vim.o.foldenable = false -- disable folding
vim.o.hidden = true -- buffers can be kept open in the background
vim.o.inccommand = "nosplit" -- visual incremental feedback for commands
vim.o.scrolloff = 8 -- scroll offset
vim.o.showmode = false -- don't show current mode in command area
vim.o.showtabline = 0 -- never show tablines
vim.o.signcolumn = "yes" -- set signcolumn display
vim.o.completeopt = "menuone,noinsert" -- completion options
-- vim.o.colorcolumn = "80" -- column for visual indent guideline
vim.o.updatetime = 200 -- time for CursorHold event
vim.o.clipboard = "unnamedplus" -- setting clipboard to system's
vim.opt.shortmess = vim.opt.shortmess + "c" -- disable completion item messages
