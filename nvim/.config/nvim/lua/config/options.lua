-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Enable mouse mode
vim.opt.mouse = "a"

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 8

-- Spellcheck
vim.opt.spelllang = "en_us,pt_br"

-- No wrap
vim.opt.wrap = false

-- No line highlight
vim.opt.cursorline = false

-- Make "." count as a word separator
vim.opt.iskeyword = "@,48-57,_,192-255"

-- Disable text conceal
vim.opt.conceallevel = 0
