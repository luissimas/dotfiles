-- Leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

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

-- Save and quit
vim.api.nvim_set_keymap("n", "<leader>w", ":w<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>q", ":q<Enter>", { noremap = true, silent = true })

-- Kill buffers without messing up the split
vim.api.nvim_set_keymap("n", "<leader>bk", ":bp <bar> vs <bar> bn <bar> bd<Enter>", { noremap = true, silent = true })

-- Splits
vim.api.nvim_set_keymap("n", "<C-s>", ":vsplit<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<C-x>", ":split<Enter>", { noremap = true, silent = true })

-- Movement with line wraps
vim.api.nvim_set_keymap("n", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("v", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("n", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("v", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap("n", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-h>", "0", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-h>", "0", { noremap = true, silent = true })

-- Close transient windows with "q"
vim.cmd [[
  augroup CloseWindows
    autocmd FileType help,qf,lspinfo map <buffer><silent> q :quit<Enter>
  augroup end
]]

-- Statusline
function _G.gitbranch()
  local branch = vim.fn.system("git -C " .. vim.fn.expand "%:h" .. " branch --show-current 2>/dev/null"):gsub("\n", "")

  if branch ~= "" then
    return string.format("[%s] ", branch)
  end

  return ""
end

-- vim.o.statusline = " %{v:lua.gitbranch()}%f %m %r %= %y %p%% "
vim.o.statusline = " %f %m %r %= %y %p%% "

-- Quickfixlist
vim.cmd [[
  augroup QuickFixSettings
    autocmd!
    autocmd BufWrite,BufEnter,InsertLeave * lua vim.diagnostic.setloclist({ open = false })
    autocmd FileType qf set nonumber
    autocmd FileType qf set norelativenumber
    autocmd FileType qf set signcolumn="no"
  augroup end
]]

vim.api.nvim_set_keymap("n", "<C-q>", ":copen<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<C-n>", ":cnext<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<C-p>", ":cprev<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>l", ":lopen<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>n", ":lnext<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":lprev<Enter>", { noremap = true, silent = true })

-- Trim whitespace on save
vim.cmd [[
  fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
  endfun

  augroup TrimWhitespace
    autocmd!
    autocmd BufWritePre * call TrimWhitespace()
  augroup end
]]

-- Highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank({ timeout = 100 })
  augroup end
]]

-- Disable comment continuation
vim.cmd [[
  augroup DisableCommentContinuation
    autocmd!
    autocmd BufEnter * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
  augroup end
]]

-- Netrw
vim.g.netrw_banner = 0
vim.g.netrw_winsize = -25
vim.g.netrw_browse_split = 4
vim.g.netrw_hide = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_list_hide = [[node_modules,\.git]]

vim.cmd [[
  augroup NetrwSettings
    autocmd!
    autocmd FileType netrw setlocal bufhidden=wipe
    autocmd FileType netrw setlocal statusline=%=
  augroup end
]]

vim.api.nvim_set_keymap("n", "<leader>e", ":Lexplore<Enter>", { noremap = true, silent = true })
