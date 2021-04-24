--   _
--  | | _____ _   _ _ __ ___   __ _ _ __  ___
--  | |/ / _ \ | | | '_ ` _ \ / _` | '_ \/ __|
--  |   <  __/ |_| | | | | | | (_| | |_) \__ \
--  |_|\_\___|\__, |_| |_| |_|\__,_| .__/|___/
--            |___/                |_|
--

vim.g.mapleader = " "

-- Resource config
vim.api.nvim_set_keymap('n', '<leader>r', ':luafile ~/.config/nvim/init.lua<Enter> :PackerInstall<Enter>', { noremap = true, silent=false })

-- Close buffer and keep the split
vim.api.nvim_set_keymap('n', '<leader>c', ':bp <bar> vs <bar> bn <bar> bd <Enter>', { noremap = true })

-- Close window
vim.api.nvim_set_keymap('n', '<leader>q', ':q<Enter>', { noremap = true, silent = true })
-- Save buffer
vim.api.nvim_set_keymap('n', '<leader>w', ':w<Enter>', { noremap = true, silent = false })

-- Open vertical split
vim.api.nvim_set_keymap('n', '<C-s>', ':vsplit<Enter>', { noremap = true, silent = true })

-- Toggle file tree
vim.api.nvim_set_keymap('n', '<leader>e', ':NvimTreeToggle<Enter>', { noremap = true, silent = true })
-- Enable spellcheck
vim.api.nvim_set_keymap('n', '<leader>sk', ':set spell!<Enter>', { noremap = true, silent = true })

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap('n', '<S-l>', '$', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<S-h>', '0', { noremap = true, silent = true })

-- Auto spell corretion
vim.api.nvim_set_keymap('i', '<C-l>', '<Esc>[s1z=`]i', { noremap = true, silent = true })

--Lsp
vim.api.nvim_set_keymap('n', 'gd', ':lua vim.lsp.buf.definition()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'K', ':lua vim.lsp.buf.hover()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>rn', ':lua vim.lsp.buf.rename()<Enter>', { noremap = true, silent = true })
--vim.api.nvim_set_keymap('n', '<leader>ca', ':lua vim.lsp.buf.code_action()<Enter>', { noremap = true, silent = true })
vim.api.nvim_command('autocmd FileType cpp nnoremap <silent> <leader>sh :ClangdSwitchSourceHeader<Enter>')
vim.api.nvim_set_keymap('n', '<C-f>', ':lua vim.lsp.buf.formatting()<Enter>', { noremap = true, silent = true })

-- Completion (setting <Tab> and <Shift-tab> to navigate through completion popup menu)
vim.api.nvim_set_keymap('i', '<Tab>', 'pumvisible() ? "\\<C-n>" : "\\<Tab>"', { expr = true, noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<S-Tab>', 'pumvisible() ? "\\<C-p>" : "\\<S-Tab>"', { expr = true, noremap = true, silent = true })
vim.cmd('inoremap <silent><expr> <C-Space> compe#complete()')
vim.cmd('inoremap <silent><expr> <CR>      compe#confirm("<CR>")')

-- UltiSnips edit snippets
vim.api.nvim_set_keymap('n', '<leader>se', ':UltiSnipsEdit<Enter>', { noremap = true, silent = true })

-- Prettier
vim.api.nvim_command('autocmd FileType javascript,html,css,json,yaml,typescript nnoremap <C-f> :Prettier<Enter>')

-- Telescope
-- vim.api.nvim_set_keymap('n', '<leader>ff', ':lua require("telescope.builtin").find_files(require("telescope.themes").get_dropdown({ width=120, results_height = 5, preview_height = 200 }))<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ff', ':Telescope find_files<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fo', ':Telescope oldfiles<Enter>', { noremap = true, silent = true })

vim.api.nvim_set_keymap('n', '<leader>fd', ':lua require("telescope_custom.find").find_dotfiles()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fh', ':lua require("telescope_custom.find").find_home()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fv', ':lua require("telescope_custom.find").find_vault()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fc', ':lua require("telescope_custom.colorscheme").colorscheme()<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fw', ':lua require("telescope_custom.colorscheme").wal()<Enter>', { noremap = true, silent = true })

-- Vim-fugitive
vim.api.nvim_set_keymap('n', '<leader>g', ':Git<Enter>', { noremap = true, silent = true })

-- Barbar tabline navigation
vim.api.nvim_set_keymap('n', '<C-o>', ':BufferNext<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-i>', ':BufferPrevious<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-o>', ':BufferMoveNext<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-i>', ':BufferMovePrev<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>c', ':BufferClose<Enter>', { noremap = true, silent = true })

-- Navigation and resizing splits
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-h>', ':vertical resize +5<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-j>', ':resize +5<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-k>', ':resize -5<Enter>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-l>', ':vertical resize -5<Enter>', { noremap = true, silent = true })


-- <leader>o and <leader>of binded in autocommands.vim


-- Latin long vowels
vim.api.nvim_set_keymap('i', 'ä', 'ā', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ä', 'Ā', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'ë', 'ē', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ë', 'Ē', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'ï', 'ī', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ï', 'Ī', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'ö', 'ō', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ö', 'Ō', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'ü', 'ū', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ü', 'Ū', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'ÿ', 'ȳ', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', 'Ÿ', 'Ȳ', { noremap = true, silent = true })
