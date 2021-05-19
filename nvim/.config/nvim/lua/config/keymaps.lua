--   _
--  | | _____ _   _ _ __ ___   __ _ _ __  ___
--  | |/ / _ \ | | | '_ ` _ \ / _` | '_ \/ __|
--  |   <  __/ |_| | | | | | | (_| | |_) \__ \
--  |_|\_\___|\__, |_| |_| |_|\__,_| .__/|___/
--            |___/                |_|
--

vim.g.mapleader = " "

-- Reload file
vim.api.nvim_set_keymap("n", "<leader>r", ":edit<Enter>", {noremap = true, silent = false})

-- Close buffer and keep the split
-- vim.api.nvim_set_keymap("n", "<leader>c", ":bp <bar> vs <bar> bn <bar> bd <Enter>", {noremap = true})

-- Close window
vim.api.nvim_set_keymap("n", "<leader>q", ":q<Enter>", {noremap = true, silent = true})

-- Write buffer
vim.api.nvim_set_keymap("n", "<leader>w", ":w<Enter>", {noremap = true, silent = false})

-- Open vertical split
vim.api.nvim_set_keymap("n", "<C-s>", ":vsplit<Enter>", {noremap = true, silent = true})

-- Toggle file tree
vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<Enter>", {noremap = true, silent = true})
-- Enable spellcheck
vim.api.nvim_set_keymap("n", "<leader>sk", ":set spell!<Enter>", {noremap = true, silent = true})

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap("n", "<S-l>", "$", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<S-h>", "0", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "<S-l>", "$", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "<S-h>", "0", {noremap = true, silent = true})

-- Use j and k for visual lines
vim.api.nvim_set_keymap("n", "j", "gj", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "k", "gk", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "j", "gj", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "k", "gk", {noremap = true, silent = true})

-- Auto spell corretion
vim.api.nvim_set_keymap("i", "<C-l>", "<Esc>[s1z=`]a", {noremap = true, silent = true})

--Lsp
vim.api.nvim_set_keymap("n", "gd", ":lua vim.lsp.buf.definition()<Enter>", {noremap = true, silent = true})
vim.api.nvim_command("autocmd FileType c,cpp nnoremap <silent> <leader>sh :ClangdSwitchSourceHeader<Enter>")
vim.api.nvim_set_keymap("n", "<C-f>", ":lua vim.lsp.buf.formatting()<Enter>", {noremap = true, silent = true})

--Lspsaga
vim.api.nvim_set_keymap("n", "<leader>rn", ":Lspsaga rename<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "ca", ":Lspsaga code_action<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "K", ":Lspsaga hover_doc<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "D", ":Lspsaga preview_definition<Enter>", {noremap = true, silent = true})

-- Completion
vim.cmd("inoremap <silent><expr> <C-Space> compe#complete()")
vim.cmd('inoremap <silent><expr> <CR>      compe#confirm("<CR>")')

-- UltiSnips edit snippets
-- vim.api.nvim_set_keymap("n", "<leader>se", ":UltiSnipsEdit<Enter>", {noremap = true, silent = true})

-- Formatter
vim.api.nvim_command(
  "autocmd FileType javascript,html,css,json,yaml,typescript,lua,python nnoremap <C-f> :FormatWrite<Enter>"
)

-- Telescope
vim.api.nvim_set_keymap("n", "<leader>ff", ":Telescope find_files<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>fo", ":Telescope oldfiles<Enter>", {noremap = true, silent = true})

vim.api.nvim_set_keymap(
  "n",
  "<leader>fd",
  ':lua require("telescope_custom.find").find_dotfiles()<Enter>',
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fh",
  ':lua require("telescope_custom.find").find_home()<Enter>',
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fv",
  ':lua require("telescope_custom.find").find_vault()<Enter>',
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fc",
  ':lua require("telescope_custom.colorscheme").colorscheme()<Enter>',
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fw",
  ':lua require("telescope_custom.colorscheme").wal()<Enter>',
  {noremap = true, silent = true}
)

-- Dadbod
-- vim.api.nvim_set_keymap("n", "<leader>du", ":DBUIToggle<Enter>", {noremap = true, silent = true})

-- Git
vim.api.nvim_set_keymap("n", "<leader>g", ":Git<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<leader>g', ':lua require("neogit").open({ kind="split" })<Enter>', { noremap = true, silent = true })

-- Trouble
vim.api.nvim_set_keymap("n", "<leader>tt", ":TroubleToggle<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap(
  "n",
  "<leader>tw",
  ":TroubleToggle lsp_workspace_diagnostics<Enter>",
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>td",
  ":TroubleToggle lsp_document_diagnostics<Enter>",
  {noremap = true, silent = true}
)
vim.api.nvim_set_keymap("n", "<leader>tq", ":TroubleToggle quickfix<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>tl", ":TroubleToggle loclist<Enter>", {noremap = true, silent = true})

-- Barbar tabline navigation
vim.api.nvim_set_keymap("n", "<C-o>", ":BufferNext<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-i>", ":BufferPrevious<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-o>", ":BufferMoveNext<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-i>", ":BufferMovePrev<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>c", ":BufferClose<Enter>", {noremap = true, silent = true})

-- Navigation and resizing splits
vim.api.nvim_set_keymap("n", "<C-h>", "<C-w>h", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-j>", "<C-w>j", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-k>", "<C-w>k", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-l>", "<C-w>l", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-h>", ":vertical resize +5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-j>", ":resize +5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-k>", ":resize -5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-l>", ":vertical resize -5<Enter>", {noremap = true, silent = true})

-- Latin long vowels
vim.api.nvim_set_keymap("i", "ä", "ā", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ä", "Ā", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "ë", "ē", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ë", "Ē", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "ï", "ī", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ï", "Ī", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "ö", "ō", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ö", "Ō", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "ü", "ū", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ü", "Ū", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "ÿ", "ȳ", {noremap = true, silent = true})
vim.api.nvim_set_keymap("i", "Ÿ", "Ȳ", {noremap = true, silent = true})
