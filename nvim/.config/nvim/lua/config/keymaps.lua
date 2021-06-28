--   _
--  | | _____ _   _ _ __ ___   __ _ _ __  ___
--  | |/ / _ \ | | | '_ ` _ \ / _` | '_ \/ __|
--  |   <  __/ |_| | | | | | | (_| | |_) \__ \
--  |_|\_\___|\__, |_| |_| |_|\__,_| .__/|___/
--            |___/                |_|
--

vim.g.mapleader = " "

-- Close buffer and keep the split
-- vim.api.nvim_set_keymap("n", "<leader>c", ":bp <bar> vs <bar> bn <bar> bd <Enter>", {noremap = true})

-- Emacs-like M-x
vim.api.nvim_set_keymap(
  "n",
  "<M-x>",
  '<cmd>lua require("telescope.builtin").commands(require("telescope.themes").get_ivy({ previewer = false, results_height=0.5 }))<Enter>',
  {noremap = true, silent = true}
)

-- Close window
-- vim.api.nvim_set_keymap("n", "<leader>q", ":q<Enter>", {noremap = true, silent = true})

-- Write buffer
-- vim.api.nvim_set_keymap("n", "<leader>w", ":w<Enter>", {noremap = true, silent = false})

-- Splits
vim.api.nvim_set_keymap("n", "<C-s>", ":vsplit<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-x>", ":split<Enter>", {noremap = true, silent = true})

-- Enable spellcheck
-- vim.api.nvim_set_keymap("n", "<leader>sk", ":set spell!<Enter>", {noremap = true, silent = true})

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap("n", "<S-l>", "$", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<S-h>", "0", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "<S-l>", "$", {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "<S-h>", "0", {noremap = true, silent = true})

-- Use j and k for visual lines
-- vim.api.nvim_set_keymap("n", "j", "gj", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "k", "gk", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("v", "j", "gj", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("v", "k", "gk", {noremap = true, silent = true})

-- Auto spell corretion
vim.api.nvim_set_keymap("i", "<C-l>", "<Esc>[s1z=`]a", {noremap = true, silent = true})

--Lsp
vim.api.nvim_set_keymap("n", "gd", ":lua vim.lsp.buf.definition()<Enter>", {noremap = true, silent = true})
vim.api.nvim_command("autocmd FileType c,cpp nnoremap <silent> <leader>sh :ClangdSwitchSourceHeader<Enter>")
vim.api.nvim_set_keymap("n", "<C-f>", ":lua vim.lsp.buf.formatting()<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "<leader>rn", ":lua vim.lsp.buf.rename()<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "ca", ":lua vim.lsp.buf.code_action()<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "K", ":lua vim.lsp.buf.hover()<Enter>", {noremap = true, silent = true})

-- Navigation and resizing splits
-- vim.api.nvim_set_keymap("n", "<C-h>", "<C-w>h", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "<C-j>", "<C-w>j", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "<C-k>", "<C-w>k", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "<C-l>", "<C-w>l", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-h>", ":vertical resize +5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-j>", ":resize +5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-k>", ":resize -5<Enter>", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<A-l>", ":vertical resize -5<Enter>", {noremap = true, silent = true})

-- Latin long vowels
-- vim.api.nvim_set_keymap("i", "ä", "ā", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ä", "Ā", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "ë", "ē", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ë", "Ē", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "ï", "ī", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ï", "Ī", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "ö", "ō", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ö", "Ō", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "ü", "ū", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ü", "Ū", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "ÿ", "ȳ", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("i", "Ÿ", "Ȳ", {noremap = true, silent = true})
