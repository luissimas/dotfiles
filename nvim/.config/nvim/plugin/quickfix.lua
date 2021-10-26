-- Quickfixlist
vim.api.nvim_exec(
  [[
  augroup QuickFixSettings
    autocmd!
    autocmd BufWrite,BufEnter,InsertLeave * lua vim.diagnostic.setloclist({ open = false })
    autocmd FileType qf set nonumber
    autocmd FileType qf set norelativenumber
    autocmd FileType qf set signcolumn="no"
    autocmd FileType qf map <buffer> q :cclose<Enter>
  augroup end
]],
    false
)

vim.api.nvim_set_keymap("n", "<C-q>", ":copen<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<C-n>", ":cnext<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<C-p>", ":cprev<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<leader>l", ":lopen<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<leader>n", ":lnext<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<leader>p", ":lprev<Enter>", { noremap=true, silent=true })

