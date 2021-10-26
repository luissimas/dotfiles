-- Netrw
vim.g.netrw_banner = 0
vim.g.netrw_winsize = -25
vim.g.netrw_browse_split = 4
vim.g.netrw_hide = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_list_hide = [[node_modules,\.git]]

vim.api.nvim_exec(
  [[
  augroup NetrwSettings
    autocmd!
    autocmd FileType netrw setlocal bufhidden=wipe
    autocmd FileType netrw setlocal statusline=%=
  augroup end
]],
  false
)

vim.api.nvim_set_keymap("n", "<leader>e", ":Lexplore<Enter>", { noremap=true, silent=true })
