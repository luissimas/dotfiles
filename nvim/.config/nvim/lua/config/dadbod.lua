vim.g.db_ui_show_help = 0
vim.g.db_ui_disable_mappings = 0
vim.g.db_ui_auto_execute_table_helpers = 1
vim.g.db_ui_show_database_icon = 1
vim.g.db_ui_use_nerd_fonts = 1

vim.api.nvim_set_keymap("n", "<leader>db", ":DBUIToggle<Enter>", { noremap = true, silent = true })

vim.cmd [[
  augroup DBUIMappings
    autocmd!
    autocmd FileType dbui nmap <silent><buffer> <C-s> <Plug>(DBUI_SelectLineVsplit)
    autocmd FileType dbui unmap <buffer> <C-j>
    autocmd FileType dbui unmap <buffer> <C-k>
    autocmd FileType sql nmap <silent><buffer> <leader><leader> <Plug>(DBUI_ExecuteQuery)
  augroup end
]]
