-- Enabling completion
vim.cmd("autocmd FileType sql setlocal omnifunc=vim_dadbod_completion#omni")

-- Disabling execute on save
vim.g.db_ui_execute_on_save = 0

-- Nerd font icons
vim.g.db_ui_show_database_icon = 1
vim.g.db_ui_use_nerd_fonts = 1
