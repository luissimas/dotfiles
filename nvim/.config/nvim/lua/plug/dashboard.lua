-- Setting telescope as default fuzzy search method
vim.g.dashboard_default_executive = 'telescope'

vim.g.dashboard_custom_header = {
 '                                                       ',
 '                                                       ',
 '                                                       ',
 '                                                       ',
 '                                                       ',
 '                                                       ',
 '                                                       ',
 ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
 ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
 ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
 ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
 ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
 ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
}

vim.g.dashboard_custom_shortcut={
 last_session       = 'SPC aaa',
 find_history       = 'SPC f h',
 find_file          = 'SPC f f',
 new_file           = 'SPC c n',
 change_colorscheme = 'SPC t c',
 find_word          = 'SPC f a',
 book_marks         = 'SPC f b',
}

vim.g.dashboard_custom_section = {
    a = {description = {'  Settings                           SPC f d'}, command = ':lua require("plug.dashboard").edit_settings()'},
    b = {description = {'  Find File                          SPC f f'}, command = 'Telescope find_files'},
    c = {description = {'  Recently Opened Files              SPC f o'}, command = 'Telescope oldfiles'}
}

local M = {}

function M.edit_settings()
  vim.cmd('cd ~/.config/nvim')
  vim.cmd('e init.lua')
end

return M
