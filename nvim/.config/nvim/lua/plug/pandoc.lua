-- Define filetypes
vim.api.nvim_command('let g:pandoc#filetypes#handled = ["pandoc", "markdown"]')
vim.api.nvim_command('let g:pandoc#command#prefer_pdf = 1')

-- Disable some modules that I don't use
vim.api.nvim_command('let g:pandoc#modules#disabled = ["folding", "templates", "spell"]')
vim.api.nvim_command('let g:pandoc#spell#enabled = 0')

-- Autocompile on save
-- vim.cmd('let g:pandoc#command#autoexec_on_writes = 1')
-- vim.cmd('let g:pandoc#command#autoexec_command = "Pandoc pdf"')

