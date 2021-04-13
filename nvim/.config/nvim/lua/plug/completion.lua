


-- Options
vim.o.completeopt="menuone,noinsert,noselect"

-- Matching strategy
vim.g.completion_matching_strategy_list = {'exact', 'substring', 'fuzzy'}

-- Enabling UltiSnips
vim.g.completion_enable_snippet = 'UltiSnips'

-- Enabling lspkind icons
require('lspkind').init()


-- Enabling completion on every markdown buffer
-- This is required for my setup since that at the moment I
-- don't have any lsp server to attach the completion
-- plugin for those files, but I still need the completion
-- to show my snippets
vim.api.nvim_command('autocmd Filetype markdown lua require("completion").on_attach()')
vim.api.nvim_command('autocmd Filetype rmd lua require("completion").on_attach()')
