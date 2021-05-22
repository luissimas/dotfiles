-- Open the edit buffer in a specific split
vim.g.UltiSnipsEditSplit = "normal"

-- Setting snippet folder
vim.g.UltiSnipsSnippetDirectories = {"ultisnips"}

-- Triggers
vim.g.UltiSnipsExpandTrigger = "<C-e>"
vim.g.UltiSnipsJumpForwardTrigger = "<Tab>"
vim.g.UltiSnipsJumpBackwardTrigger = "<S-tab>"

-- Edit snippets keymap
vim.api.nvim_set_keymap("n", "<leader>se", ":UltiSnipsEdit<Enter>", {noremap = true, silent = true})
