vim.g.vsnip_snippet_dir = vim.fn.expand("~/.config/nvim/vsnips")

-- Edit snippets keymap
vim.api.nvim_set_keymap("n", "<leader>se", ":VsnipOpen<Enter>", {noremap = true, silent = true})
