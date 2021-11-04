require("todo-comments").setup {
  signs = false,
}

vim.api.nvim_set_keymap("n", "<leader>ft", ":TodoTelescope<Enter>", { noremap = true, silent = true })
