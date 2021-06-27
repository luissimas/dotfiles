require("Navigator").setup(
  {
    auto_save = "none",
    disable_on_zoom = false
  }
)

-- Keymaps
vim.api.nvim_set_keymap("n", "<C-h>", ":lua require('Navigator').left()<Enter>)", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-j>", ":lua require('Navigator').down()<Enter>)", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-k>", ":lua require('Navigator').up()<Enter>)", {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<C-l>", ":lua require('Navigator').right()<Enter>)", {noremap = true, silent = true})
