vim.g.slime_target = "tmux"
vim.g.slime_paste_file = "/tmp/.slime_paste"

vim.api.nvim_set_keymap("n", "<leader><leader>", "<Plug>SlimeRegionSend", { noremap = false, silent = true })
