local neogit = require "neogit"

neogit.setup {
  disable_commit_confirmation = true,
  disable_hint = true,
  signs = {
    section = { "○", "●" },
    item = { "○", "●" },
    hunk = { "", "" },
  },
}

neogit.config.use_magit_keybindings()

vim.api.nvim_set_keymap("n", "<leader>gs", ":Neogit <Enter>", { noremap = true, silent = true })

vim.cmd [[
  augroup NeogitCommit
    autocmd!
    autocmd FileType NeogitCommitMessage nnoremap <buffer><silent> q :x<Enter>
  augroup end
]]
