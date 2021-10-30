require("gitsigns").setup {
  keymaps = {
    ["n <leader>gb"] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',
  },
}
