require("gitsigns").setup {
  keymaps = {
    ["n <leader>gg"] = '<cmd>lua require("gitsigns").blame_line(true)<CR>',
  },
}
