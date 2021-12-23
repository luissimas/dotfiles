require("gitsigns").setup {
  signcolumn = true,
  current_line_blame = true,
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = "eol",
    delay = 200,
    ignore_whitespace = false,
  },
  current_line_blame_formatter_opts = {
    relative_time = true,
  },
  keymaps = {
    ["n <leader>gg"] = '<cmd>lua require("gitsigns").blame_line(true)<CR>',
  },
}
