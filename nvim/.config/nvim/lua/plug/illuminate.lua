-- Don't highlight word under cursor (default: 1)
vim.g.Illuminate_highlightUnderCursor = 1

-- Delay time in milliseconds (default 0)
vim.g.Illuminate_delay = 500

-- Disable for this filetypes
vim.g.Illuminate_ftblacklist = {
  "help",
  "terminal",
  "toggleterm",
  "git",
  "fugitive",
  "dashboard",
  "NeogitStatus",
  "packer",
  "NvimTree",
  ""
}
