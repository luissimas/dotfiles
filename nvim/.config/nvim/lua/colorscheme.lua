vim.api.nvim_exec(
  [[
  augroup ColorschemeHighlights
    autocmd!
    autocmd ColorScheme * hi VertSplit guifg=bg
    autocmd ColorScheme * hi EndOfBuffer guifg=bg
  augroup end
  ]],
  false
)

vim.g.tokyonight_style = "night"
vim.g.tokyonight_lualine_bold = true

local file = io.open(vim.fn.expand "~/.colorscheme", "r")
local lualine_theme = {
  ["base16-material-palenight"] = "palenight",
  ["base16-gruvbox-dark-hard"] = "gruvbox",
  ["rose-pine"] = "rose-pine",
  ["tokyonight"] = "tokyonight",
  ["wally"] = "pywal",
}

local M = {}

if file then
  local theme = file:read()

  M.lualine_theme = lualine_theme[theme]

  vim.cmd("colorscheme " .. theme)
end

return M
