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

local file = io.open(vim.fn.expand "~/.colorscheme", "r")

if file then
  local theme = file:read()
  vim.cmd("colorscheme " .. theme)
else
  vim.cmd "colorscheme modus-vivendi"
end
