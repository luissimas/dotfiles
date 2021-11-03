vim.api.nvim_exec(
  [[
  augroup ColorschemeHighlights
    autocmd!
    autocmd ColorScheme * hi VertSplit guifg=bg
    autocmd ColorScheme * hi NonText guifg=bg
  augroup end
  ]],
  false
)

vim.cmd "colorscheme modus-operandi"
