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

require("catppuccin").setup {
  styles = {
    comments = "italic",
    functions = "NONE",
    keywords = "NONE",
    strings = "NONE",
    variables = "NONE",
  },
  integrations = {
    treesitter = true,
    native_lsp = {
      enabled = true,
      virtual_text = {
        errors = "italic",
        hints = "italic",
        warnings = "italic",
        information = "italic",
      },
      underlines = {
        errors = "underline",
        hints = "underline",
        warnings = "underline",
        information = "underline",
      },
    },
    lsp_trouble = false,
    gitsigns = true,
    telescope = true,
    neogit = true,
    markdown = true,
    ts_rainbow = true,
  },
}

local file = io.open(vim.fn.expand "~/.colorscheme", "r")
local lualine_theme = {
  ["base16-material-palenight"] = "palenight",
  ["base16-gruvbox-dark-hard"] = "gruvbox",
  ["rose-pine"] = "rose-pine",
  ["tokyonight"] = "tokyonight",
  ["wally"] = "pywal",
  ["modus-operandi"] = "modus-operandi",
  ["modus-vivendi"] = "modus-vivendi",
  ["nord"] = "nord",
  ["catppuccin"] = "catppuccin",
}

local M = {}

if file then
  local theme = file:read()

  M.lualine_theme = lualine_theme[theme]

  vim.cmd("colorscheme " .. theme)
end

return M
