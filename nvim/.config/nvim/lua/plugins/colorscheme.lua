-- Colorschemes
return {
  'catppuccin/nvim',
  priority = 1000, -- Make sure to load this before all the other start plugins.
  -- opts = { contrast = 'hard' },
  init = function()
    -- Load the colorscheme here.
    vim.cmd.colorscheme 'catppuccin-latte'

    -- You can configure highlights by doing something like:
    -- vim.cmd.hi 'Comment gui=none'
    -- vim.cmd.hi 'SignColumn guibg=none'
  end,
}
