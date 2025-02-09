-- Colorschemes
return {
  'rebelot/kanagawa.nvim',
  lazy = false,
  priority = 1000, -- Make sure to load this before all the other start plugins.
  opts = {
    background = {
      dark = 'dragon',
      light = 'lotus',
    },
    colors = {
      theme = {
        all = {
          ui = {
            bg_gutter = 'none',
          },
        },
      },
    },
  },
  init = function()
    -- Load the colorscheme
    vim.cmd.colorscheme 'kanagawa'
  end,
}
