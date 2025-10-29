-- Colorschemes
return {
  'metalelf0/black-metal-theme-neovim',
  priority = 1000, -- Make sure to load this before all the other start plugins.
  config = function()
    require('black-metal').setup {
      transparent = true,
    }
    require('black-metal').load()
  end,
}
