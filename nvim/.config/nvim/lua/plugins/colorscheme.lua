-- Colorschemes
return {
  'scottmckendry/cyberdream.nvim',
  priority = 1000, -- Make sure to load this before all the other start plugins.
  opts = {
    variant = 'auto',
    transparent = true,
  },
  init = function()
    vim.cmd.colorscheme 'cyberdream'
  end,
}
