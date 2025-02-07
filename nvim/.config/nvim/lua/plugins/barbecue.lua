return {
  'utilyre/barbecue.nvim',
  name = 'barbecue',
  version = '*',
  dependencies = {
    'SmiteshP/nvim-navic',
    'nvim-tree/nvim-web-devicons', -- optional dependency
  },
  config = function()
    require('barbecue').setup {
      show_dirname = true,
      show_basename = true,
    }
    vim.keymap.set('n', '<leader>tb', require('barbecue.ui').toggle, { desc = '[T]oggle [B]readcrumbs' })
  end,
}
