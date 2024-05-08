-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

return {
  'nvim-neo-tree/neo-tree.nvim',
  version = '*',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
  },
  cmd = 'Neotree',
  keys = {
    { '<leader>ot', ':Neotree reveal<CR>', { desc = 'Neo[T]ree' } },
  },
  opts = {
    filesystem = {
      window = {
        mappings = {
          ['<leader>ot'] = 'close_window',
        },
      },
    },
  },
}
