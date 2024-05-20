return {
  'epwalsh/obsidian.nvim',
  version = '*', -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = 'markdown',
  dependencies = {
    -- Required.
    'nvim-lua/plenary.nvim',
    'hrsh7th/nvim-cmp',
    'nvim-telescope/telescope.nvim',
    'nvim-treesitter/nvim-treesitter',
  },
  opts = {
    workspaces = {
      {
        name = 'personal',
        path = '~/Documents/vault/',
      },
    },
    templates = {
      folder = '5 - Templates',
      date_format = '%Y-%m-%d',
      time_format = '%H:%M',
    },
    attachments = {
      img_folder = '4 - Attachments',
    },
    disable_frontmatter = true,
  },
}
