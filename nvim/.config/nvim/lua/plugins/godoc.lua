return {
  'fredrikaverpil/godoc.nvim',
  version = '*',
  dependencies = {
    { 'nvim-telescope/telescope.nvim' },
    {
      'nvim-treesitter/nvim-treesitter',
      opts = {
        ensure_installed = { 'go' },
      },
    },
  },
  cmd = { 'GoDoc' },
  opts = {
    window = {
      type = 'vsplit',
    },
    picker = {
      type = 'telescope',
    },
  },
}
