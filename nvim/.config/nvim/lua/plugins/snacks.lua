return {
  'folke/snacks.nvim',
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = true },
    gitbrowse = { notify = false },
    notifier = { enabled = false },
    quickfile = { enabled = true },
    scratch = { name = 'Scratch', ft = 'markdown' },
    statuscolumn = { enabled = true },
    words = { enabled = true },
  },
  keys = {
    {
      '<leader>bk',
      function()
        Snacks.bufdelete.delete()
      end,
      mode = 'n',
      desc = '[K]ill buffer',
    },
    {
      '<leader>bK',
      function()
        Snacks.bufdelete.other()
      end,
      mode = 'n',
      desc = '[K]ill other buffers',
    },

    {
      '<leader>go',
      function()
        Snacks.gitbrowse.open()
      end,
      mode = 'n',
      desc = '[O]pen repository',
    },
    {
      '<leader>s',
      function()
        Snacks.scratch()
      end,
      mode = 'n',
      desc = 'Toggle Scratch Buffer',
    },
    {
      '<leader>S',
      function()
        Snacks.scratch.select()
      end,
      mode = 'n',
      desc = 'Select Scratch Buffer',
    },
    {
      '<leader>gb',
      function()
        Snacks.git.blame_line()
      end,
      mode = 'n',
      desc = '[B]lame line',
    },
  },
}
