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
      folder = 'Templates',
      date_format = '%Y-%m-%d',
      time_format = '%H:%M',
    },
    daily_notes = {
      folder = 'Journal',
      date_format = '%Y-%m-%d',
    },
    attachments = {
      img_folder = 'Attachments',
    },
    ui = {
      checkboxes = {
        -- Replace the above with this if you don't have a patched font:
        [' '] = { char = '☐', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '✔', hl_group = 'ObsidianDone' },
      },
    },
    disable_frontmatter = true,
    -- Customize how note IDs are generated given an optional title.
    ---@param title string|?
    ---@return string
    note_id_func = function(title)
      if title ~= nil then
        return title
      else
        return tostring(os.time())
      end
    end,
    -- Customize how note file names are generated given the ID, target directory, and title.
    ---@param spec { id: string, dir: obsidian.Path, title: string|? }
    ---@return string|obsidian.Path The full path to the new note.
    note_path_func = function(spec)
      local path = spec.dir / 'Inbox' / tostring(spec.id)
      return path:with_suffix '.md'
    end,
  },
  keys = {
    { '<leader>fn', '<cmd>ObsidianQuickSwitch<CR>', desc = '[F]ind [N]otes' },
    { '<leader>ob', '<cmd>ObsidianBacklinks<CR>', desc = '[O]pen [B]acklinks' },
    { '<leader>oo', '<cmd>ObsidianOpen<CR>', desc = '[O]pen [O]bsidian' },
    { '<leader>op', '<cmd>ObsidianPasteImg<CR>', desc = '[O]bsidian [P]aste Image' },
    { '<leader>or', '<cmd>ObsidianRename<CR>', desc = '[O]bsidian [R]ename Note' },
    { '<leader>on', '<cmd>ObsidianNew<CR>', desc = '[N]ew Note' },
    { '<leader>og', '<cmd>ObsidianSearch<CR>', desc = '[G]rep' },
    { '<leader>ot', '<cmd>ObsidianTemplate<CR>', desc = 'Insert [T]emplate' },
    { '<leader>od', '<cmd>ObsidianDailies<CR>', desc = 'Open [D]ailies' },
    {
      '<CR>',
      function()
        return require('obsidian').util.smart_action()
      end,
      desc = 'Follow Link',
      mode = 'n',
      ft = 'markdown',
    },
  },
}
