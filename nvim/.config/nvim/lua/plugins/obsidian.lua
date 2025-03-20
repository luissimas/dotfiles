local zettelkasten_root = '~/projects/zettelkasten/'

return {
  'epwalsh/obsidian.nvim',
  version = '*', -- recommended, use latest release instead of latest commit
  lazy = true,
  ft = 'markdown',
  dependencies = {
    -- Required.
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
    'nvim-treesitter/nvim-treesitter',
  },
  opts = {
    workspaces = {
      {
        name = 'personal',
        path = zettelkasten_root,
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
      -- A function that determines the text to insert in the note when pasting an image.
      ---@param client obsidian.Client
      ---@param path obsidian.Path the absolute path to the image file
      ---@return string
      img_text_func = function(client, path)
        path = client:vault_relative_path(path) or path
        return string.format('![[%s]]', path)
      end,
    },
    ui = {
      enable = false,
      checkboxes = {
        -- Replace the above with this if you don't have a patched font:
        [' '] = { char = '☐', hl_group = 'ObsidianTodo' },
        ['x'] = { char = '✔', hl_group = 'ObsidianDone' },
      },
    },
    disable_frontmatter = false,
    -- Optional, alternatively you can customize the frontmatter data.
    ---@param note obsidian.Note
    ---@return table
    note_frontmatter_func = function(note)
      local out = { ['created-at'] = os.date '%Y-%m-%d' }

      -- `note.metadata` contains any manually added fields in the frontmatter.
      -- So here we just make sure those fields are kept in the frontmatter.
      if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
        for k, v in pairs(note.metadata) do
          out[k] = v
        end
      end

      return out
    end,

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
      local path = require 'obsidian.path'
      local note_path = path.new(zettelkasten_root) / 'Inbox' / tostring(spec.id)
      return note_path:with_suffix '.md'
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
