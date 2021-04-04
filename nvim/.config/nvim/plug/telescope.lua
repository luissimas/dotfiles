--
--  _       _
-- | |_ ___| | ___  ___  ___ ___  _ __   ___
-- | __/ _ \ |/ _ \/ __|/ __/ _ \| '_ \ / _ \
-- | ||  __/ |  __/\__ \ (_| (_) | |_) |  __/
--  \__\___|_|\___||___/\___\___/| .__/ \___|
--                               |_|
--

local actions = require('telescope.actions')

-- Global customization affecting all pickers
require('telescope').setup{
  defaults = {
    -- Custom mappings (defaults can be found at <telescopedir>/lua/telescope/mappings.lua)
    mappings = {
      i = {
        ['<C-s>'] = actions.file_vsplit,

        ["<C-k>"] = actions.preview_scrolling_up,
        ["<C-j>"] = actions.preview_scrolling_down,
      },

      n = {
        ['<C-s>'] = actions.file_vsplit,

        ['<leader>q'] = actions.close,
        ['<leader>c'] = actions.close
      }
    },

    -- Files to be ignored
    file_ignore_patterns = { '%.png', '%.jpg', '%.jpeg', '%.pdf' }
  }
}
