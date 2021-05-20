--
--  _       _
-- | |_ ___| | ___  ___  ___ ___  _ __   ___
-- | __/ _ \ |/ _ \/ __|/ __/ _ \| '_ \ / _ \
-- | ||  __/ |  __/\__ \ (_| (_) | |_) |  __/
--  \__\___|_|\___||___/\___\___/| .__/ \___|
--                               |_|
--

local actions = require("telescope.actions")
local trouble = require("trouble.providers.telescope")

-- Global customization affecting all pickers
require("telescope").setup {
  defaults = {
    -- Custom mappings (defaults can be found at <telescopedir>/lua/telescope/mappings.lua)
    mappings = {
      i = {
        ["<C-s>"] = actions.file_vsplit,
        -- ["<C-k>"] = actions.preview_scrolling_up,
        -- ["<C-j>"] = actions.preview_scrolling_down,
        ["<C-t>"] = trouble.open_with_trouble
      },
      n = {
        ["<C-s>"] = actions.file_vsplit,
        ["<leader>q"] = actions.close,
        ["<leader>c"] = actions.close,
        ["<C-t>"] = trouble.open_with_trouble
      }
    },
    layout_strategy = "horizontal",
    layout_defaults = {
      horizontal = {
        preview_width = 0.6
      }
    },
    -- Files to be ignored
    file_ignore_patterns = {"node_modules", "%.png", "%.jpg", "%.jpeg", "%.pdf"}
  }
}

-- Loading extensions
require("telescope").load_extension("fzy_native")
