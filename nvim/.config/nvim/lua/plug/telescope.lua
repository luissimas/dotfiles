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
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-t>"] = trouble.open_with_trouble
      },
      n = {
        ["<C-s>"] = actions.file_vsplit,
        ["<leader>q"] = actions.close,
        ["<leader>c"] = actions.close,
        ["<C-t>"] = trouble.open_with_trouble
      }
    },
    prompt_prefix = " ",
    selection_caret = "  ",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        preview_width = 0.6
      }
    },
    -- Files to be ignored
    file_ignore_patterns = {"node_modules", "_build", ".elixir_ls", "%.png", "%.jpg", "%.jpeg", "%.pdf"},
    winblend = 20
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = true,
      override_file_sorter = true
    },
    project = {
      base_dirs = {
        {path = "~/fun", max_depth = 4},
        {path = "~/cati", max_depth = 5},
        {path = "~/exercism", max_depth = 5}
      }
    }
  }
}

-- Loading extensions
require("telescope").load_extension("fzy_native")

-- Keymaps (currently it's all mapped by whichkey)
-- vim.api.nvim_set_keymap("n", "<leader>ff", ":Telescope find_files<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "<leader>fo", ":Telescope oldfiles<Enter>", {noremap = true, silent = true})

-- vim.api.nvim_set_keymap(
--   "n",
--   "<leader>fd",
--   ':lua require("telescope_custom.find").find_dotfiles()<Enter>',
--   {noremap = true, silent = true}
-- )
-- vim.api.nvim_set_keymap(
--   "n",
--   "<leader>fh",
--   ':lua require("telescope_custom.find").find_home()<Enter>',
--   {noremap = true, silent = true}
-- )
-- vim.api.nvim_set_keymap(
--   "n",
--   "<leader>fv",
--   ':lua require("telescope_custom.find").find_vault()<Enter>',
--   {noremap = true, silent = true}
-- )
-- vim.api.nvim_set_keymap(
--   "n",
--   "<leader>fc",
--   ':lua require("telescope_custom.colorscheme").colorscheme()<Enter>',
--   {noremap = true, silent = true}
-- )
-- vim.api.nvim_set_keymap(
--   "n",
--   "<leader>fw",
--   ':lua require("telescope_custom.colorscheme").wal()<Enter>',
--   {noremap = true, silent = true}
-- )
