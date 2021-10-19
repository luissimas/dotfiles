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
local previewers = require("telescope.previewers")
local putils = require("telescope.previewers.utils")
local pfiletype = require("plenary.filetype")

local new_maker = function(filepath, bufnr, opts)
  opts = opts or {}
  if opts.use_ft_detect == nil then
    local ft = pfiletype.detect(filepath)
    -- Here for example you can say: if ft == "xyz" then this_regex_highlighing else nothing end
    if ft == "elixir" then
      opts.use_ft_detect = false
      putils.regex_highlighter(bufnr, ft)
    end
  end
  previewers.buffer_previewer_maker(filepath, bufnr, opts)
end

-- Global customization affecting all pickers
require("telescope").setup({
  defaults = {
    -- Custom mappings (defaults can be found at <telescopedir>/lua/telescope/mappings.lua)
    mappings = {
      i = {
        ["<C-s>"] = actions.file_vsplit,
        ["<C-x>"] = actions.file_split,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-t>"] = trouble.open_with_trouble,
      },
      n = {
        ["<C-s>"] = actions.file_vsplit,
        ["<C-x>"] = actions.file_split,
        ["<leader>q"] = actions.close,
        ["<leader>c"] = actions.close,
        ["<C-t>"] = trouble.open_with_trouble,
      },
    },
    prompt_prefix = " ",
    selection_caret = "  ",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        preview_width = 0.6,
      },
    },
    -- Files to be ignored
    file_ignore_patterns = { "node_modules", "_build", ".elixir_ls", "%.png", "%.jpg", "%.jpeg", "%.pdf" },
    winblend = 10,
    buffer_previewer_maker = new_maker,
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = true,
      override_file_sorter = true,
    },
    nodescripts = {
      command = "yarn",
      display_method = "vsplit",
      ignore_pre_post = true,
    },
  },
})

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
