local telescope = require "telescope"
local previewers = require "telescope.previewers"
local preview_utils = require "telescope.previewers.utils"
local filetype = require "plenary.filetype"

local custom_preview = function(filepath, bufnr, opts)
  opts = opts or {}

  if opts.use_ft_detect == nil then
    local ft = filetype.detect(filepath)

    if ft == "elixir" then
      opts.use_ft_detect = false
      preview_utils.regex_highlighter(bufnr, ft)
    end
  end

  previewers.buffer_previewer_maker(filepath, bufnr, opts)
end

telescope.setup {
  defaults = {
    prompt_prefix = " ",
    selection_caret = " ",
    entry_prefix = " ",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    buffer_previewer_maker = custom_preview,
    layout_config = {
      horizontal = {
        preview_width = 0.6,
      },
    },
    mappings = {
      i = {
        ["<C-s>"] = "file_vsplit",
        ["<C-x>"] = "file_split",
        ["<C-k>"] = "move_selection_previous",
        ["<C-j>"] = "move_selection_next",
      },
      n = {
        ["<C-s>"] = "file_vsplit",
        ["<C-x>"] = "file_split",
        ["<leader>q"] = "close",
      },
    },
    file_ignore_patterns = { "node_modules", "_build", ".elixir_ls", "%.png", "%.jpg", "%.jpeg", "%.pdf" },
  },
}

telescope.load_extension "fzf"
telescope.load_extension "projects"

local function builtin(picker)
  return string.format(":lua require('telescope.builtin').%s() <Enter>", picker)
end

local function config(picker)
  return string.format(":lua require('config.telescope').%s() <Enter>", picker)
end

vim.api.nvim_set_keymap("n", "<leader>ff", config "find_files", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fg", builtin "live_grep", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fw", builtin "grep_string", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fb", builtin "current_buffer_fuzzy_find", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fh", builtin "help_tags", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fm", builtin "man_pages", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bb", builtin "buffers", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fp", ":Telescope projects<Enter>", { noremap = true, silent = true })

-- Custom pickers
local M = {}

M.find_files = function()
  local ok = pcall(require("telescope.builtin").git_files)

  if not ok then
    require("telescope.builtin").find_files()
  end
end

return M
