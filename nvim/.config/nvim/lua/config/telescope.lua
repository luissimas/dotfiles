require("telescope").setup {
  defaults = {
    prompt_prefix = " ",
    selection_caret = " ",
    entry_prefix = " ",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
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

require("telescope").load_extension "projects"

vim.api.nvim_set_keymap("n", "<leader>ff", ":Telescope find_files<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fg", ":Telescope live_grep<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fw", ":Telescope grep_string<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bb", ":Telescope buffers<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fh", ":Telescope help_tags<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fm", ":Telescope man_pages<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fp", ":Telescope projects<Enter>", { noremap = true, silent = true })
