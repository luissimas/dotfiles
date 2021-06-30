--
--  _
-- | |_ _ __ ___  ___
-- | __| '__/ _ \/ _ \
-- | |_| | |  __/  __/
--  \__|_|  \___|\___|
--

-- Tree side
vim.g.nvim_tree_side = "right"

-- Tree width
vim.g.nvim_tree_width = 25

-- Update cursor when entering a buffer
vim.g.nvim_tree_follow = 1

-- Visual indent markers
vim.g.nvim_tree_indent_markers = 1

-- Files to be ignored
vim.g.nvim_tree_ignore = {".git", "node_modules", ".cache"}

-- Automatically close tree buffer when a file is opened
vim.g.nvim_tree_quit_on_open = 0

-- Automatically close tree buffer when it's the last buffer
vim.g.nvim_tree_auto_close = 1

-- Hide folders and files starting with dot
vim.g.nvim_tree_hide_dotfiles = 0

-- Icons to show
vim.g.nvim_tree_show_icons = {git = 0, folders = 1, files = 1}

-- Highlight for git attributes
vim.g.nvim_tree_git_hl = 1

-- Keeping netrw (it's useful for some other stuff)
vim.g.nvim_tree_disable_netrw = 0 -- 1 by default, disables netrw

-- Prevents netrw from automatically opening on folders
vim.g.nvim_tree_hijack_netrw = 1

-- Visual trailing lines on folders
vim.g.nvim_tree_add_trailing = 0

--
vim.g.nvim_tree_root_folder_modifier = ":t"

-- Lsp diagnostics in the tree
vim.g.nvim_tree_lsp_diagnostics = 0

-- Files that get an special highlight
vim.g.nvim_tree_special_files = {"README.md", "Makefile", "MAKEFILE", "makefile"}

-- Icons
vim.g.nvim_tree_icons = {
  default = "",
  symlink = "",
  git = {
    unstaged = "✗",
    staged = "✓",
    unmerged = "",
    renamed = "➜",
    untracked = "★",
    deleted = "",
    ignored = "◌"
  },
  folder = {
    default = "",
    open = "",
    empty = "",
    empty_open = "",
    symlink = "",
    symlink_open = ""
  },
  lsp = {
    hint = "",
    info = "",
    warning = "",
    error = ""
  }
}

-- Keymappings
local tree_cb = require "nvim-tree.config".nvim_tree_callback
-- default mappings
vim.g.nvim_tree_bindings = {
  {key = {"<CR>", "o", "<2-LeftMouse>", "l"}, cb = tree_cb("edit")},
  {key = {"<2-RightMouse>", "<C-}>"}, cb = tree_cb("cd")},
  {key = "<C-s>", cb = tree_cb("vsplit")},
  {key = "<C-x>", cb = tree_cb("split")},
  {key = "<C-t>", cb = tree_cb("tabnew")},
  {key = "<", cb = tree_cb("prev_sibling")},
  {key = ">", cb = tree_cb("next_sibling")},
  {key = "P", cb = tree_cb("parent_node")},
  {key = {"<BS>", "h"}, cb = tree_cb("close_node")},
  {key = "<S-CR>", cb = tree_cb("close_node")},
  {key = "<Tab>", cb = tree_cb("preview")},
  {key = "K", cb = tree_cb("first_sibling")},
  {key = "J", cb = tree_cb("last_sibling")},
  {key = "I", cb = tree_cb("toggle_ignored")},
  {key = "H", cb = tree_cb("toggle_dotfiles")},
  {key = "R", cb = tree_cb("refresh")},
  {key = "a", cb = tree_cb("create")},
  {key = "d", cb = tree_cb("remove")},
  {key = "r", cb = tree_cb("rename")},
  {key = "<C->", cb = tree_cb("full_rename")},
  {key = "x", cb = tree_cb("cut")},
  {key = "c", cb = tree_cb("copy")},
  {key = "p", cb = tree_cb("paste")},
  {key = "y", cb = tree_cb("copy_name")},
  {key = "Y", cb = tree_cb("copy_path")},
  {key = "gy", cb = tree_cb("copy_absolute_path")},
  {key = "[c", cb = tree_cb("prev_git_item")},
  {key = "}c", cb = tree_cb("next_git_item")},
  {key = "-", cb = tree_cb("dir_up")},
  {key = "q", cb = tree_cb("close")},
  {key = "g?", cb = tree_cb("toggle_help")}
}

-- Toggle file tree
vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<Enter>", {noremap = true, silent = true})
